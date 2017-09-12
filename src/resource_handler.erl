-module(resource_handler).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([terminate/4]).
-export([get/3, put/3, delete/3, post/3]).
-export([cross_domains/3]).

%%for keeping state
%%The application record is defined in application.hrl
%%In Mnesia, the first element is the type of record and the second element is the key
%%http://erlang.org/doc/apps/mnesia/Mnesia_chap2.html
-include("application.hrl").

-define(CONSURL, dict:fetch("consulurl", State)).
-define(CDAPURL, dict:fetch("cdapurl", State)).
%below come from config map
-define(HCInterval, maps:get(<<"hcinterval">>, dict:fetch("configmap", State))).
-define(AutoDeregisterAfter, maps:get(<<"autoderegisterafter">>, dict:fetch("configmap", State))).
-define(PipelineHealthLimit, maps:get(<<"pipelinehealthlimit">>, dict:fetch("configmap", State))).
-define(PUBLICFIELDS, [<<"appname">>, <<"apptype">>, <<"namespace">>, <<"healthcheckurl">>, <<"metricsurl">>, <<"url">>, <<"connectionurl">>, <<"serviceendpoints">>]).

%super lazy macros/imports...
-import(logging, [audit/3, metrics/3, err/2]).
-import(util, [iso/0, to_str/1]).
%lazy concat
-define(SC(L), util:concat(L)).
%lazy shorthand to write info audit records. man I miss defines in python. c ftw.
-define(AUDI(Req, Bts, XER, Rcode), audit(info, Req, [{bts, Bts}, {xer,XER}, {rcode, RCode}, {mod, mod()}])).

%need this for the ?MODULE:FUNCTION to work with meck unit tests
-export([delete_app_helper/4, appname_to_field_vals/2]).

%%%
%%Helper functions
%%%
mod() -> to_str(?MODULE).

get_request_id(Req) ->
    %ECOMP request tracing
    %see if we got a X-ECOMP-REQUESTID, or generate a new one if not
    HXER = leptus_req:header(Req, <<"x-ecomp-requestid">>),
    case HXER of
        undefined ->
                      XER = util:gen_uuid(),
                      %LOL, use the client ip here to shame them into their request id
                      audit(warning, Req, [{bts, iso()}, {xer, XER}, {mod, mod()}, {msg, "Request is missing requestID. Assigned this one."}]), %eelf documentation says to log this message if requestid was missing
                      XER;
        _         ->
                      binary_to_list(HXER) %httpc expects strings as headers, so this needs to be str for subsequent passing
    end.

%shared-code function initlization that creates a begining timestamp and gets or generates a request ID
init_api_call(Req) ->
    Bts = iso(), %record begining timestamp
    XER = get_request_id(Req), %get or generate XER
    {Bts, XER}.

lookup_application(Appname) ->
    %do a lookup in mnesia of an appname
    Ret = mnesia:transaction(fun() -> mnesia:match_object(application, {application, Appname, '_', '_', '_', '_', '_', '_', '_', '_'}, read) end),
    case Ret of
        {atomic, []} -> none; %no matches
        {atomic, [Rec]} -> Rec
        %fail hard if there was more than one result
    end.

appname_to_application_map(Appname) ->
    %return a Map of an Mnesia record
    Rec = lookup_application(Appname),
    case Rec of
        none -> none;
        {application, Appname, AppType, Namespace, Healthcheckurl, Metricsurl, Url, Connectionurl, ServiceEndpoints, CreationTime} ->
              #{<<"appname">> => Appname,
                <<"apptype">>  => AppType,
                <<"namespace">> => Namespace,
                <<"healthcheckurl">> => Healthcheckurl,
                <<"metricsurl">> => Metricsurl,
                <<"url">> => Url,
                <<"connectionurl">> => Connectionurl,
                <<"serviceendpoints">> => ServiceEndpoints,
                <<"creationtime">> => CreationTime
                }
    end.

appname_to_field_vals(Appname, FieldList) ->
    %Return just a list of values of an application with fields FieldList
    M = appname_to_application_map(Appname),
    case M of
        none -> none;
        _ -> [maps:get(F, M) || F <- FieldList]
    end.

appname_to_application_http(XER, Appname, State) ->
    %Return an HTTP response of an application record. If this is a program flowlet style app, additionally return it's bound and unbound config
    A = appname_to_application_map(Appname),
    case A of
        none -> {404, "", State};
        _ ->
            Body = maps:with(?PUBLICFIELDS, A),
            case maps:get(<<"apptype">>, Body) of
                 %if program-flowlet style app, append the bound and unbound config into the return JSON
                 <<"program-flowlet">> ->
                    UB = case consul_interface:consul_get_configuration(XER, Appname, ?CONSURL) of
                            {200, Unbound} -> Unbound;
                            {_, _} -> <<"WARNING: COULD NOT FETCH CONFIG FROM CONSUL">>
                        end,
                    B = case cdap_interface:get_app_config(XER, Appname, maps:get(<<"namespace">>, Body), ?CDAPURL) of
                            {200, Bound} -> Bound;
                            {_, _} -> <<"WARNING: COULD NOT FETCH CONFIG FROM CDAP">>
                       end,
                    CM = #{<<"unbound_config">> => UB,
                           <<"bound_config">>   => B},
                    {200, {json, maps:merge(Body, CM)}, State};
                %TODO! can we do something for hydrator apps?
                 <<"hydrator-pipeline">> ->
                    {200, {json, Body}, State}
            end
    end.

delete_app_helper(Appname, State, XER, Req) ->
    %Helper because it is used by both delete and rollback on failed deploy
    %
    %%Internal Crisis Alert:
    %
    %I pondered this for some time. There are three points of state for this: the cdap cluster, consul, and the broker's internal database
    %The question is, if something in the delete fails, do we:
    %1) Tell the user to try again later
    %2) Clean up as much as we can, log the error, and keep going
    %
    %I have decided for now on taking number 2). This is the "Cloudify" way of doing things where you don't raise a NonRecoerable in a Delete operation.
    %This has the benefit that this delete operation can be used as the *rollback*, so if anything fails in the deploy, this delete function is called to clean up any dirty state.
    %
    %Number 1 is not so straitforward, because "putting back things the way they were" is difficult. For example, the deletion from CDAP succeeds, but Consul can't be reached.
    %What happens? Do I *redeploy* the CDAP app to try to make their state as it was before the botched delete was called?
    %
    %My conclusion is that transactions across distributed systems is hard. It's much easier if it is all local (e.g., Transactions in a single Postgres DB)
    %
    %SO, as a result of this decision, the broker does *NOT* assert the status code of any delete operations to be 200.
    %The only way this function does not return a 200 is if I can't even delete from my own database.
    %
    metrics(info, Req, [{bts, iso()}, {xer, XER}, {mod, mod()}, {msg, io_lib:format("Delete recieved for ~s", [Appname])}]),
    case appname_to_field_vals(Appname, [<<"apptype">>, <<"namespace">>]) of
        none -> {404, "Tried to delete an application that was not registered", State};
        [AppType, Namespace] ->
            try
                case AppType of
                    <<"program-flowlet">>   ->
                        ok = workflows:undeploy_cdap_app(Req, XER, Appname, ?CDAPURL, ?CONSURL, Namespace),
                        %delete from the program-flowlet supplementary table
                        {atomic, ok} = mnesia:transaction(fun() -> mnesia:delete(prog_flow_supp, Appname, write) end);
                    <<"hydrator-pipeline">> -> ok = workflows:undeploy_hydrator_pipeline(Req, XER, Appname, Namespace, ?CDAPURL, ?CONSURL)
                end,
                %delete from application table (shared between both types of apps)
                {atomic, ok} = mnesia:transaction(fun() -> mnesia:delete(application, Appname, write) end),
                {200, "", State} %Return
            catch
                %this is really bad, means I can't even delete from my own database. For now, log and pray.
                %generic failure catch-all, catastrophic
                Class:Reason ->
                    err(emergency, [{xer, XER}, {msg, io_lib:format("Catastrophic failure, can't delete ~s from my database. ~s:~s", [Appname, Class, Reason])}]),
                    err(error, [{xer, XER}, {msg, io_lib:format("~nError Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})])}]),
                    {500, "Please report this error", State}
            end
    end.

parse_put_body(B) ->
    %parse the PUT body to application
    try
        Body = jiffy:decode(B, [return_maps]),
        Type = maps:get(<<"cdap_application_type">>, Body),
        case Type ==  <<"hydrator-pipeline">> orelse Type == <<"program-flowlet">> of
            false -> unsupported;
            true ->
                %common to both
                Namespace = maps:get(<<"namespace">>, Body),
                Streamname = maps:get(<<"streamname">>, Body),
                case Type of
                    <<"program-flowlet">> ->
                        JarURL = maps:get(<<"jar_url">>, Body),
                        ArtifactName = maps:get(<<"artifact_name">>, Body),
                        ArtifactVersion = maps:get(<<"artifact_version">>, Body),
                        AppConfig = maps:get(<<"app_config">>, Body),
                        AppPreferences = maps:get(<<"app_preferences">>, Body),
                        ParsedServices = lists:map(fun(S) -> {maps:get(<<"service_name">>, S),
                                                              maps:get(<<"service_endpoint">>, S),
                                                              maps:get(<<"endpoint_method">>, S)}
                                                   end, maps:get(<<"services">>, Body)),
                        Programs = lists:map(fun(P) -> #program{type=maps:get(<<"program_type">>, P),
                                                                id= maps:get(<<"program_id">>, P)}
                                                   end, maps:get(<<"programs">>, Body)),
                        ParsedProgramPreferences = lists:map(fun(P) -> {maps:get(<<"program_type">>, P),
                                                                        maps:get(<<"program_id">>, P),
                                                                        maps:get(<<"program_pref">>, P)}
                                                             end, maps:get(<<"program_preferences">>, Body)),
                       {<<"program-flowlet">>, {Namespace, Streamname, JarURL, ArtifactName, ArtifactVersion, AppConfig, AppPreferences, ParsedServices, Programs, ParsedProgramPreferences}};
                    <<"hydrator-pipeline">> ->
                        PipelineConfigJsonURL = maps:get(<<"pipeline_config_json_url">>, Body),

                        %Dependencies is optional. This function will normalize it's return with [] if the dependencies key was not passed in.
                        ParsedDependencies = case maps:is_key(<<"dependencies">>, Body) of
                            true ->
                                D = maps:get(<<"dependencies">>, Body),
                                %crash and let caller deal with it if not a list or if required keys are missing. Else parse it into
                                %     {artifact-extends-header, artifact_name, artifact-version-header, artifact_url}
                                %regarding the binart_to_lists: these all come in as binaries but they need to be "strings" (which are just lists of integers in erlang)
                                %for headers requiring strings, see http://stackoverflow.com/questions/28292576/setting-headers-in-a-httpc-post-request-in-erlang
                                lists:map(fun(X) -> {binary_to_list(maps:get(<<"artifact_extends_header">>, X)),
                                                     maps:get(<<"artifact_name">>, X),
                                                     binary_to_list(maps:get(<<"artifact_version_header">>, X)),
                                                     maps:get(<<"artifact_url">>, X),
                                                     %even if dependencies is specified, ui_properties is optional. This will normalize it's return with 'none' if  not passed in
                                                     case maps:is_key(<<"ui_properties_url">>, X) of true -> maps:get(<<"ui_properties_url">>, X); false -> none end
                                                    } end, D);
                            false -> [] %normalize optional user input into []; just prevents user from having to explicitly pass in []
                        end,
                        {<<"hydrator-pipeline">>, {Namespace, Streamname, PipelineConfigJsonURL, ParsedDependencies}}
                end
            end
    catch _:_ ->  invalid
    end.

parse_reconfiguration_put_body(Body) ->
    try
        D = jiffy:decode(Body, [return_maps]),
        ReconfigurationType = maps:get(<<"reconfiguration_type">>, D),
        Config = maps:get(<<"config">>, D),
        case ReconfigurationType == <<"program-flowlet-app-config">> orelse
             ReconfigurationType == <<"program-flowlet-app-preferences">> orelse
             ReconfigurationType == <<"program-flowlet-smart">> of
             false -> notimplemented;
             true -> {ReconfigurationType, Config}
        end
    catch _:_ ->  invalid
    end.

handle_reconfigure_put(Req, State, XER, Appname, ReqBody) ->
    %handle the reconfiguration put. broker out from the http call, and takes the lookup func as an arg, to allow for better unit testing.
    %this is still not a pure function due to the workflows call, still needs enhancement
    case ?MODULE:appname_to_field_vals(Appname, [<<"namespace">>]) of
        none ->  {404, "Reconfigure recieved but the app is not registered", State};
        [Namespace] ->
            ParsedBody = parse_reconfiguration_put_body(ReqBody),
            case ParsedBody of
                invalid ->  {400, "Invalid PUT Reconfigure Body", State};
                notimplemented -> {501, "This type of reconfiguration is not implemented", State};
                {ReconfigurationType, Config} ->
                    try
                        ok = case ReconfigurationType of
                            <<"program-flowlet-app-config">> ->
                                %reconfigure a program-flowlet style app's app config
                                workflows:app_config_reconfigure(Req, XER, Appname, Namespace, ?CONSURL, ?CDAPURL, Config);
                            <<"program-flowlet-app-preferences">> ->
                                %reconfigure a program-flowlet style app's app config
                                workflows:app_preferences_reconfigure(Req, XER, Appname, Namespace, ?CONSURL, ?CDAPURL, Config);
                           <<"program-flowlet-smart">> ->
                                %try to "figure out" whether the supplied JSON contains keys in appconfig, app preferences, or both
                                workflows:smart_reconfigure(Req, XER, Appname, Namespace, ?CONSURL, ?CDAPURL, Config)
                        end,
                        {200, "", State}
                    catch
                        %catch a bad HTTP error code; also catches the non-overlapping configuration case
                        error:{badmatch, {BadErrorCode, BadStatusMsg}} ->
                            err(error, [{xer, XER}, {msg, io_lib:format("~p ~s", [BadErrorCode, BadStatusMsg])}]),
                            {BadErrorCode, BadStatusMsg, State};
                        Class:Reason ->
                            err(error, [{xer,XER},  {msg, io_lib:format("~nError Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})])}]),
                            {500, "", State}
                    end
            end
    end.

handle_put(Req, State, XER, Appname, ReqBody, RequestUrl) ->
    %use of ?MODULE here is due to the meck limitation described here: https://github.com/eproxus/meck
    case ?MODULE:appname_to_field_vals(Appname, [<<"appname">>]) == none  of
        false -> {400, "Put recieved on /application/:appname but appname is already registered. Call /application/:appname/reconfigure if trying to reconfigure or delete first", State};
        true ->
            %Initial put requires the put body parameters
            ParsedBody = parse_put_body(ReqBody),
            case ParsedBody of
                invalid -> {400, "Invalid PUT Body or unparseable URL", State};   %could not parse the body
                unsupported -> {400, "Unsupported CDAP Application Type", State}; %unsupported cdap application type
                {AppType, Params} ->
                    %form shared info
                    Metricsurl = <<RequestUrl/binary, <<"/metrics">>/binary>>,
                    Healthcheckurl = <<RequestUrl/binary, <<"/healthcheck">>/binary>>,

                    try
                       case AppType of
                           <<"hydrator-pipeline">> ->
                               {Namespace, Streamname, PipelineConfigJsonURL, ParsedDependencies} = Params,
                               ConnectionURL = cdap_interface:form_stream_url_from_streamname(?CDAPURL, Namespace, Streamname),
                               ServiceEndpoints = [], %TODO: unclear if this is possible with pipelines

                               %write into mnesia, deploy
                               A = #application{appname = Appname, apptype = AppType, namespace = Namespace, healthcheckurl = Healthcheckurl, metricsurl = Metricsurl, url = RequestUrl, connectionurl = ConnectionURL, serviceendpoints = ServiceEndpoints, creationtime=erlang:system_time()},
                               {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(A) end),
                               ok = workflows:deploy_hydrator_pipeline(Req, XER, Appname, Namespace, ?CDAPURL, PipelineConfigJsonURL, ParsedDependencies, ?CONSURL, RequestUrl, Healthcheckurl, ?HCInterval, ?AutoDeregisterAfter),
                               metrics(info, Req, [{bts, iso()}, {xer, XER}, {mod, mod()}, {msg, io_lib:format("New Hydrator Application Created: ~p", [lager:pr(A, ?MODULE)])}]), %see Record Pretty Printing: https://github.com/basho/lager
                               ok;
                           <<"program-flowlet">> ->
                               {Namespace, Streamname, JarURL, ArtifactName, ArtifactVersion, AppConfig, AppPreferences, ParsedServices, Programs, ParsedProgramPreferences} = Params,
                               %Form URLs that are part of the record
                               %NOTE: These are both String concatenation functions and neither make an HTTP call so not catching normal {Code, Status} return here
                               ConnectionURL = cdap_interface:form_stream_url_from_streamname(?CDAPURL, Namespace, Streamname),
                               ServiceEndpoints = lists:map(fun(X) -> cdap_interface:form_service_json_from_service_tuple(Appname, Namespace, ?CDAPURL, X) end, ParsedServices),

                               %write into mnesia. deploy
                               A = #application{appname = Appname, apptype = AppType, namespace = Namespace, healthcheckurl = Healthcheckurl, metricsurl = Metricsurl, url = RequestUrl, connectionurl = ConnectionURL, serviceendpoints = ServiceEndpoints, creationtime=erlang:system_time()},
                               ASupplemental = #prog_flow_supp{appname = Appname, programs = Programs},
                               {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(A) end),             %warning, here be mnesia magic that knows what table you want to write to based on the record type
                               {atomic,ok} = mnesia:transaction(fun() -> mnesia:write(ASupplemental) end), %warning: ""
                               ok = workflows:deploy_cdap_app(Req, XER, Appname, ?CONSURL, ?CDAPURL, ?HCInterval, ?AutoDeregisterAfter, AppConfig, JarURL, ArtifactName, ArtifactVersion, Namespace, AppPreferences, ParsedProgramPreferences, Programs, RequestUrl, Healthcheckurl),
                               metrics(info, Req, [{bts, iso()}, {xer, XER}, {mod, mod()}, {msg, io_lib:format("New Program-Flowlet Application Created: ~p with supplemental data: ~p", [lager:pr(A, ?MODULE), lager:pr(ASupplemental, ?MODULE)])}]),
                               ok
                       end,
                       appname_to_application_http(XER, Appname, State)

                    catch
                        %catch a bad HTTP error code
                        error:{badmatch, {BadErrorCode, BadStatusMsg}} ->
                            err(error, [{xer, XER}, {msg, io_lib:format("Badmatch caught in Deploy. Rolling Back. ~p ~s", [BadErrorCode, BadStatusMsg])}]),
                            {_,_,_} = delete_app_helper(Appname, State, XER, Req),
                            {BadErrorCode, BadStatusMsg, State}; %pass the bad error/status back to user
                        Class:Reason ->
                            %generic failure catch-all, catastrophic
                            err(error, [{xer, XER}, {msg, io_lib:format("~nUnexpected Exception caught in Deploy. Error Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})])}]),
                            {_,_,_} = delete_app_helper(Appname, State, XER, Req),
                            {500, "Please report this error", State}
                    end
            end
    end.

handle_post_multidelete_app(Req, State, XER, ReqBody) ->
    %handler method for the multi delete post ala AWS
    case
        try
             B = maps:get(<<"appnames">>, jiffy:decode(ReqBody, [return_maps])),
             true = erlang:is_list(B),
             B
        catch _:_ ->
            invalid
        end
        of
        invalid -> {400, "Invalid PUT Body", State};
        IDs ->
            case IDs of
                [] -> {200, "EMPTY PUT BODY", State};
                _ ->
                %<<"*">> ->
                %this block deleted all apps, but decided this backdoor wasn't very RESTy
                %%    {atomic, Apps} = mnesia:transaction(fun() -> mnesia:match_object(application, {application, '_', '_', '_', '_', '_', '_', '_', '_', '_'}, read) end),
                %    AppsToDelete = lists:map(fun(X) -> {application, Appname, _,_,_,_,_,_,_,_} = X, Appname end, Apps),
                    Returns = lists:map(fun(X) -> ?MODULE:delete_app_helper(X, State, XER, Req) end, IDs),
                    RL = lists:map(fun({RC, _, _}) -> RC end, Returns),
                    {200, jiffy:encode(RL), State}
            end
        end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HTTP API CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Route, _Req, State) ->
     {ok, State}.
terminate(_Reason, _Route, _Req, _State) ->
     ok.
%%%FOR Cors support
%%%Note! only matches on host. Does not handle ports. See: https://github.com/s1n4/leptus/issues/55
cross_domains(_Route, _Req, State) ->
    {['_'], State}.
%%%GET Methods
get("/", Req, State) ->
    %The broker's "info" endpoint; returns some possibly useful information
    {Bts, XER} = init_api_call(Req),
    Apps = util:get_all_appnames_from_db(),
    {UT, _} = statistics(wall_clock),
    CDAPVer = cdap_interface:get_cdap_cluster_version(XER, ?CDAPURL),
    RB = {[
           {<<"cdap cluster version">>, CDAPVer},
           {<<"managed cdap url">>, ?CDAPURL},
           {<<"cdap GUI port">>, cdap_interface:get_cdap_gui_port_from_version(CDAPVer)},
           {<<"number of applications registered">>, length(Apps)},
           {<<"uptime (s)">>, UT/1000},
           {<<"broker API version">>, util:get_my_version()}
         ]},
    {RCode, RBody, RState} = {200, {json, RB}, State},
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState};
get("/application", Req, State) ->
    %get a list of all registered apps
    {Bts, XER} = init_api_call(Req),
    {RCode, RBody, RState} = {200, {json, util:get_all_appnames_from_db()}, State},
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState};
get("/application/:appname", Req, State) ->
    %get information about a registered application
    {Bts, XER} = init_api_call(Req),
    Appname = leptus_req:param(Req, appname),
    {RCode, RBody, RState} = appname_to_application_http(XER, Appname, State),
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState};
get("/application/:appname/metrics", Req, State) ->
    %get metrics for a registered application
    {Bts, XER} = init_api_call(Req),
    Appname = leptus_req:param(Req, appname),
    {RCode, RBody, RState} = case appname_to_field_vals(Appname, [<<"apptype">>, <<"namespace">>]) of
        none -> {404, "", State};
        [<<"program-flowlet">>, Namespace] ->
            {ReturnCode, ReturnBody} = cdap_interface:get_app_metrics(XER, Appname, Namespace, ?CDAPURL), %warning, see note in README, this always reutrns 200
            {ReturnCode, {json, ReturnBody}, State};
        [<<"hydrator-pipeline">>, Namespace] ->
            lager:warning("WARNING, metrics not actually implemented yet for pipelines!!"),
            {ReturnCode, ReturnBody} = cdap_interface:get_pipeline_metrics(Appname, Namespace, ?CDAPURL),
            {ReturnCode, {json, ReturnBody}, State}
    end,
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState};
get("/application/:appname/healthcheck", Req, State) ->
    %get healthcheck of an application
    {Bts, XER} = init_api_call(Req),
    Appname = leptus_req:param(Req, appname),
    lager:info(io_lib:format("Get Healthcheck recieved for ~s", [Appname])),
    {RCode, RBody, RState} = case appname_to_field_vals(Appname, [<<"apptype">>, <<"namespace">>]) of
        none -> {404, "", State};
        [<<"program-flowlet">>, Namespace] ->
            {cdap_interface:get_app_healthcheck(XER, Appname, Namespace, ?CDAPURL), "", State};
        [<<"hydrator-pipeline">>, Namespace] ->
            {cdap_interface:get_pipeline_healthcheck(XER, Appname, Namespace, ?CDAPURL, ?PipelineHealthLimit), "", State}
    end,
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState}.
%%%DELETE Methods
delete("/application/:appname", Req, State) ->
    %Uninstall and delete a CDAP app
    {Bts, XER} = init_api_call(Req),
    Appname = leptus_req:param(Req, appname),
    {RCode, RBody, RState} = delete_app_helper(Appname, State, XER, Req),
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState}.
%%%PUT Methods
put("/application/:appname", Req, State) ->
    %create a new registration; deploys and starts a cdap application
    {Bts, XER} = init_api_call(Req),
    Appname = leptus_req:param(Req, appname),
    ReqBody = leptus_req:body_raw(Req),
    {RequestUrl,_} = cowboy_req:url((leptus_req:get_req(Req))),
    {RCode, RBody, RState} = handle_put(Req, State, XER, Appname, ReqBody, RequestUrl),
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState};
put("/application/:appname/reconfigure", Req, State) ->
    %if appname already is registerd, trigger a consul pull and reconfigure
    {Bts, XER} = init_api_call(Req),
    Appname = leptus_req:param(Req, appname),
    ReqBody = leptus_req:body_raw(Req),
    {RCode, RBody, RState} = handle_reconfigure_put(Req, State, XER, Appname, ReqBody),
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState}.
%%%POST methods
post("/application/delete", Req, State) ->
    %This follows the AWS S3 Multi Key Delete: http://docs.aws.amazon.com/AmazonS3/latest/API/multiobjectdeleteapi.html
    {Bts, XER} = init_api_call(Req),
    ReqBody = leptus_req:body_raw(Req),
    {RCode, RBody, RState} = handle_post_multidelete_app(Req, State, XER, ReqBody),
    ?AUDI(Req, Bts, XER, Rcode),
    {RCode, RBody, RState}.

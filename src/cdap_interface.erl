% ============LICENSE_START=======================================================
% org.onap.dcae
% ================================================================================
% Copyright (c) 2017 AT&T Intellectual Property. All rights reserved.
% ================================================================================
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% ============LICENSE_END=========================================================
%
% ECOMP is a trademark and service mark of AT&T Intellectual Property.

-module(cdap_interface).
-export([get_app_metrics/4,
         get_app_healthcheck/4,
         push_down_config/5,
         form_service_json_from_service_tuple/4,
         form_stream_url_from_streamname/3,
         exec_programs/6,
         push_down_program_preferences/5,
         push_down_app_preferences/5,
         deploy_app/8,
         deploy_pipeline/5,
         delete_app/4,
         %get_app_programs/3,
         exec_pipeline/5,
         exec_pipeline_workflow/5,
         get_pipeline_healthcheck/5,
         get_pipeline_metrics/3,
         deploy_pipeline_dependencies/4,
         deploy_pipeline_dependencies_properties/4,
         create_namespace/3,
         get_app_config/4,
         get_app_preferences/4,
         get_cdap_cluster_version/2,
         get_cdap_gui_port_from_version/1
        ]).
-include("application.hrl").
-define(SC(L), util:concat(L)).
-define(BAD_HEALTH_CODE, 400). %%not sure if this is the best status code for "unhealthy". I don't like 500 because I am able to complete the user's request (healthcheck)

%helpful: https://robhirschfeld.com/2012/08/15/erlang-http-client-restful-api-post-example-code/

%%%
%%%INTERNAL
%%%
map_appname(Appname) ->
    %CDAP APIs do not allow app names with any special characters. Here we will map the
    %name to ensure it does not contain special characters using a regex whitelist
    %see http://stackoverflow.com/questions/3303420/regex-to-remove-all-special-characters-from-string
    re:replace(Appname, "[^0-9a-zA-Z]+", "", [{return, binary}, global]).

get_metrics_list_for_app(XER, Appname, Namespace, CDAPURL) ->
    URL = ?SC([CDAPURL, "/v3/metrics/search?target=metric&tag=namespace:", Namespace, "&tag=app:", map_appname(Appname)]),
    {ReturnCode, RetBody} = httpabs:post(XER, URL, "application/json", ""),
    case ReturnCode of
        200 -> [ X ||   X <- jiffy:decode(RetBody),binary:match(X, <<"user.">>) /= nomatch];
        _ -> 504 %must bubble this up
    end.

-spec get_app_healthcheck_program(string(), binary(), binary(), string(), #program{}) -> integer().
get_app_healthcheck_program(XER, Appname, Namespace, CDAPURL, P) ->
    %helper function: checks for a partocular program from an app
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/", P#program.type, "/", P#program.id]),
    {RC, _} = httpabs:get(XER, URL),
    case RC  of
        200 ->
            %Next make sure it's status is running
            {RC2, RB2} = httpabs:get(XER, ?SC([URL, "/status"])),
            case RC2 of
                200 ->
                    S = jiffy:decode(RB2, [return_maps]),
                    case maps:is_key(<<"status">>, S) andalso maps:get(<<"status">>, S) == <<"RUNNING">> of
                       true -> 200; %return 200
                       false -> ?BAD_HEALTH_CODE %return
                    end;
                _  -> ?BAD_HEALTH_CODE
            end;
        _ -> ?BAD_HEALTH_CODE
    end.

-spec exec_program(string(), binary(), binary(), string(), #program{}, string()) -> httpstat().
exec_program(XER, Appname, Namespace, CDAPURL, P, Exec) ->
    %Exec should be 'start' or 'stop'
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/", P#program.type, "/", P#program.id, "/", Exec]),
    httpabs:post(XER, URL, "application/json", "").

push_down_program_preference(XER, Appname, Namespace, CDAPURL, {PT, PI, PP}) ->
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/", PT, "/", PI, "/", "preferences"]),
    httpabs:put(XER, URL, "application/json", jiffy:encode(PP)).

deploy_pipeline_dependency(XER, Namespace, CDAPURL, {ArtExtendsHeader, ArtName, ArtVerHeader, ArtURL,  _}) ->
    %deploys a single dependency
    %TODO! I should really be using Erlang records more and not relying on positional arguments so much. For example, I could add a record that represents a Dependency instead of using a positionally-based tuple.
    {200, JarBody} = httpabs:get(XER, ArtURL),
    Headers = [{"Artifact-Extends", ArtExtendsHeader}, {"Artifact-Version", ArtVerHeader}],
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/artifacts/", ArtName]),
    httpabs:post(XER, URL, Headers, "application/octet-stream", JarBody).

deploy_pipeline_dependency_property(XER, Namespace, CDAPURL, {_, ArtName, ArtVerHeader, _, UIPropertiesURL}) ->
    %TODO! I should really be using Erlang records more and not relying on positional arguments so much. For example, I could add a record that represents a Dependency instead of using a positionally-based tuple.
    case UIPropertiesURL of
        none -> {200, ""}; %nothing to do
        _ ->
            {200, PropertiesJSON} = httpabs:get(XER, UIPropertiesURL),
            URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/artifacts/", ArtName, "/versions/", ArtVerHeader, "/properties"]),
            httpabs:put(XER, URL, "application/json", PropertiesJSON)
    end.

%%%
%%%EXTERNAL
%%%
get_app_metrics(XER, Appname, Namespace, CDAPURL) ->
    %Namespace should be a binary
    MetricsList = get_metrics_list_for_app(XER, Appname, Namespace, CDAPURL), %assert 200 or bomb
    case MetricsList of
        504 -> {504, []};
        %TODO! cdap seems to return a {200, []} for above call even if the app is not deployed. Is that OK? for now return empty list but maybe we should determine this and error with a 404 or something
        [] -> {200, []};
        _  ->
            URL = ?SC([CDAPURL, "/v3/metrics/query"]),
            Body = jiffy:encode(
                 {[{<<"appmetrics">>,
                  {[{<<"tags">>, {[{<<"namespace">>, Namespace}, {<<"app">>, map_appname(Appname)}]}},
                   {<<"metrics">>, MetricsList}]}
                 }]}),
            {ReturnCode, RetBody} = httpabs:post(XER, URL, "application/json", Body), %even when app does not exist this seems to return a 200 so assert it!
            case ReturnCode of
                200 -> {200, jiffy:decode(RetBody)};
                504 -> {504, []}
            end
    end.

%THIS FUNCTION WAS ONCE NEEDED
%It gets the list of all programs in a running CDAP app.
%However this is no longer used due to a feature request where people want to start/healthcheck only *some* programs in their application
%So now this information is sourced from the initial PUT request, where those programs are saved as supplemntal state
%However, leaving it here because maybe one day it will be useful
%%%get_app_programs(Appname, Namespace, CDAPURL) ->
%%%    %fetch the list of programs from a running CDAP app.
%%%    %Parse this into a list of [{ProgramType, ProgramID}] tuples.
%%%    %Used as part of healthcheck, and also as part of undeploy so I don't have to keep Programs in MNesia pipeline.
%%%    %get informaation about application
%%%    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname)]),
%%%    {ReturnCode, RetBody} = httpabs:http_get(URL),
%%%    case ReturnCode of
%%%        504 -> 504;
%%%        404 -> 404;
%%%        400 -> 400;
%%%        200 ->
%%%            Drb = jiffy:decode(RetBody, [return_maps]),
%%%            Programs = maps:get(<<"programs">>, Drb),
%%%            lists:map(fun(X) -> #program{
%%%                                   %stupid CDAP APIs require a different get call then what is returned as the Program Type. For example you need to get "flows" to get a program of type Flow. im filing a bug with them. see program-typeOne of flows, mapreduce, services, spark, workers, or workflows here: http://docs.cask.co/cdap/current/en/reference-manual/http-restful-api/lifecycle.html#details-of-a-deployed-application. I filed an issue: https://issues.cask.co/browse/CDAP-7191?filter=-2
%%%                                   %From http://docs.cask.co/cdap/current/en/developers-manual/building-blocks/program-lifecycle.html
%%%                                   %All the uppercase ones are: Flow, MapReduce, Service, Spark, Worker, Workflow
%%%                                   %From http://docs.cask.co/cdap/current/en/reference-manual/http-restful-api/lifecycle.html#details-of-a-program
%%%                                   %All the lowercase ones are: flows, mapreduce, services, spark, workers, or workflows
%%%                                   program_type = case maps:get(<<"type">>, X) of
%%%                                       <<"Flow">> -> <<"flows">>; %cdap api fail man
%%%                                       <<"Mapreduce">> -> <<"mapreduce">>;
%%%                                       <<"Service">> -> <<"services">>;
%%%                                       <<"Spark">> -> <<"spark">>;
%%%                                       <<"Worker">> -> <<"workers">>;
%%%                                       <<"Workflow">> -> <<"workflows">>
%%%                                   end,
%%%                                   program_id = maps:get(<<"id">>, X)
%%%                                }
%%%                      end, Programs)
%%%    end.
%%%
-spec get_app_healthcheck(string(), binary(), binary(), string()) -> integer().
get_app_healthcheck(XER, Appname, Namespace, CDAPURL) ->
    %cdap does not provide a simple "heathcheck" api for apps like it does metrics
    %what I am using is:
    %   making sure the application is there
    %   making sure all programs (flows, services, etc) are there
    %for now. From: http://docs.cask.co/cdap/current/en/reference-manual/http-restful-api/lifecycle.html#details-of-a-deployed-application

    Programs = util:get_programs_for_pfapp_from_db(Appname),
    %check each program
    M = lists:map(fun(X) -> get_app_healthcheck_program(XER, Appname, Namespace, CDAPURL, X) end, Programs),
    case lists:foldl(fun(X, Y) -> X == 200 andalso Y end, true, M) of %check all 200s
        true -> 200;
        false -> ?BAD_HEALTH_CODE
    end.

push_down_config(XER, Appname, Namespace, CDAPURL, ConfigJson) ->
    %http://docs.cask.co/cdap/current/en/reference-manual/http-restful-api/lifecycle.html#update-an-application
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/update"]),
    Body = jiffy:encode(
            {[
             {<<"config">>, ConfigJson}
           ]}),
    httpabs:post(XER, URL, "application/json", Body). %returns {ReturnCode, ReturnBody}

push_down_program_preferences(XER, Appname, Namespace, CDAPURL, ParsedProgramPreferences) ->
    FClosure = fun(X) -> push_down_program_preference(XER, Appname, Namespace, CDAPURL, X) end,
    workflows:all_200s_else_showerror(FClosure, ParsedProgramPreferences).

form_service_json_from_service_tuple(Appname, Namespace, CDAPURL, {SN, SE, EM}) ->
    %transforms {SN, SE, EM} into {url: foo, method: bar}
    URL = list_to_binary(?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/services/", SN, "/methods/", SE])),
    {[{<<"url">>, URL},
      {<<"method">>, EM}
    ]}.

form_stream_url_from_streamname(CDAPURL, Namespace, Streamname) ->
    list_to_binary(?SC([CDAPURL, "/v3/namespaces/", Namespace, "/streams/", Streamname])).

-spec exec_programs(string(), binary(), binary(), string(), lprogram(), string()) -> httpstat().
exec_programs(XER, Appname, Namespace, CDAPURL, Programs, Exec) ->
    FClosure = fun(X) -> exec_program(XER, Appname, Namespace, CDAPURL, X, Exec) end,
    workflows:all_200s_else_showerror(FClosure, Programs).

push_down_app_preferences(XER, Appname, Namespace, CDAPURL, AppPreferences) ->
    %use app level preferences API if specified
    %http://docs.cask.co/cdap/current/en/reference-manual/http-restful-api/preferences.html
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/preferences"]),
    httpabs:put(XER, URL, "application/json", jiffy:encode(AppPreferences)).

deploy_app(XER, Appname, Namespace, CDAPURL, JarBody, ArtifactName, ArtifactVersion, AppConfig) ->
    %Create Artifact
    %Deploy App

    %post the artifact, OK if already exists, no check
    %explicitly set artifact version becausme some JARS do not have it
    httpabs:post(XER, ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/artifacts/", ArtifactName]), [{"Artifact-Version", erlang:binary_to_list(ArtifactVersion)}], "application/octet-stream", JarBody),

    %deploy the application
    PutBody = jiffy:encode(
        {[
          {<<"artifact">>, {[
                         {<<"name">>, ArtifactName},
                         {<<"version">>, ArtifactVersion},
                         {<<"scope">>, <<"user">>}
                        ]}},
          {<<"config">>, AppConfig}
        ]}),
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname)]),
    httpabs:put(XER, URL, "application/json", PutBody).

delete_app(XER, Appname, Namespace, CDAPURL) ->
    %delete an application; works for prog-flow and hydrator
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname)]),
    httpabs:delete(XER, URL).

deploy_pipeline(XER, Appname, Namespace, CDAPURL, PipelineJson) ->
    %Deploy a hydrator pipeline, assumes namespace has already been set up
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname)]),
    httpabs:put(XER, URL, "application/json", PipelineJson).

exec_pipeline(XER, Appname, Namespace, CDAPURL, Exec) ->
    %Exec assumed to be: "resume" or "suspend"
    %TODO! REVISIT WHETHER datapipelineschedule is a parameter
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/schedules/dataPipelineSchedule/", Exec]),
    httpabs:post(XER, URL, "application/json", ""). %this CDAP API is a POST but there is no body.

exec_pipeline_workflow(XER, Appname, Namespace, CDAPURL, Exec) ->
    %Exec assumed to be: "stop" or
    %TODO! REVISIT WHETHER DataPipelineWorkflow is a parameter
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/workflows/DataPipelineWorkflow/", Exec]),
    httpabs:post(XER, URL, "application/json", ""). %this CDAP API is a POST but there is no body.

get_pipeline_healthcheck(XER, Appname, Namespace, CDAPURL, PipelineHealthLimit) ->
    URLRoot = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname)]),
    URL = ?SC([URLRoot, "/schedules/dataPipelineSchedule/status"]),
    {RC, RB} = httpabs:get(XER, URL),
    case RC /= 200 of
        true -> ?BAD_HEALTH_CODE; %failed to even hit the status.
        false ->
            Status = jiffy:decode(RB, [return_maps]),
            case maps:is_key(<<"status">>, Status) andalso maps:get(<<"status">>, Status) == <<"SCHEDULED">> of
                false -> ?BAD_HEALTH_CODE; %status is malformed or the pipeline is not scheduled, both not good
                true ->
                    %Next, check the last <LIMIT> number of runs, and report a failure if they were not sucessful, or report of more than one of them is running at the same time.
                    %This logic came from Terry.
                    %His  logic is essentially that, if your application is running, but is failing, that should be interpeted as unhealthy and needs investigation.
                    %His logic was also that if you have two runs running at the same time, that requires investigation as that should not happen.
                    %RE list_to_binary(integer_to_list( see http://stackoverflow.com/questions/4010713/integer-to-binary-erlang
                    L = list_to_binary(integer_to_list(PipelineHealthLimit)),
                    {RC2, RB2} = httpabs:get(XER, ?SC([URLRoot, "/workflows/DataPipelineWorkflow/runs?limit=", L])),
                    case RC2 /= 200 of
                        true -> RC2; %failed to even hit the status
                        false ->
                            LRST = lists:map(fun(S) ->
                                                 case maps:is_key(<<"status">>, S) of
                                                    false -> critical;
                                                    true -> case maps:get(<<"status">>, S) of <<"COMPLETED">> -> ok; <<"RUNNING">> -> running; <<"FAILED">> -> critical end
                                                end end, jiffy:decode(RB2, [return_maps])),
                            %now process the transformed list
                            %check if any had failed or if the status JSONs were malformed, or check if more than 2 running at once
                            case lists:any(fun(X) -> X == critical end, LRST) orelse length(lists:filter(fun(X) -> X == running end, LRST)) > 1 of
                                true -> ?BAD_HEALTH_CODE;
                                false -> 200 %ALL TESTS PASS
                            end
                    end
            end
    end.

get_pipeline_metrics(_Appname, _Namespace, _CDAPURL) ->
    lager:warning("WARNING, metrics not actually implemented yet for pipelines!!"),
    {200, []}.

deploy_pipeline_dependencies(XER, Namespace, CDAPURL, ParsedDependencies) ->
    FClosure = fun(X) -> deploy_pipeline_dependency(XER, Namespace, CDAPURL, X) end,
    workflows:all_200s_else_showerror(FClosure, ParsedDependencies).

deploy_pipeline_dependencies_properties(XER, Namespace, CDAPURL, ParsedDependencies) ->
    FClosure = fun(X) -> deploy_pipeline_dependency_property(XER, Namespace, CDAPURL, X) end,
    workflows:all_200s_else_showerror(FClosure, ParsedDependencies).

create_namespace(_XER, <<"default">>, _) -> {200, ""}; %no-op, already exists
create_namespace(XER, Namespace, CDAPURL) ->
    httpabs:put(XER, ?SC([CDAPURL, "/v3/namespaces/", Namespace]), "", "").

get_app_config(XER, Appname, Namespace, CDAPURL) ->
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname)]),
    {RC, RB} = httpabs:get(XER, URL),
    case RC of
        200 -> {200, maps:get(<<"configuration">>, jiffy:decode(RB, [return_maps]))};
        _   -> {RC, RB}
    end.

get_app_preferences(XER, Appname, Namespace, CDAPURL) ->
    URL = ?SC([CDAPURL, "/v3/namespaces/", Namespace, "/apps/", map_appname(Appname), "/preferences"]),
    {RC, RB} = httpabs:get(XER, URL),
    case RC of
        200 -> {200, jiffy:decode(RB, [return_maps])};
        _   -> {RC, RB}
    end.

get_cdap_cluster_version(XER, CDAPURL) ->
    %CDAP decided to change their port numbers between release 3 and 4.
    %The broker works with both.
    %In order to add the correct GUI information into the broker "info endpoint", I have to know what CDAP version we are connected to.
    %The GUI information is a convinence function for component developers that hit the broker via the CLI tool.
    URL = ?SC([CDAPURL, "/v3/version"]),
    {RC, RB} = httpabs:get(XER, URL),
    case RC of
        200 -> maps:get(<<"version">>, jiffy:decode(RB, [return_maps]));
        _   -> <<"UNKNOWN CDAP VERSION">>
    end.

-spec get_cdap_gui_port_from_version(binary() | string()) -> 9999 | 11011 | binary().
get_cdap_gui_port_from_version(Version) ->
    %given the cdap clsuter version, return the GUI port
    case re:run(Version, "3\.\[0-9]+\.[0-9]+") of
        nomatch ->
            case re:run(Version, "4\.\[0-9]+\.[0-9]+") of
                nomatch -> <<"UNKNOWN CDAP VERSION">>;
                _       -> 11011
            end;
        _ -> 9999
    end.


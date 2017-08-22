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

-module(workflows).

%Module holds functions that execute big workflows, like deploying a CDAP application. 

-include("application.hrl").
-export([deploy_cdap_app/17, %super offensive arity.. should probably start using some structs to cut this
        undeploy_cdap_app/6,
        undeploy_hydrator_pipeline/6,
        deploy_hydrator_pipeline/12,
        all_200s_else_showerror/2,
        app_config_reconfigure/7,
        app_preferences_reconfigure/7,
        smart_reconfigure/7
        ]).

-import(util, [iso/0, to_str/1]).
-import(logging, [metrics/3]).

-define(MET(Sev, Req, Bts, XER, TgtE, TgtS, TgtRSC, Msg), metrics(Sev, Req, [{bts, Bts}, {xer,XER}, {tgte, TgtE}, {tgts, TgtS}, {tgtrsc, TgtRSC}, {mod, to_str(?MODULE)}, {msg, Msg}])).
-define(CDAPE, "cdap cluster").
-define(CNSE, "consul cluster").

%private
attempt( Req, XER, { Mod, Func, Args }, ServiceName, Action, LogResponse) ->
    %Thanks Garry!!
    %Helper function to 
    %1. log the start timestamp
    %2. Do an action specificed by mod:func(args). Assumes XER always first arg
    %3. Log a metrics info statement about the API cll
    %4. assert the return code was a 200, let it crash otehrwise, caller catches
    Start = iso(),
    {RC, RB} = apply( Mod, Func, [XER | Args] ),
    ?MET(info, Req, Start, XER, ServiceName, Action, RC, case LogResponse of true -> to_str(RB); false -> "" end),
    {RC, RB}.

%public
-spec all_200s_else_showerror(fun((any()) -> httpstat()), list()) -> httpstat().
all_200s_else_showerror(FClosure, ListToMap) ->
    %Takes a "partial" with the spec: f(X) -> {HTTP_Status_Code, HTTP_Response}, maps it onto ListToMap, and either 
    %returns {200, ""} or else the first error encountered after executing the entire list (does not short circuit!)
    %
    %I say "partial" because there are no real "partials" in Erlang but you can make them using Closure's out of funs (anonymous functions), so FClosure is a Closure just waiting for the last argument
    %See:
    %https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0ahUKEwjtyeiC6LbSAhVH0FQKHffhAr0QFggcMAA&url=http%3A%2F%2Fstackoverflow.com%2Fquestions%2F13355544%2Ferlang-equivalents-of-haskell-where-partial-lambda&usg=AFQjCNHnEZjQHtQhKXN67DBKKoJpqRXztg&cad=rja
    %http://stackoverflow.com/questions/16183971/currying-functions-erlang
    L = lists:filter(fun({X, _}) -> X /= 200 end, lists:map(FClosure, ListToMap)),
    case L of 
        [] -> {200, ""};
        _ -> lists:nth(1, L)
    end.

deploy_cdap_app(Req, XER, Appname, ConsulURL, CDAPURL, HCInterval, AutoDeregisterAfter, AppConfig, JarURL, ArtifactName, ArtifactVersion, Namespace, AppPreferences, ParsedProgramPreferences, Programs, RequestUrl, Healthcheckurl) ->
    %push the UNBOUND config and preferences into Consul. 
    %I don't think we should push bound configs because triggering a "rebind" will suffer if the templating language is lost. 
    {200,_} = attempt(Req, XER, { consul_interface, consul_push_config, [ Appname, ConsulURL, AppConfig ] }, ?CNSE, "push config", true),
    
    %push the preferences
    {200,_} = attempt(Req, XER, { consul_interface, consul_push_preferences, [ Appname, ConsulURL, AppPreferences ] }, ?CNSE, "push preferences", true),
    
    %get the bound config
    {200,BoundConfig} = attempt(Req, XER, { consul_interface, consul_bind_config, [ Appname, ConsulURL ] }, "config binding service", "bind config", false),
    
    %fetch the JAR. 
    {200,JarBody} = attempt(Req, XER, { httpabs, get, [ JarURL ] }, "nexus", "file get", false),
    
    %create the Namespace
    {200,_} = attempt( Req, XER, { cdap_interface, create_namespace, [ Namespace, CDAPURL ] }, ?CDAPE, "create namespace", true),
    
    %deploy the application
    {200,_} = attempt( Req, XER, { cdap_interface, deploy_app, [ Appname, Namespace, CDAPURL, JarBody, ArtifactName, ArtifactVersion, BoundConfig ] }, ?CDAPE, "deploy application", true),
    
    %set app preferences
    {200,_} = attempt( Req, XER, { cdap_interface, push_down_app_preferences, [ Appname, Namespace, CDAPURL, AppPreferences ] }, ?CDAPE, "set app preferences", true),
    
    %push down the program preferences
    {200,_} = attempt( Req, XER, { cdap_interface, push_down_program_preferences, [ Appname, Namespace, CDAPURL, ParsedProgramPreferences ] }, ?CDAPE, "set program preferences", true),
    
    %start the CDAP application services
    {200,_} = attempt( Req, XER, { cdap_interface, exec_programs, [ Appname, Namespace, CDAPURL, Programs, "start" ] }, ?CDAPE, "start program", true),
    
    %Parse my IP and port
    {ok, {http, _, IPaS, Port, _, _}} = http_uri:parse(binary_to_list(RequestUrl)),
    
    %register with Consul; We will register the broker's URL as the address in Consul then the upstream service can do a GET on this Broker to get the resource object
    {200,_} = attempt( Req, XER, { consul_interface, consul_register, [ Appname, ConsulURL, list_to_binary(IPaS), Port, Healthcheckurl, HCInterval, AutoDeregisterAfter ] }, ?CNSE, "service register", true),

    ok. %if got to here, all went well.

-spec undeploy_cdap_app(any(), string(), binary(), string(), string(), binary()) -> ok.
undeploy_cdap_app(Req, XER, Appname, CDAPURL, ConsulURL, Namespace) ->
    %stop the CDAP programs assuming they are valid
    Programs = util:get_programs_for_pfapp_from_db(Appname),
    Bts = iso(), %record begining timestamp
    {RC, RB} = cdap_interface:exec_programs(XER, Appname, Namespace, CDAPURL, Programs, "stop"), 
    case RC of 
        200 -> ?MET(info,    Req, Bts, XER, ?CDAPE, "program stop", RC, "OK");
        400 -> ?MET(warning, Req, Bts, XER, ?CDAPE, "program stop", RC, io_lib:format("Delete called on ~s but it's programs were not running, probably indicates the app crashed in some way: ~s", [Appname, RB])); 
        404 -> ?MET(warning, Req, Bts, XER, ?CDAPE, "program stop", RC, io_lib:format("Delete called on ~s but it's gone, probably indicates a horrible manual deletion from CDAP: ~s", [Appname, RB])); 
        _   -> ?MET(warning, Req, Bts, XER, ?CDAPE, "program stop", RC, io_lib:format("Delete called on ~s but CDAP returned a ~p. This likely means things will NOT be cleaned up properly!! ~s", [Appname, RC, RB]))
    end,
    
    %delete the application
    Bts2 = iso(),
    {RC2, RB2} = cdap_interface:delete_app(XER, Appname, Namespace, CDAPURL),
    case RC2 of 
        200 -> ?MET(info,    Req, Bts2, XER, ?CDAPE, "app delete", RC2, "OK");
        404 -> ?MET(warning, Req, Bts2, XER, ?CDAPE, "app delete", RC2, io_lib:format("Delete called on ~s but it's gone, probably indicates a horrible manual deletion from CDAP: ~s", [Appname, RB2]));
        _   -> ?MET(warning, Req, Bts2, XER, ?CDAPE, "app delete", RC2, io_lib:format("Delete called on ~s but CDAP returned a ~p. This likely means things will NOT be cleaned up properly!!  ~s", [Appname, RC2, RB2])) 
    end,
    
    %deregister with consul
    Bts3 = iso(),
    {RC3, RB3} = consul_interface:consul_deregister(XER, Appname, ConsulURL),
    case RC3 of 
        200 -> ?MET(info,    Req, Bts3, XER, ?CNSE, "service deregister", RC3, "OK");
        _   -> ?MET(warning, Req, Bts3, XER, ?CNSE, "service deregister", RC3, io_lib:format("Delete called on ~s but Consul returned a ~p. This likely means a service is not cleaned up properly! ~s", [Appname, RC3, RB3]))
    end,
    
    %delete the config key stored earlier
    Bts4 = iso(),
    {RC4, RB4} = consul_interface:consul_delete_config(XER, Appname, ConsulURL),
     case RC4 of 
        200 -> ?MET(info,    Req, Bts4, XER, ?CNSE, "key (config) delete", RC4, "OK");
        404 -> ?MET(warning, Req, Bts4, XER, ?CNSE, "key (config) delete", RC4, io_lib:format("Delete called on ~s but it's consul key is gone, probably indicates a horrible manual deletion from Consul: ~s", [Appname, RB4])); 
        _   -> ?MET(warning, Req, Bts4, XER, ?CNSE, "key (config) delete", RC4, io_lib:format("Delete called on ~s but Consul returned a ~p. This likely means a key is not cleaned up properly! ~s", [Appname, RC4, RB4]))
    end,

    %delete the config key stored earlier
    Bts5 = iso(),
    {RC5, RB5} = consul_interface:consul_delete_preferences(XER, Appname, ConsulURL),
    case RC5 of 
        200 -> ?MET(info,    Req, Bts5, XER, ?CNSE, "key (preferences) delete", RC5, "OK");
        404 -> ?MET(warning, Req, Bts5, XER, ?CNSE, "key (preferences) delete", RC5, io_lib:format("Delete called on ~s but it's consul key is gone, probably indicates a horrible manual deletion from Consul: ~s", [Appname, RB5])); 
        _   -> ?MET(warning, Req, Bts5, XER, ?CNSE, "key (preferences) delete", RC5, io_lib:format("Delete called on ~s but Consul returned a ~p. This likely means a key is not cleaned up properly! ~s", [Appname, RC5, RB5]))
    end,

    ok.

deploy_hydrator_pipeline(Req, XER, Appname, Namespace, CDAPURL, PipelineConfigJsonURL, Dependencies, ConsulURL, RequestUrl, Healthcheckurl, HCInterval, AutoDeregisterAfter) ->
    %fetch the JSON
    {200,PipelineJson} = attempt(Req, XER, { httpabs, get, [ PipelineConfigJsonURL ] }, "nexus", "file get", false),
    
    %TODO! Config
    
    %create the Namespace
    {200,_} = attempt( Req, XER, { cdap_interface, create_namespace, [ Namespace, CDAPURL ] }, ?CDAPE, "create namespace", true),
    
    %deploy pipeline dependencies%
    {200,_} = attempt( Req, XER, { cdap_interface, deploy_pipeline_dependencies, [ Namespace, CDAPURL, Dependencies ] }, ?CDAPE, "deploy dependencies", true),
    
    %deploy pipeline dependencies UI properties
        %NOTE! There is a bit of redundancy with the above call. I debated merging the two. 
        %I decided against it because I want failures to load the deps seperated from failures to load the properties files, because they are different URLs.
        %Splitting them like this allows me to return the error to the user on the exact step that failed
    {200,_} = attempt( Req, XER, { cdap_interface, deploy_pipeline_dependencies_properties, [ Namespace, CDAPURL, Dependencies ] }, ?CDAPE, "deploy dependency properties", true),
    
    %deploy the pipeline
    {200,"Deploy Complete"} = attempt( Req, XER, { cdap_interface, deploy_pipeline, [ Appname, Namespace, CDAPURL, PipelineJson ] }, ?CDAPE, "deploy pipeline", true),
    
    %start the pipeline
    {200,_} = attempt( Req, XER, { cdap_interface, exec_pipeline, [ Appname, Namespace, CDAPURL, "resume" ] }, ?CDAPE, "start pipeline", true),
    
    %Parse my IP and port
    {ok, {http, _, IPaS, Port, _, _}} = http_uri:parse(binary_to_list(RequestUrl)),
    
    %register with Consul; We will register the broker's URL as the address in Consul, then the upstream service can do a GET on this Broker to get the resource object
    {200,_} = attempt( Req, XER, { consul_interface, consul_register, [ Appname, ConsulURL, list_to_binary(IPaS), Port, Healthcheckurl, HCInterval, AutoDeregisterAfter] }, ?CNSE, "service register", true),
    
    ok.

undeploy_hydrator_pipeline(Req, XER, Appname, Namespace, CDAPURL, ConsulURL) ->
    %UNDEPLOY NOTES:
    % 1 Never fail on undeploy, log and continue. 
    % 2 Leave artifact dependencies on the cluster. We can revisit this if we need a "LEAVE NO TRACE" solution. TODO.
    % 3 I noticed an asymetry in deploy/undeplopy here: there is no need to start workflows. Terry clarified this is correct: "Batch pipelines contain a schedule, but deploying the pipeline does not activate the schedule.  Resuming the schedule makes it active so the pipeline will run at its next scheduled time. When undeploying, if you only suspend the schedule which prevents future runs from starting, then any currently active runs will continue until they finish (or not finish if they are hung). So we follow up with a stop workflow to kill any run that may be in progress so the following commands will not fail (delete pipeline or delete namespace).
    %We avoid a race condition by suspending the schedule first.
   
    %suspend the pipeline
    Bts = iso(),
    {RC1, RB1} = cdap_interface:exec_pipeline(XER, Appname, Namespace, CDAPURL, "suspend"),
    case RC1 of 
        200 -> ?MET(info,    Req, Bts, XER, ?CDAPE, "pipeline suspend", RC1, "OK");
        400 -> ?MET(warning, Req, Bts, XER, ?CDAPE, "pipeline suspend", RC1, io_lib:format("Pipeline suspend called on ~s but it's was not running, probably OK, probably it is on a schedule ~s", [Appname, RB1])); 
        404 -> ?MET(warning, Req, Bts, XER, ?CDAPE, "pipeline suspend", RC1, io_lib:format("Pipeline suspend called on ~s but it's gone, probably indicates a horrible manual deletion from CDAP: ~s", [Appname, RB1]));
        _   -> ?MET(warning, Req, Bts, XER, ?CDAPE, "pipeline suspend", RC1, io_lib:format("Pipeline suspend called on ~s but CDAP unexpectedly return a ~p. This likely means things will NOT be cleaned up properly!! ~s", [Appname, RC1, RB1])) 
    end,

    %stop the workflow
    Bts2 = iso(),
    {RC2, RB2} = cdap_interface:exec_pipeline_workflow(XER, Appname, Namespace, CDAPURL, "stop"),
    case RC2 of 
        200 -> ?MET(info,    Req, Bts2, XER, ?CDAPE, "workflow stop", RC2, "OK");
        400 -> ?MET(warning, Req, Bts2, XER, ?CDAPE, "workflow stop", RC2, io_lib:format("Workflow stop called on ~s but it's was not running, probably OK, probably it is on a schedule ~s", [Appname, RB2]));
        404 -> ?MET(warning, Req, Bts2, XER, ?CDAPE, "workflow stop", RC2, io_lib:format("Workflow stop called on ~s but it's gone, probably indicates a horrible manual deletion from CDAP: ~s", [Appname, RB2]));
        _   -> ?MET(warning, Req, Bts2, XER, ?CDAPE, "workflow stop", RC2, io_lib:format("Workflow stop called on ~s but CDAP unexpectedly return a ~p. This likely means things will NOT be cleaned up properly!!  ~s", [Appname, RC2, RB2]))
    end,

    %?MET(warning, Req, Bts2, XER, ?CDAPE, "workflow stop", RC2, io_lib:format());

    %TODO! Delete config (Configs are currently not pushed for hydrator pipelines, so have to do that first)

    %delete the application
    Bts3 = iso(),
    {RC3, RB3} = cdap_interface:delete_app(XER, Appname, Namespace, CDAPURL),
    case RC3 of 
        200 -> ?MET(info,    Req, Bts3, XER, ?CDAPE, "app delete", RC3, "OK");
        404 -> ?MET(warning, Req, Bts3, XER, ?CDAPE, "app delete", RC3, io_lib:format("Delete called on ~s but it's gone, probably indicates a horrible manual deletion from CDAP: ~s", [Appname, RB3]));
        _   -> ?MET(warning, Req, Bts3, XER, ?CDAPE, "app delete", RC3, io_lib:format("Delete called on ~s but CDAP returned a ~p. This likely means things will NOT be cleaned up properly!!  ~s", [Appname, RC3, RB3])) 
    end,

    %deregister with consul
    Bts4 = iso(),
    {RC4, RB4} = consul_interface:consul_deregister(XER, Appname, ConsulURL),
    case RC4 of 
        200 -> ?MET(info,    Req, Bts4, XER, ?CNSE, "service deregister", RC4, "OK");
        _   -> ?MET(warning, Req, Bts4, XER, ?CNSE, "service deregister", RC3, io_lib:format("Delete called on ~s but Consul returned a ~p. This likely means a service is not cleaned up properly! ~s", [Appname, RC4, RB4])) 
    end,
    ok.

app_config_reconfigure(Req, XER, Appname, Namespace, ConsulURL, CDAPURL, AppConfig) ->
    %Reconfigure CDAP App's App Config
    
    %push the UNBOUND config into Consul. I don't think we should push bound configs because triggering a "rebind" will suffer if the templating language is lost.
    {200,_} = attempt( Req, XER, { consul_interface, consul_push_config, [ Appname, ConsulURL, AppConfig ] }, ?CNSE, "push config", true),
    
    %get the bound config
    {200,BoundConfig} = attempt(Req, XER, { consul_interface, consul_bind_config, [ Appname, ConsulURL ] }, "config binding service", "bind config", false),
    
    %push it to CDAP
        %TODO! What happens when we push to consul but connection to CDAP fails? Then CDAP and Consul are out of sync.
        %Maybe create a "BACKUP" key in Consul for the old config and "rollback" if the below fails
        %Transactions across distributed systems is hard =(
    {200,_} = attempt( Req, XER, { cdap_interface, push_down_config, [ Appname, Namespace, CDAPURL, BoundConfig ] }, ?CDAPE, "reconfigure app config", true),
    
    ok.

app_preferences_reconfigure(Req, XER, Appname, Namespace, ConsulURL, CDAPURL, AppPreferences) ->
    %Workflow:
    %  1) push the new preferences to Cosnul
    %  2) stop all the programs
    %  3) push the programs to CDAP
    %  4) start all the programs
    %
    %  NOTE! Currently it is assumed that preferences do not need to be bound by the config_binding_service,
    %        as only app config contains service discovery items. 
    
    Programs = util:get_programs_for_pfapp_from_db(Appname),
    
    %1 push the new prefs up to Consul
    {200,_} = attempt(Req, XER, { consul_interface, consul_push_preferences, [ Appname, ConsulURL, AppPreferences ] }, ?CNSE, "push preferences", true),

    %2 stop the programs
    {200,_} = attempt( Req, XER, { cdap_interface, exec_programs, [ Appname, Namespace, CDAPURL, Programs, "stop" ] }, ?CDAPE, "stop programs", true),

    %3 set app preferences
    {200,_} = attempt( Req, XER, { cdap_interface, push_down_app_preferences, [ Appname, Namespace, CDAPURL, AppPreferences ] }, ?CDAPE, "set app preferences", true),

    %4 start er' up again
    {200,_} = attempt( Req, XER, { cdap_interface, exec_programs, [ Appname, Namespace, CDAPURL, Programs, "start" ] }, ?CDAPE, "start program", true),

    ok.
    
smart_reconfigure(Req, XER, Appname, Namespace, ConsulURL, CDAPURL, NewConfig) ->
    %Smart reconfigure takes in a JSON (NewConfig) and tries to be "smart"; it tries to figure out whether Config is a reconfiguration of 
    %app config, app preferences, or both. 
    %
    %Specifically this workflow works as follows;
    %1) pull down AppConfig in consul
    %2) pull down Prefernces in consul
    %3) see if any keynames in this function's input (NewConfig) are keynames in AppConfig
    %    3a if so, reconfigure it
    %    3b write the delta'd AppConfig back to consul
    %4) see if any keynames in this fucntion's input (NewConfig) are keynames in Preferences
    %    4a if so, reconfigure ppreferences
    %    4b write the delta'd preferences back to consul
    %5) Return a status


    %see if we have app config overlaps
    {200, ConsulAppConfig} = consul_interface:consul_get_configuration(XER, Appname, ConsulURL),
    NewAppConfig = util:update_with_new_config_map(NewConfig, ConsulAppConfig),
    WasNewAppConfig = case NewAppConfig of 
        nooverlap -> nooverlap;
        _ ->
            ok = app_config_reconfigure(Req, XER, Appname, Namespace, ConsulURL, CDAPURL, NewAppConfig)
    end,

    %see if we have preferences overlap
    {200, ConsulPreferences} = consul_interface:consul_get_preferences(XER, Appname, ConsulURL),
    NewAppPreferences = util:update_with_new_config_map(NewConfig, ConsulPreferences),
    WasNewAppPreferences = case NewAppPreferences of 
        nooverlap -> nooverlap;
        _ ->
            ok = app_preferences_reconfigure(Req, XER, Appname, Namespace, ConsulURL, CDAPURL, NewAppPreferences)
    end,

    case WasNewAppConfig == nooverlap andalso WasNewAppPreferences == nooverlap of
        true -> 
            {400, "non-overlapping configuration was sent"};
        false -> 
            ok
    end.
    


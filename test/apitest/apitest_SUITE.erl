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
-module(apitest_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../../src/application.hrl").
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([server_health_test/1, app_deploy/1, hydrator_deploy/1, app_teardown/1, app_test/1, app_reconfigure/1, test_failures/1, app_botch_flows/1, app_botch_delete/1, app_botch_consul_delete/1, invalid_reconfigure/1, delete_all/1,
        hydrator_app_teardown/1, hydrator_test/1,
        hydrator_wdeps_deploy/1,
        hydrator_wdeps_test/1,
        hydrator_wdeps_teardown/1
        ]).

%lazy shorthands (yay C style macros! miss these in python)
-define(SC(L), util:concat(L)).
-define(PLG(K, PL), proplists:get_value(K, PL)). 
-define(XER, "testing-XER").
-define(D(X), erlang:display(X)).

all() -> [
          {group, progapi},
          {group, hydratorapi},
          {group, apibotchedflows},
          {group, apibotcheddeleted},
          {group, apibotchedconsuldeleted},
          {group, invalidreconfig},
          {group, apideleteall}
         ].
groups() ->  [
              {progapi, %prog-flow test
              [],
              [
                server_health_test,
                test_failures,
                app_deploy,
                app_test,
                app_reconfigure,
                app_test,
                app_teardown
              ]},
              {hydratorapi, %hydrator test
              [],
              [
                server_health_test,
                test_failures,
                hydrator_deploy,
                hydrator_test,
                hydrator_app_teardown,
                hydrator_wdeps_deploy,
                hydrator_wdeps_test,
                hydrator_wdeps_teardown
              ]},
              {apibotchedflows, %deploy, manually stop flows, then try to delete
              [],
              [
                server_health_test,
                app_deploy,
                app_botch_flows,
                app_teardown
              ]},
              {apibotcheddeleted, %deploy, manually stop flows, delete it manually from cdap, then try to delete
              [],
              [
                server_health_test,
                app_deploy,
                app_botch_delete,
                app_teardown
              ]},
              {apibotchedconsuldeleted, %deploy, manually stop flows, delete it manually from cdap, then try to delete
              [],
              [
                server_health_test,
                app_deploy,
                app_botch_consul_delete,
                app_teardown
              ]},
              {invalidreconfig, %call reconfigure on an app that DNE
              [],
              [
                server_health_test,
                app_deploy,
                invalid_reconfigure,
                app_teardown
              ]},
              {apideleteall, 
              [],
              [
                server_health_test,
                app_deploy,
                delete_all
              ]}
             ].

%HELPER FUNCTIONS
setup_rels(Config, D) ->
    %deploy/delete the testing keys into Consul. This would normally be done by the Cloudify plugin
    %
    %#NOTE: This is weird. The sequence of steps is:
    %  1 Cloudify populates rels key
    %  2 Cloudify sends broker the unbound config
    %  3 Broker pushes unbound config to consul
    %  4 Broker binds config
    %  5 Broker pushes bound config to CDAP
    %  Between state 1 and 3 consul is in an inconsistent state where it has only the rels key but not the config key. Not so sure about this. They seem to be a pair. Maybe the rels key should be pushed to the source node to be dealt with.
    % #Here, we are mocking step 1 
    URL = ?SC([?PLG(consul_url, Config), "/v1/kv/", ?PLG(appname, Config), ":rel"]),
    case D of 
        setup -> {200,"true"} = httpabs:put(?XER, URL, "application/json", jiffy:encode([<<"666_fake_testing_service">>]));
        teardown -> {200,"true"} = httpabs:delete(?XER, URL)
    end,
    httpabs:get(?XER, URL).

setup_fake_testing_service(Config, D) ->
    %register a fake testing service to test that the CDAP app recieved it's bound configuration properly
    Name = <<"666_fake_testing_service">>,
    SrvURL = ?SC([?PLG(consul_url, Config), "/v1/catalog/service/", Name]),
    case D of 
        setup ->
            URL = ?SC([?PLG(consul_url, Config), "/v1/agent/service/register"]),
            Body = {[{<<"name">>, Name},
                     {<<"Address">>, <<"666.666.666.666">>},
                     {<<"Port">>, 13}
                   ]},
            {200, []} = httpabs:put(?XER, URL, "application/json", jiffy:encode(Body)),
            httpabs:get(?XER, SrvURL);
        teardown ->
            %total failure on Consul's part for this not to be a delete on the same endpoint
            URL = ?SC([?PLG(consul_url, Config), "/v1/agent/service/deregister/", Name]),
            {200, []} = httpabs:put(?XER, URL, "application/json", ""),
            httpabs:get(?XER, SrvURL)
    end.

get_config_consul(C) -> 
    %get config from consul. returns the code too for tests testing for a 404
    {RC, RB} = consul_interface:consul_get_configuration(?XER, ?PLG(appname, C), ?PLG(consul_url, C)), 
    case RC of 
        200 ->  {RC,  util:ejson_to_map(RB)};
        _   ->  {RC, RB}
    end.

get_config_cdap(C) ->
    {RC, RB} = cdap_interface:get_app_config(?XER, ?PLG(appname, C), ?PLG(namespace, C),?PLG(cdap_url, C)),
    case RC of
        200 ->  
                %I think CDAP is DOUBLY encoding JSON!!
                {RC, jiffy:decode(jiffy:decode(jiffy:encode(RB)), [return_maps])};
        _   ->  {RC, RB}
    end.

get_preferences_cdap(C) ->
    {RC, RB} = cdap_interface:get_app_preferences(?XER, ?PLG(appname, C), ?PLG(namespace, C),?PLG(cdap_url, C)),
    case RC of
        200 ->  {RC,  util:ejson_to_map(RB)};
        _   ->  {RC, RB}
    end.

get_preferences_consul(C) -> 
    %get preferences from consul. returns the code too for tests testing for a 404
    {RC, RB} = consul_interface:consul_get_preferences(?XER, ?PLG(appname, C), ?PLG(consul_url, C)),
    case RC of 
        200 ->  {RC,  util:ejson_to_map(RB)};
        _   ->  {RC, RB}
    end.

valid_deploy_body(C) ->
     {[
         {<<"cdap_application_type">>, <<"program-flowlet">>},
         {<<"namespace">>, ?PLG(namespace, C)},
         {<<"streamname">>, ?PLG(streamname, C)},
         {<<"jar_url">>, ?PLG(jar_url, C)},
         {<<"artifact_name">>,  ?PLG(art_name, C)},
         {<<"artifact_version">>, ?PLG(art_ver, C)},
         {<<"app_config">>,  ?PLG(init_config, C)},
         {<<"app_preferences">>, ?PLG(init_preferences, C)},
         {<<"services">>, [{[{<<"service_name">>, <<"Greeting">>},  
                             {<<"service_endpoint">>, <<"greet">>},
                             {<<"endpoint_method">>, <<"GET">>}]}]},
         {<<"programs">>,  [
                            {[{<<"program_type">>, <<"flows">>},
                              {<<"program_id">>, <<"WhoFlow">>}]},
                            {[{<<"program_type">>, <<"services">>},
                              {<<"program_id">>, <<"Greeting">>}]}]},
         {<<"program_preferences">>, [
                                      {[{<<"program_type">>,<<"flows">>}, 
                                        {<<"program_id">>, <<"WhoFlow">>}, 
                                        {<<"program_pref">>, ?PLG(whoflowpref, C)}]}
                                      ]}
         ]}.

%%%%%%%%%%%%%%%
%TEST FUNCTIONS
init_per_suite(_C) ->
    %get platform ENVs
    [MyName, ConsulURL, _, _] = util:get_platform_envs_and_config(),
    
    BrokerUrl = case os:getenv("BROKER_TEST_TYPE") of
        false -> %no env variable means start the broker on localhost
            %start a local broker
            {ok,[syntax_tools,compiler,goldrush,lager,jiffy,mnesia,ranch,cowlib,cowboy,leptus,uuid,iso8601,cdapbroker]} = application:ensure_all_started(cdapbroker),
            "http://localhost:7777";
       "DOCKER" ->
            "http://localhost:7777";
       "REMOTE" ->
            %Using MyName, fetch from Consul the broker info
            {MyIP, MyPort} = consul_interface:consul_get_service_ip_port(MyName, ConsulURL),
            ?SC(["http://", MyIP, ":", integer_to_binary(MyPort)])
    end,

    %get NEXUS_ROOT for testing purposes.
    Nexus = os:getenv("NEXUS_RAW_ROOT"),
    true = (Nexus /= false), %blow if this wasn't set, we need it.

    {200, RB} = httpabs:get(?XER, BrokerUrl),
    CDAPUrl = maps:get(<<"managed cdap url">>, jiffy:decode(RB, [return_maps])),
    
    %set properties that are shared between program-flowlet and hydrator
    Namespace = <<"testns">>,
    CDAPUrlNS = ?SC([CDAPUrl, "/v3/namespaces/", Namespace]),

    %set up config for program-flowlet app
    Appname = <<"hwtest">>,
    Streamname = <<"who">>,

    %setup config for hydrator pipeline
    HydratorAppname = <<"hydratortest">>,
    HydratorAppURL = ?SC([CDAPUrlNS, "/apps/", HydratorAppname]),
    HydratorStreamname = <<"s1">>, %horrible name but not made by me
    
    HydratorWDepsAppname = <<"hydratorwdepstest">>,
    HydratorWDepsAppURL = ?SC([CDAPUrlNS, "/apps/", HydratorWDepsAppname]),
    HydratorWDepsStreamname = <<"t1">>, %horrible name but not made by me

    %Set up this test suites configuration 
    [{broker_url, BrokerUrl}, 
     {cdap_url,   CDAPUrl}, 
     {cdap_ns_url, CDAPUrlNS},
     {jar_url, ?SC([Nexus, "/jar_files/HelloWorld-3.4.3.jar"])},
     {consul_url, ConsulURL},
     {consul_app_url, ?SC([ConsulURL, "/v1/catalog/service/", Appname])},
     {app_url, ?SC([CDAPUrlNS, "/apps/", Appname])},
     {namespace, Namespace},
     {appname, Appname},
     {broker_app_url, ?SC([BrokerUrl, "/application/", Appname])},
     {stream_url, ?SC([CDAPUrlNS, "/streams/", Streamname])},
     {art_ver, <<"3.4.3">>},
     {art_name, <<"HelloWorld">>},
     {streamname, Streamname},
     {init_config, {[
                     {<<"streams_produces">>, <<"\{\{fake_testing_service\}\}">>},
                     {<<"services_calls">>, <<"\{\{fake_testing_service\}\}">>},
                     {<<"donotresolveme">>, <<"donotabsolveme">>}
                   ]}},
     {whoflowpref, {[{<<"progfoo">>, <<"progbar">>}]}},
     {init_preferences, {[{<<"preffoo">>, <<"prefbar">>}]}},
     {reconfig, {[{<<"foo">>, <<"bar">>}]}},

     %hydrator test properties
     {hydrator_appname, HydratorAppname},
     {broker_hydrator_app_url, ?SC([BrokerUrl, "/application/", HydratorAppname])},
     {hydrator_app_url, HydratorAppURL},
     {hydrator_json_url, ?SC([Nexus, "/json_files/t1-4.1.2.json   "])},
     {hydrator_pipeline_status_url, ?SC([HydratorAppURL, "/schedules/dataPipelineSchedule/status"])},
     {hydrator_stream_url, ?SC([CDAPUrlNS, "/streams/", HydratorStreamname])},
     {hydrator_streamname, HydratorStreamname},
     {consul_hydrator_app_url, ?SC([ConsulURL, "/v1/catalog/service/", HydratorAppname])},

     %hydrator with deps test properties
     {hydrator_wdeps_appname, HydratorWDepsAppname},
     {hydrator_wdeps_artname, <<"demoTCA">>},
     {hydrator_wdeps_artver, <<"1.0.0-SNAPSHOT">>},
     {hydrator_wdeps_app_url, HydratorWDepsAppURL},
     {broker_hydrator_wdeps_app_url, ?SC([BrokerUrl, "/application/", HydratorWDepsAppname])},
     {hydrator_wdeps_streamname, HydratorWDepsStreamname},
     {hydrator_wdeps_stream_url, ?SC([CDAPUrlNS, "/streams/", HydratorWDepsStreamname])},
     {hydrator_wdeps_json_url, ?SC([Nexus, "/json_files/t1-4.1.2.json"])},
     {hydrator_wdeps_properties_json_url, ?SC([Nexus, "/json_files/demoTCA-1.0.0-SNAPSHOT-properties.json"])},
     {hydrator_wdeps_jar_url, ?SC([Nexus, "/json_files/demoTCA-1.0.0-SNAPSHOT.jar"])},
     {hydrator_wdeps_test_data_url, ?SC([Nexus, "/txt_files/tcaDemoData100k.txt"])},
     {consul_hydrator_wdeps_app_url, ?SC([ConsulURL, "/v1/catalog/service/", HydratorWDepsAppname])},
     {hydrator_pipeline_wdeps_status_url, ?SC([HydratorWDepsAppURL, "/schedules/dataPipelineSchedule/status"])}
    ]
    .

end_per_suite(_C) ->
    _ = application:stop(cdapbroker).

server_health_test(C) ->
    {200, RB} = httpabs:get(?XER, ?PLG(broker_url,C)),
    M = jiffy:decode(RB, [return_maps]),
    true = maps:is_key(<<"managed cdap url">>, M),
    true = maps:is_key(<<"number of applications registered">>, M),
    true = maps:is_key(<<"uptime (s)">>, M),
    true = maps:is_key(<<"cdap cluster version">>, M),
    true = maps:is_key(<<"cdap GUI port">>, M),
    true = maps:is_key(<<"broker API version">>, M)
    .

app_deploy(C) -> %C == Config
    %Deploy the test application
    
    %Deploy the rel key
    {200, _} = setup_rels(C, setup),

    %Register the fake testing service to test config binding. Make sure it's not empty
    {200, F}  = setup_fake_testing_service(C, setup),
    true = F /= [],

    ExpectedBoundConfg = maps:from_list([
                               {<<"services_calls">> ,  [<<"666.666.666.666:13">>]},
                               {<<"streams_produces">>, [<<"666.666.666.666:13">>]}, 
                               {<<"donotresolveme">> ,   <<"donotabsolveme">>}
                               ]),

    %Maps can be used safely with the == operator but appears proplists cannot be
    Expected = maps:from_list([
                 {<<"appname">>, ?PLG(appname, C)},
                 {<<"apptype">>, <<"program-flowlet">>},
                 {<<"namespace">>, ?PLG(namespace, C)},
                 {<<"healthcheckurl">>, list_to_binary(?SC([?PLG(broker_app_url, C), "/healthcheck"]))},
                 {<<"metricsurl">>, list_to_binary(?SC([?PLG(broker_app_url, C), "/metrics"]))},
                 {<<"url">>, list_to_binary(?PLG(broker_app_url, C))},
                 {<<"connectionurl">>, list_to_binary(?PLG(stream_url, C))},
                 {<<"serviceendpoints">>, [#{<<"url">> => list_to_binary(?SC([?PLG(app_url, C), "/services/Greeting/methods/greet"])),
                                            <<"method">> => <<"GET">>}]},
                 {<<"unbound_config">>, #{<<"streams_produces">> => <<"\{\{fake_testing_service\}\}">>, <<"services_calls">> => <<"\{\{fake_testing_service\}\}">>, <<"donotresolveme">> => <<"donotabsolveme">>}},
                 {<<"bound_config">>, ExpectedBoundConfg}
               ]),

    %assert the current appliccation list does not contain our test app
    {200,RB0} = httpabs:get(?XER, ?SC([?PLG(broker_url, C), "/application"])),
    true = lists:all(fun(X) -> X /=  ?PLG(appname, C) end, jiffy:decode(RB0)),
    
    %deploy the app
    Body = valid_deploy_body(C),
    {200, RB} = httpabs:put(?XER, ?PLG(broker_app_url, C), "application/json", jiffy:encode(Body)),
    
    %The CDAP APIs return the config as a JSON dumped to a string, so we need to get that back into a real JSON to have key-order-independent equality testing
    Fix = fun(X) -> 
            RBMap = jiffy:decode(X, [return_maps]),
            maps:update(<<"bound_config">>, jiffy:decode(maps:get(<<"bound_config">>, RBMap), [return_maps]), RBMap)
        end,

    %assert that the return and get matches what we put in
    true = Fix(RB) == Expected,

    %assert hitting the get application endpoint works
    {200, RB2} = httpabs:get(?XER, ?PLG(broker_app_url, C)),
    true = Fix(RB2) == Expected,

    %assert the current application list now includes our new app
    {200, RB3} = httpabs:get(?XER, ?SC([?PLG(broker_url, C), "/application"])),
    true = lists:any(fun(X) -> X == ?PLG(appname, C) end, jiffy:decode(RB3)),

    %make sure it is in CDAP
    {200, _} = httpabs:get(?XER, ?PLG(app_url, C)),
    
    %check metrics
    {200, _} = httpabs:get(?XER, ?SC([?PLG(broker_app_url, C), "/metrics"])),
    
    %check healthcheck
    {200, _} = httpabs:get(?XER,?SC([?PLG(broker_app_url, C), "/healthcheck"])),

    %make sure that the service is registered. TODO! Could get more fancy by manually checking a healthcheck
    {200, RBHC} = httpabs:get(?XER,?PLG(consul_app_url, C)),
    true = jiffy:decode(RBHC) /= [],

    %check that the UNbound config is correct
    true = {200, util:ejson_to_map(?PLG(init_config, C))} == get_config_consul(C),
    
    %check that the preferences in Consul is correct
    InitPrefMap = util:ejson_to_map(?PLG(init_preferences, C)),
    true = {200, InitPrefMap} == get_preferences_consul(C),

    %make sure CDAP has right preferences
    true = {200, InitPrefMap} == get_preferences_cdap(C),

    %make sure the config binding service and pulling config out of CDAP all match
    %> get it strait from CBS
    CBSUrl = util:resolve_cbs(?XER, ?PLG(consul_url, C)),
    {200, RB4} = httpabs:get(?XER, ?SC([CBSUrl, "/service_component/", ?PLG(appname, C)])),
    %get it from cdap
    {200, CDAPConfig} = get_config_cdap(C),
    %make sure everythng is as expected
    true = ExpectedBoundConfg == jiffy:decode(RB4, [return_maps]),
    true = ExpectedBoundConfg == CDAPConfig,

    %try to put the same app again and assert you get a 400
    {400,"State: Bad Request. Return Body: Put recieved on /application/:appname but appname is already registered. Call /application/:appname/reconfigure if trying to reconfigure or delete first"} = 
        httpabs:put(?XER, ?PLG(broker_app_url, C), "application/json", jiffy:encode(Body)).

hydrator_deploy(C) ->
    Body = {[
              {<<"cdap_application_type">>, <<"hydrator-pipeline">>},
              {<<"namespace">>, ?PLG(namespace, C)},
              {<<"streamname">>, ?PLG(hydrator_streamname, C)},
              {<<"pipeline_config_json_url">>, ?PLG(hydrator_json_url, C)}
            ]},
    Expected = maps:from_list([
                  {<<"appname">>, ?PLG(hydrator_appname, C)},
                  {<<"apptype">>, <<"hydrator-pipeline">>},
                  {<<"namespace">>, ?PLG(namespace, C)},
                  {<<"healthcheckurl">>, list_to_binary(?SC([?PLG(broker_hydrator_app_url, C), "/healthcheck"]))},
                  {<<"metricsurl">>, list_to_binary(?SC([?PLG(broker_hydrator_app_url, C), "/metrics"]))},
                  {<<"url">>, list_to_binary(?PLG(broker_hydrator_app_url, C))},
                  {<<"connectionurl">>, list_to_binary(?PLG(hydrator_stream_url, C))},
                  {<<"serviceendpoints">>, []}
                ]),

    %assert the current appliccation list does not contain our test app
    {200,RB0} = httpabs:get(?XER, ?SC([?PLG(broker_url, C), "/application"])),
    true = lists:all(fun(X) -> X /=  ?PLG(hydrator_appname, C) end, jiffy:decode(RB0)),
    
    %try the deploy
    {200, RB1} = httpabs:put(?XER, ?PLG(broker_hydrator_app_url, C), "application/json", jiffy:encode(Body)),
    true = jiffy:decode(RB1, [return_maps]) == Expected,

    %make sure the Execution resume worked
    {200, RB2} = httpabs:get(?XER, ?PLG(hydrator_pipeline_status_url, C)),
    true = jiffy:decode(RB2) == {[{<<"status">>, <<"SCHEDULED">>}]},

    %make sure it is in CDAP
    {200, _} = httpabs:get(?XER, ?PLG(hydrator_app_url, C)),
     
    %assert the current application list now includes our new app
    {200, RB3} = httpabs:get(?XER, ?SC([?PLG(broker_url, C), "/application"])),
    true = lists:any(fun(X) -> X ==  ?PLG(hydrator_appname, C) end, jiffy:decode(RB3)),
    
    %check healthcheck
    {200, _} = httpabs:get(?XER,?SC([?PLG(broker_hydrator_app_url, C), "/healthcheck"])),
    
    %check metrics
    {200, _} = httpabs:get(?XER,?SC([?PLG(broker_hydrator_app_url, C), "/metrics"])),
    
    %make sure that the service is registered. TODO! Could get more fancy by manually checking a healthcheck
    {200, RBHC} = httpabs:get(?XER,?PLG(consul_hydrator_app_url, C)),
    true = jiffy:decode(RBHC) /= []
    .

hydrator_wdeps_deploy(C) ->
    Body = {[
              {<<"cdap_application_type">>, <<"hydrator-pipeline">>},
              {<<"namespace">>, ?PLG(namespace, C)},
              {<<"streamname">>, ?PLG(hydrator_wdeps_streamname, C)},
              {<<"pipeline_config_json_url">>, ?PLG(hydrator_wdeps_json_url, C)},
              {<<"dependencies">>, [
                                    {[
                                       {<<"artifact_extends_header">>, <<"system:cdap-data-pipeline[4.1.0,5.0.0)">>},
                                       {<<"artifact_name">>, ?PLG(hydrator_wdeps_artname, C)},
                                       {<<"artifact_version_header">>, ?PLG(hydrator_wdeps_artver, C)},
                                       {<<"artifact_url">>, ?PLG(hydrator_wdeps_jar_url, C)},
                                       {<<"ui_properties_url">>, ?PLG(hydrator_wdeps_properties_json_url, C)}
                                    ]}
                                   ]}
            ]},
    Expected = maps:from_list([
                  {<<"appname">>, ?PLG(hydrator_wdeps_appname, C)},
                  {<<"apptype">>, <<"hydrator-pipeline">>},
                  {<<"namespace">>, ?PLG(namespace, C)},
                  {<<"healthcheckurl">>, list_to_binary(?SC([?PLG(broker_hydrator_wdeps_app_url, C), "/healthcheck"]))},
                  {<<"metricsurl">>, list_to_binary(?SC([?PLG(broker_hydrator_wdeps_app_url, C), "/metrics"]))},
                  {<<"url">>, list_to_binary(?PLG(broker_hydrator_wdeps_app_url, C))},
                  {<<"connectionurl">>, list_to_binary(?PLG(hydrator_wdeps_stream_url, C))},
                  {<<"serviceendpoints">>, []}
                ]),
    %assert the current appliccation list does not contain our test app
    {200,RB0} = httpabs:get(?XER,?SC([?PLG(broker_url, C), "/application"])),
    true = lists:all(fun(X) -> X /=  ?PLG(hydrator_wdeps_appname, C) end, jiffy:decode(RB0)),

    %try the deploy
    {200, RB1} = httpabs:put(?XER, ?PLG(broker_hydrator_wdeps_app_url, C), "application/json", jiffy:encode(Body)),
    true = jiffy:decode(RB1, [return_maps]) == Expected,
    
    %make sure properties are loaded, test artifact
    {200, _} = httpabs:get(?XER,?SC([?PLG(cdap_ns_url, C), "/artifacts/", ?PLG(hydrator_wdeps_artname, C), "/versions/", ?PLG(hydrator_wdeps_artver, C), "/properties"])),

     %make sure the Execution resume worked
    {200, RB2} = httpabs:get(?XER,?PLG(hydrator_pipeline_wdeps_status_url, C)),
    true = jiffy:decode(RB2) == {[{<<"status">>, <<"SCHEDULED">>}]},

    %make sure it is in CDAP
    {200, _} = httpabs:get(?XER,?PLG(hydrator_wdeps_app_url, C)),
     
    %assert the current application list now includes our new app
    {200, RB3} = httpabs:get(?XER,?SC([?PLG(broker_url, C), "/application"])),
    true = lists:any(fun(X) -> X ==  ?PLG(hydrator_wdeps_appname, C) end, jiffy:decode(RB3)),
    
    %check healthcheck
    {200, _} = httpabs:get(?XER,?SC([?PLG(broker_hydrator_wdeps_app_url, C), "/healthcheck"])),
    
    %check metrics
    {200, _} = httpabs:get(?XER,?SC([?PLG(broker_hydrator_wdeps_app_url, C), "/metrics"])),
    
    %make sure that the service is registered. TODO! Could get more fancy by manually checking a healthcheck
    {200, RBHC} = httpabs:get(?XER,?PLG(consul_hydrator_wdeps_app_url, C)),
    true = jiffy:decode(RBHC) /= []
    .
    
hydrator_test(C) ->
    %test te app by injecting some data into the stream and getting it out
        %Sleeping since HTTP services may still be booting up: see https://issues.cask.co/browse/CDAP-812
    ok = timer:sleep(30000), %30s
    %curl into stream
    {200, _} = httpabs:post(?XER, ?PLG(hydrator_stream_url, C), "text/plain", "beer, vodka, gin"),
    %query data out
    PB = jiffy:encode({[{<<"query">>, <<"select v1, v2, v3 from dataset_pf1">>}]}),
    {200, RB} = httpabs:post(?XER, ?SC([?PLG(cdap_ns_url, C), "/data/explore/queries"]), "text/plain", PB),
    {[{<<"handle">>, Handle}]} = jiffy:decode(RB),
    %results can take time, sleep again
    ok = timer:sleep(30000),
    Expected = {[
     {<<"status">>,<<"FINISHED">>},
     {<<"hasResults">>,true}
    ]},
    {200, RB2} = httpabs:get(?XER, ?SC([?PLG(cdap_url, C), "/v3/data/explore/queries", "/", Handle, "/status"])),
    true = Expected == jiffy:decode(RB2),
    {200, _} = httpabs:post(?XER, ?SC([?PLG(cdap_url, C), "/v3/data/explore/queries", "/", Handle, "/next"]), "text/plain", "")
    .

app_test(C) ->
    %Sleeping since HTTP services may still be booting up: see https://issues.cask.co/browse/CDAP-812
    ok = timer:sleep(30000), %30s
    {200, _} = httpabs:post(?XER, ?PLG(stream_url, C), "text/plain", "'Prince of Darkness'"),
    {200, "Hello 'Prince of Darkness'!"} = httpabs:get(?XER,?SC([?PLG(app_url, C), "/services/Greeting/methods/greet"])).

app_reconfigure(C) ->
    %Test app reconfiguration
    %test new config right in Consul
    true = {200, util:ejson_to_map(?PLG(init_config, C))} == get_config_consul(C),

    %do the reconfig
    ReconfigMap = util:ejson_to_map({[{<<"foo REDUX EDITION">>, <<"bar">>}, {<<"LEAVE ME ALONE">>, <<"CONFIG EDITION">>}]}),
    %test httpabs bad body (not encoded as JSON)
    {400,"ERROR: The request Body is malformed"} = httpabs:put(?XER, ?SC([?PLG(broker_app_url, C), "/reconfigure"]), "application/json", {[{<<"reconfiguration_type">>, <<"program-flowlet-app-config">>}, {<<"config">>, ReconfigMap}]}),
    %do it properly
    {200, _} = httpabs:put(?XER, ?SC([?PLG(broker_app_url, C), "/reconfigure"]), "application/json", jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-app-config">>},{<<"config">>, ReconfigMap}]})),
    %test new config right in consul
    true = {200, ReconfigMap} == get_config_consul(C),
    %test new config right in cdap
    true = {200, ReconfigMap} == get_config_cdap(C),
 
    %Test preferences reconfiguration
    %check that the preferences in Consul is correct
    InitMap = util:ejson_to_map(?PLG(init_preferences, C)),
    true = {200, InitMap} == get_preferences_consul(C),
    %check that the preferences in CDAP are correct
    true = {200, InitMap} == get_preferences_cdap(C),
    %reconfigure the preferences
    PreferencesReconfigMap = util:ejson_to_map({[{<<"preffoo REDUX EDITION">>, <<"prefbar REMIXXX">>}, {<<"LEAVE ME ALONE">>, <<"PREFERENCES EDITION">>}]}),
    {200, _} = httpabs:put(?XER, ?SC([?PLG(broker_app_url, C), "/reconfigure"]), "application/json", jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-app-preferences">>},{<<"config">>, PreferencesReconfigMap}]})),
    %make sure consul has right preferences
    true = {200, PreferencesReconfigMap} == get_preferences_consul(C),
    %make sure CDAP has right preferences
    true = {200, PreferencesReconfigMap} == get_preferences_cdap(C),

    %test the smart reconfiguration call
    %try to give it a smart where there are keys in just preferences
    SmartReconfigPrefMap = util:ejson_to_map({[{<<"preffoo REDUX EDITION">>, <<"BAR'D AGAIN">>}]}),
    {200, _} = httpabs:put(?XER, ?SC([?PLG(broker_app_url, C), "/reconfigure"]), "application/json", jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-smart">>},{<<"config">>, SmartReconfigPrefMap}]})),
    ExpectedNewPreferences = #{<<"LEAVE ME ALONE">>=><<"PREFERENCES EDITION">>,<<"preffoo REDUX EDITION">>=><<"BAR'D AGAIN">>},
    true = {200, ExpectedNewPreferences} == get_preferences_consul(C),
    true = {200, ExpectedNewPreferences} == get_preferences_cdap(C),

    %try to give it a smart where there are keys in just config
    SmartReconfigConfig = {[{<<"foo REDUX EDITION">>, <<"FOO'D AGAIN">>}]},
    {200, _} = httpabs:put(?XER, ?SC([?PLG(broker_app_url, C), "/reconfigure"]), "application/json", jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-smart">>},{<<"config">>, SmartReconfigConfig}]})),
    %make sure CDAP and Consul agree and are equal to what we expected
    ExpectedNewConfig = #{<<"LEAVE ME ALONE">>=><<"CONFIG EDITION">>,<<"foo REDUX EDITION">>=><<"FOO'D AGAIN">>},
    true = {200, ExpectedNewConfig} == get_config_consul(C),
    true = {200, ExpectedNewConfig} == get_config_cdap(C),

    %try to give it a smart where there are keys in both preferences and config
    SmartReconfigBoth = {[{<<"foo REDUX EDITION">>, <<"FOO'D AGAIN AGAIN">>}, {<<"preffoo REDUX EDITION">>, <<"BAR'D AGAIN AGAIN">>}]},
    {200, _} = httpabs:put(?XER, ?SC([?PLG(broker_app_url, C), "/reconfigure"]), "application/json", jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-smart">>},{<<"config">>, SmartReconfigBoth}]})),
    ExpectedNewPreferencesBoth = #{<<"LEAVE ME ALONE">>=><<"PREFERENCES EDITION">>,<<"preffoo REDUX EDITION">>=><<"BAR'D AGAIN AGAIN">>},
    ExpectedNewConfigBoth = #{<<"LEAVE ME ALONE">>=><<"CONFIG EDITION">>,<<"foo REDUX EDITION">>=><<"FOO'D AGAIN AGAIN">>},
    true = {200, ExpectedNewPreferencesBoth} == get_preferences_consul(C),
    true = {200, ExpectedNewPreferencesBoth} == get_preferences_cdap(C),
    true = {200, ExpectedNewConfigBoth} == get_config_consul(C),
    true = {200, ExpectedNewConfigBoth} == get_config_cdap(C),
    
    %try to give it a smart where there are no overlaps 
    SmartReconfigNone = {[{<<"EMPTY">>, <<"LIKE YOUR SOUL">>}]},
    {400, _} = httpabs:put(?XER, ?SC([?PLG(broker_app_url, C), "/reconfigure"]), "application/json", jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-smart">>},{<<"config">>, SmartReconfigNone}]})),
    true = {200, ExpectedNewPreferencesBoth} == get_preferences_consul(C),
    true = {200, ExpectedNewPreferencesBoth} == get_preferences_cdap(C),
    true = {200, ExpectedNewConfigBoth} == get_config_consul(C),
    true = {200, ExpectedNewConfigBoth} == get_config_cdap(C)
    .

app_botch_flows(C) ->
    %check healthcheck
    {200, _} = httpabs:get(?XER,?SC([?PLG(broker_app_url, C), "/healthcheck"])),

    %purposely shut down a flow "manually" to test that undeploy works with a "partially deployed" app
    {200, []} = cdap_interface:exec_programs(?XER, ?PLG(appname, C), ?PLG(namespace, C), ?PLG(cdap_url, C), 
                                                [#program{type = <<"flows">>, id = <<"WhoFlow">>}, #program{type = <<"services">>, id = <<"Greeting">>}], "stop"),
    %make sure healthcheck now fails
    {400, _} = httpabs:get(?XER,?SC([?PLG(broker_app_url, C), "/healthcheck"]))
    .

app_botch_delete(C) ->
    %purposely shut down flows and then delete the app from the CDAP api to test undeploy works with a [gone] app 
    {200, []} = cdap_interface:exec_programs(?XER, ?PLG(appname, C), ?PLG(namespace, C), ?PLG(cdap_url, C), 
                                                [#program{type = <<"flows">>, id = <<"WhoFlow">>}, #program{type = <<"services">>, id = <<"Greeting">>}], "stop"),
    {200, []} = cdap_interface:delete_app(?XER, ?PLG(appname, C), ?PLG(namespace, C), ?PLG(cdap_url, C)),
    
    %make sure healthcheck now fails
    {400, _} = httpabs:get(?XER,?SC([?PLG(broker_app_url, C), "/healthcheck"]))
    .

app_botch_consul_delete(C) ->
    %purposefully delete the config in consul to make sure delete doesnt blow up
    {200, "true"} = consul_interface:consul_delete_config(?XER, ?PLG(appname, C),?PLG(consul_url, C)).

app_teardown(C) ->
    %Test app teardown and delete
    %app is there for now in broker
    {200,_ } = httpabs:get(?XER,?PLG(broker_app_url, C)),

    %teardown the test application
    {200, []} = httpabs:delete(?XER, ?PLG(broker_app_url, C)),
    
    %make sure the broker deleted the config from Consul
    {404, _} = get_config_consul(C),

    %make sure broker deleted the preferences
    {404, _} = get_preferences_consul(C),

    %make sure the broker app url no longer exists
    {404, _ } = httpabs:get(?XER,?PLG(broker_app_url, C)),
    
    %teardown the testing rels
    {404, _} = setup_rels(C, teardown),
    
    %teardown the fake service and make sure it is gone
    {200, Srv} = setup_fake_testing_service(C, teardown),
    true = Srv == "[]",
    
    %cdap app gone
    {404,"State: Not Found. Return Body: 'application:testns.hwtest.-SNAPSHOT' was not found."} = httpabs:get(?XER,?PLG(app_url, C)),
    
    %make sure that the service is not registered. TODO! Could get more fancy by manually checking a healthcheck
    {200, RBHC} = httpabs:get(?XER,?PLG(consul_app_url, C)),
    true = jiffy:decode(RBHC) == [].

hydrator_app_teardown(C) ->
    %Test app teardown and delete
    %app is there for now in cdap
    {200, _} = httpabs:get(?XER,?PLG(hydrator_app_url, C)),
    %app is in broker
    {200,_ } = httpabs:get(?XER,?PLG(broker_hydrator_app_url, C)),
    %teardown the test application
    {200, []} = httpabs:delete(?XER, ?PLG(broker_hydrator_app_url, C)),
    %make sure the broker deleted the config from Consul
    ?D(<<"todo! put this back:">>),
    %{404, _} = get_config_consul(C),
    %make sure the broker app url no longer exists
    {404, _ } = httpabs:get(?XER,?PLG(broker_hydrator_app_url, C)),
    %make sure gone from CDAP
    {404,"State: Not Found. Return Body: 'application:testns.hydratortest.-SNAPSHOT' was not found."} = httpabs:get(?XER,?PLG(hydrator_app_url, C)),
    %make sure that the service is not registered. TODO! Could get more fancy by manually checking a healthcheck
    {200, RBHC} = httpabs:get(?XER,?PLG(consul_hydrator_app_url, C)),
    true = jiffy:decode(RBHC) == []
    .

hydrator_wdeps_test(C) ->
    %test te app by injecting some data into the stream and getting it out
        %Sleeping since HTTP services may still be booting up: see https://issues.cask.co/browse/CDAP-812
    ok = timer:sleep(30000), %30s
    %curl into stream
    %grab the test data
    {200, TestData} = httpabs:get(?XER,?PLG(hydrator_wdeps_test_data_url, C)),
    %push it in
    {200, _} = httpabs:post(?XER, ?SC([?PLG(hydrator_wdeps_stream_url, C), "/batch"]), "text/plain", TestData),
    %query data out
    PB = jiffy:encode({[{<<"query">>, <<"select ts from dataset_t1file">>}]}),
    {200, RB} = httpabs:post(?XER, ?SC([?PLG(cdap_ns_url, C), "/data/explore/queries"]), "text/plain", PB),
    {[{<<"handle">>, Handle}]} = jiffy:decode(RB),
    %results can take time, sleep again
    ok = timer:sleep(30000),
    Expected = {[
     {<<"status">>,<<"FINISHED">>},
     {<<"hasResults">>,true}
    ]},
    {200, RB2} = httpabs:get(?XER,?SC([?PLG(cdap_url, C), "/v3/data/explore/queries", "/", Handle, "/status"])),
    true = Expected == jiffy:decode(RB2),
    {200, _} = httpabs:post(?XER, ?SC([?PLG(cdap_url, C), "/v3/data/explore/queries", "/", Handle, "/next"]), "text/plain", "")
    .

hydrator_wdeps_teardown(C) ->
    %Test app teardown and delete
    %app is there for now in cdap
    {200, _} = httpabs:get(?XER,?PLG(hydrator_wdeps_app_url, C)),
    %app is in broker
    {200,_ } = httpabs:get(?XER,?PLG(broker_hydrator_wdeps_app_url, C)),
    %teardown the test application
    {200, []} = httpabs:delete(?XER, ?PLG(broker_hydrator_wdeps_app_url, C)),
    %make sure the broker deleted the config from Consul
    ?D(<<"todo! put this back:">>),
    %{404, _} = get_config_consul(C),
    %make sure the broker app url no longer exists
    {404, _ } = httpabs:get(?XER,?PLG(broker_hydrator_wdeps_app_url, C)),
    %make sure gone from CDAP
    {404,"State: Not Found. Return Body: 'application:testns.hydratortest.-SNAPSHOT' was not found."} = httpabs:get(?XER,?PLG(hydrator_app_url, C)),
    %make sure that the service is not registered. TODO! Could get more fancy by manually checking a healthcheck
    {200, RBHC} = httpabs:get(?XER,?PLG(consul_hydrator_wdeps_app_url, C)),
    true = jiffy:decode(RBHC) == []
    .

test_failures(C) ->
    %test things that should fail
    %delete a non-existent app
    {404, "State: Not Found. Return Body: Tried to delete an application that was not registered"} = 
        httpabs:delete(?XER, ?SC([?PLG(broker_app_url, C), "MYFRIENDOFMISERY"])),
    
    %malformed Broker put
    URL = ?SC([?PLG(broker_app_url, C), "FAILURETEST"]),
    Body = {[
              {<<"malformed">>, <<"i am">>}
            ]},
    {400, "State: Bad Request. Return Body: Invalid PUT Body or unparseable URL"} = httpabs:put(?XER, URL, "application/json", jiffy:encode(Body)),

    %deploy a bad CDAP app with a bad program_id
    Body2 = {[
      {<<"cdap_application_type">>, <<"program-flowlet">>},
      {<<"namespace">>, ?PLG(namespace, C)},
      {<<"streamname">>, ?PLG(streamname, C)},
      {<<"jar_url">>, ?PLG(jar_url, C)},
      {<<"artifact_name">>,  ?PLG(art_name, C)},
      {<<"artifact_version">>, ?PLG(art_ver, C)},
      {<<"app_config">>,  ?PLG(init_config, C)},
      {<<"app_preferences">>, ?PLG(init_preferences, C)},
      {<<"services">>, [{[{<<"service_name">>, <<"Greeting">>},  
                          {<<"service_endpoint">>, <<"greet">>},
                          {<<"endpoint_method">>, <<"GET">>}]}]},
      {<<"programs">>,  [
                         {[{<<"program_type">>, <<"flows">>},
                           {<<"program_id">>, <<"DISSAPOINTMENT">>}]}
                         ]},
      {<<"program_preferences">>, []}
      ]},
    %WORKS IN CDAP 3:
    %{404,"State: Not Found. Return Body: State: Not Found. Return Body: 'program:testns.hwtestFAILURETEST.flow.DISSAPOINTMENT' was not found."} = httpabs:put(?XER, URL, "application/json", jiffy:encode(Body2)),
    %WORKS IN CDAP 4 (looks like they are doing more intrispection on the jar name)
    {404,_} = httpabs:put(?XER, URL, "application/json", jiffy:encode(Body2)),
    %make sure the rollback happened
    {200, "[]"} = httpabs:get(?XER,?SC([?PLG(broker_url, C), "/application"])),
    
    %try to deploy with a bad URL where bad means nonexistent (504)
    Body3 = {[
         {<<"cdap_application_type">>, <<"program-flowlet">>},
         {<<"namespace">>, ?PLG(namespace, C)},
         {<<"streamname">>, ?PLG(streamname, C)},
         {<<"jar_url">>, ?SC([?PLG(jar_url, C), "DOESNOTEXISTMOSTLIKELY"])},
         {<<"artifact_name">>,  ?PLG(art_name, C)},
         {<<"artifact_version">>, ?PLG(art_ver, C)},
         {<<"app_config">>,  ?PLG(init_config, C)},
         {<<"app_preferences">>, ?PLG(init_preferences, C)},
         {<<"services">>, [{[{<<"service_name">>, <<"Greeting">>},  {<<"service_endpoint">>, <<"greet">>}, {<<"endpoint_method">>, <<"GET">>}]}]},
         {<<"programs">>,  [{[{<<"program_type">>, <<"flows">>},{<<"program_id">>, <<"WhoFlow">>}]},{[{<<"program_type">>, <<"services">>},{<<"program_id">>, <<"Greeting">>}]}]},
         {<<"program_preferences">>, [{[{<<"program_type">>,<<"flows">>}, {<<"program_id">>, <<"WhoFlow">>}, {<<"program_pref">>, ?PLG(whoflowpref, C)}]}]}
         ]},
    {404, _} = httpabs:put(?XER, URL, "application/json", jiffy:encode(Body3)),

    %try to deploy with a bad URL where bad means malformed
    Body4 = {[
         {<<"cdap_application_type">>, <<"program-flowlet">>},
         {<<"namespace">>, ?PLG(namespace, C)},
         {<<"streamname">>, ?PLG(streamname, C)},
         {<<"jar_url">>, <<"THIS IS NOT EVEN A URL WHAT ARE YOU DOING TO ME">>},
         {<<"artifact_name">>,  ?PLG(art_name, C)},
         {<<"artifact_version">>, ?PLG(art_ver, C)},
         {<<"app_config">>,  ?PLG(init_config, C)},
         {<<"app_preferences">>, ?PLG(init_preferences, C)},
         {<<"services">>, [{[{<<"service_name">>, <<"Greeting">>},  {<<"service_endpoint">>, <<"greet">>}, {<<"endpoint_method">>, <<"GET">>}]}]},
         {<<"programs">>,  [{[{<<"program_type">>, <<"flows">>},{<<"program_id">>, <<"WhoFlow">>}]},{[{<<"program_type">>, <<"services">>},{<<"program_id">>, <<"Greeting">>}]}]},
         {<<"program_preferences">>, [{[{<<"program_type">>,<<"flows">>}, {<<"program_id">>, <<"WhoFlow">>}, {<<"program_pref">>, ?PLG(whoflowpref, C)}]}]}
         ]},
         {400,"State: Bad Request. Return Body: ERROR: The following URL is malformed: THIS IS NOT EVEN A URL WHAT ARE YOU DOING TO ME"} = httpabs:put(?XER, URL, "application/json", jiffy:encode(Body4))
    .

invalid_reconfigure(C) ->
    %test reconfiguring an app that does not exist despite put body being correct
    {404,"State: Not Found. Return Body: Reconfigure recieved but the app is not registered"} = httpabs:put(?XER, ?SC([?PLG(broker_app_url, C), "THE_VOID", "/reconfigure"]), "application/json", jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-app-config>">>}, {<<"config">>, {[{<<"foo">>, <<"bar">>}]}}]})),

     %test reconfiguring with an invalid PUT body (missing "reconfiguration_type")
    {400,"State: Bad Request. Return Body: Invalid PUT Reconfigure Body: key 'reconfiguration_type' is missing"} = httpabs:put(?XER, ?SC([?PLG(broker_app_url,C),  "/reconfigure"]), "application/json", jiffy:encode({[{<<"config">>, <<"bar">>}]})),

     %test reconfiguring with an invalid PUT body (missing app_config)
    {400,"State: Bad Request. Return Body: Invalid PUT Reconfigure Body: key 'config' is missing"} = httpabs:put(?XER, ?SC([?PLG(broker_app_url,C),  "/reconfigure"]), "application/json", jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-app-config">>}, {<<"foo">>, <<"bar">>}]})),

    %test reconfiguring an invalid (unimplemented) type
    {501, "State: Not Implemented. Return Body: This type (EMPTINESS) of reconfiguration is not implemented"} = httpabs:put(?XER, ?SC([?PLG(broker_app_url,C),  "/reconfigure"]), "application/json", jiffy:encode({[{<<"config">>, <<"bar">>}, {<<"reconfiguration_type">>, <<"EMPTINESS">>}]}))
    .

delete_all(C) ->
    %test invalid key
    Body1 = jiffy:encode({[{<<"ids">>, [<<"hwtest">>]}]}),
    {400,"State: Bad Request. Return Body: Invalid PUT Body"} = httpabs:post(?XER, ?SC([?PLG(broker_url, C), "/application/delete"]), "application/json", Body1),
    %test invalid: not a list
    Body2 = jiffy:encode({[{<<"appnames">>, <<"hwtest">>}]}),
    {400,"State: Bad Request. Return Body: Invalid PUT Body"} = httpabs:post(?XER, ?SC([?PLG(broker_url, C), "/application/delete"]), "application/json", Body2),
    %test undeploy a real app and also an app that is not deployed
    Body3 = jiffy:encode({[{<<"appnames">>, [<<"hwtest">>, <<"dissapointment">>]}]}),
    {200, "[200,404]"} = httpabs:post(?XER, ?SC([?PLG(broker_url, C), "/application/delete"]), "application/json", Body3),
    %teardown the fake service and make sure it is gone
    {200, Srv} = setup_fake_testing_service(C, teardown),
    true = Srv == "[]".
    

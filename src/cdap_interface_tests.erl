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

-module(cdap_interface_tests).
-include_lib("eunit/include/eunit.hrl").

-import(cdap_interface, [
           get_cdap_gui_port_from_version/1,
           map_appname/1,
           get_cdap_cluster_version/2,
           form_stream_url_from_streamname/3,
           form_service_json_from_service_tuple/4,
           get_app_preferences/4,
           get_app_config/4,
           get_pipeline_healthcheck/5
          ]).

get_pipeline_healthcheck_test() ->
    FakeReturn = jiffy:encode({[{<<"status">>, <<"SCHEDULED">>}]}),
    try meck:new(httpabs, [passthrough]) catch _:_ -> ok end,
    %notfound
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/schedules/dataPipelineSchedule/status"  -> {200, FakeReturn};
                                   "http://666.666.666.666:666/v3/namespaces/testns/apps/1234appNOTFOUND/schedules/dataPipelineSchedule/status" -> {404, ""}
                              end end),
    ?assert(get_pipeline_healthcheck("", "1234%%%%^@#$%@#$%#$^@$.appNOTFOUND", "testns", "http://666.666.666.666:666", 666) == 400),

    %bad status
    FakeReturnBadStatus = jiffy:encode({[{<<"status">>, <<"nosoupforyou">>}]}),
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/schedules/dataPipelineSchedule/status"  -> {200, FakeReturnBadStatus}
                              end end),
    ?assert(get_pipeline_healthcheck("", "1234%%%%^@#$%@#$%#$^@$.app", "testns", "http://666.666.666.666:666", 666) == 400),

     %good status but no status endpoint
     meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                   "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/schedules/dataPipelineSchedule/status"  -> {200, FakeReturn};
                                   "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/workflows/DataPipelineWorkflow/runs?limit=666" -> {404, ""}
                               end end),
    ?assert(get_pipeline_healthcheck("", "1234%%%%^@#$%@#$%#$^@$.app", "testns", "http://666.666.666.666:666", 666) == 404),

    %good status but a bad malformed inner
    MalformedRuns = jiffy:encode([
                            {[{<<"nostatus">>, <<"foryou">>}]},
                            {[{<<"status">>, <<"RUNNING">>}]}
                           ]),
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                   "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/schedules/dataPipelineSchedule/status"  -> {200, FakeReturn};
                                   "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/workflows/DataPipelineWorkflow/runs?limit=666" -> {200, MalformedRuns}
                              end end),

    ?assert(get_pipeline_healthcheck("", "1234%%%%^@#$%@#$%#$^@$.app", "testns", "http://666.666.666.666:666", 666) == 400),

    %good status but a not good inner
    BadRuns = jiffy:encode([
                            {[{<<"status">>, <<"FAILED">>}]},
                            {[{<<"status">>, <<"RUNNING">>}]}
                           ]),
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/schedules/dataPipelineSchedule/status"  -> {200, FakeReturn};
                                  "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/workflows/DataPipelineWorkflow/runs?limit=666" -> {200, BadRuns}
                              end end),
    ?assert(get_pipeline_healthcheck("", "1234%%%%^@#$%@#$%#$^@$.app", "testns", "http://666.666.666.666:666", 666) == 400),

    %two running
    TwoRunning = jiffy:encode([
                            {[{<<"status">>, <<"RUNNING">>}]},
                            {[{<<"status">>, <<"COMPLETED">>}]},
                            {[{<<"status">>, <<"RUNNING">>}]}
                           ]),
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/schedules/dataPipelineSchedule/status"  -> {200, FakeReturn};
                                  "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/workflows/DataPipelineWorkflow/runs?limit=666" -> {200, TwoRunning}
                              end end),
    ?assert(get_pipeline_healthcheck("", "1234%%%%^@#$%@#$%#$^@$.app", "testns", "http://666.666.666.666:666", 666) == 400),

    %all good
     %good status but a not good inner
    GoodRuns = jiffy:encode([
                            {[{<<"status">>, <<"COMPLETED">>}]},
                            {[{<<"status">>, <<"RUNNING">>}]},
                            {[{<<"status">>, <<"COMPLETED">>}]}
                           ]),
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/schedules/dataPipelineSchedule/status"  -> {200, FakeReturn};
                                  "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/workflows/DataPipelineWorkflow/runs?limit=666" -> {200, GoodRuns}
                              end end),
    ?assert(get_pipeline_healthcheck("", "1234%%%%^@#$%@#$%#$^@$.app", "testns", "http://666.666.666.666:666", 666) == 200),

    meck:unload(httpabs).



get_app_config_test() ->
    FakeReturn = jiffy:encode({[{<<"configuration">>, {[{<<"welcome">>, <<"toeternity">>}]}}]}),
    try meck:new(httpabs, [passthrough]) catch _:_ -> ok end,
    meck:expect(httpabs, get, fun(_XER, URL) ->
                                  case URL of
                                      "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app"  -> {200, FakeReturn};
                                      "http://666.666.666.666:666/v3/namespaces/testns/apps/1234appNOTFOUND" -> {404, ""}
                                  end
                              end),
    ?assert(get_app_config("", "1234%%%%^@#$%@#$%#$^@$.app", "testns", "http://666.666.666.666:666") ==
             {200, #{<<"welcome">> => <<"toeternity">>}}),
    ?assert(get_app_config("", "1234%%%%^@#$%@#$%#$^@$.appNOTFOUND", "testns", "http://666.666.666.666:666") == {404, ""}),
    meck:unload(httpabs).

get_app_preferences_test() ->
    FakeReturn = jiffy:encode({[{<<"welcome">>, <<"toeternity">>}]}),
    try meck:new(httpabs, [passthrough]) catch _:_ -> ok end,
    meck:expect(httpabs, get, fun(_XER, URL) ->
                                  case URL of
                                      "http://666.666.666.666:666/v3/namespaces/testns/apps/1234app/preferences"  -> {200, FakeReturn};
                                      "http://666.666.666.666:666/v3/namespaces/testns/apps/1234appNOTFOUND/preferences" -> {404, ""}
                                  end
                              end),
    ?assert(get_app_preferences("", "1234%%%%^@#$%@#$%#$^@$.app", "testns", "http://666.666.666.666:666") ==
             {200, #{<<"welcome">> => <<"toeternity">>}}),
    ?assert(get_app_preferences("", "1234%%%%^@#$%@#$%#$^@$.appNOTFOUND", "testns", "http://666.666.666.666:666") == {404, ""}),
    meck:unload(httpabs).

form_service_json_from_service_tuple_test() ->
    ?assert(form_service_json_from_service_tuple(<<"amazin@)#$%@#)$%gapp">>, "amazingns", "http://666.666.666.666:666", {"seeme", "feelme", "PUT"})
           == {[{<<"url">> ,<<"http://666.666.666.666:666/v3/namespaces/amazingns/apps/amazingapp/services/seeme/methods/feelme">>},
                {<<"method">>, "PUT"}]}).

form_stream_url_from_streamname_test() ->
    ?assert(form_stream_url_from_streamname("666.666.666.666:666", "thevoid", "souls") == <<"666.666.666.666:666/v3/namespaces/thevoid/streams/souls">>).

get_cdap_gui_port_from_version_test() ->
    ?assert(9999 == get_cdap_gui_port_from_version("3.0.0")),
    ?assert(9999 == get_cdap_gui_port_from_version("3.10.0")),
    ?assert(9999 == get_cdap_gui_port_from_version("3.0.10")),
    ?assert(9999 == get_cdap_gui_port_from_version("3.10.10")),
    ?assert(11011 == get_cdap_gui_port_from_version(<<"4.0.0">>)),
    ?assert(11011 == get_cdap_gui_port_from_version(<<"4.10.0">>)),
    ?assert(11011 == get_cdap_gui_port_from_version(<<"4.0.10">>)),
    ?assert(11011 == get_cdap_gui_port_from_version(<<"4.10.10">>)),
    ?assert(<<"UNKNOWN CDAP VERSION">> == get_cdap_gui_port_from_version("5.0.0")).

map_appname_test() ->
    ?assert(map_appname(<<"foo">>) == <<"foo">>),
    ?assert(map_appname(<<"fo.o">>) == <<"foo">>),
    ?assert(map_appname(<<"f_oo">>) == <<"foo">>),
    ?assert(map_appname(<<"._*#$%#*%$$#@#foo">>) == <<"foo">>).

get_cdap_cluster_version_test() ->
    FakeReturn = jiffy:encode({[{<<"version">>, <<"4.0.3">>}]}),
    try meck:new(httpabs, [passthrough]) catch _:_ -> ok end,
    meck:expect(httpabs, get, fun(_XER, _URL) -> {200, FakeReturn} end),
    ?assert(get_cdap_cluster_version("","") == <<"4.0.3">>),
    meck:expect(httpabs, get, fun(_XER, _URL) -> {404, ""} end),
    ?assert(get_cdap_cluster_version("","") == <<"UNKNOWN CDAP VERSION">>),
    meck:unload(httpabs).


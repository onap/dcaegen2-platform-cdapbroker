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

-module(util_tests).
-include_lib("eunit/include/eunit.hrl").
-import(util, [
               iso_elapsed/2,
               ip_to_str/1,
               update_with_new_config_map/2,
               ejson_to_map/1,
               to_str/1,
               get_platform_envs_and_config/0,
               resolve_cbs/2
              ]).

to_str_test() ->
    ?assert(to_str("") == ""),
    ?assert(to_str(<<"asdf">>) == "asdf").

iso_elapsed_test() ->
    ?assert(iso_elapsed(<<"2017-04-27T18:38:10Z">>, <<"2017-04-27T18:38:08Z">>) == 2),
    ?assert(iso_elapsed(<<"2017-04-29T18:38:10Z">>, <<"2017-04-27T18:38:08Z">>) == 60*60*24*2+2).

ip_to_str_test() ->
    ?assert(ip_to_str({{6,6,6,6}, 666}) == "6.6.6.6:666"),
    ?assert(ip_to_str({{196,196,196,196}, 1}) == "196.196.196.196:1"),
    ?assert(ip_to_str({{6,6,6,6666}, 666}) == invalid),
    ?assert(ip_to_str({{6,6,6,6}, 66666}) == invalid),
    ?assert(ip_to_str({{6,6,-6,6}, 666}) == invalid),
    ?assert(ip_to_str({{6,6,six,6}, 666}) == invalid).

update_with_new_config_map_test() ->
    ?assert(update_with_new_config_map(#{<<"foo">>=><<"smartbar">>,  <<"preffoo">>=><<"smartprefbar">>}, #{<<"foo">>=><<"bar">>})                         == #{<<"foo">>=><<"smartbar">>}),
    ?assert(update_with_new_config_map(#{<<"fooD">>=><<"smartbar">>},                                    #{<<"foo">>=><<"bar">>})                         == nooverlap),
    ?assert(update_with_new_config_map(#{<<"foo">>=><<"smartbar">>,<<"foo2">>=><<"smartbar2">>},         #{<<"foo">>=><<"bar">>, <<"foo2">>=><<"bar2">>}) == #{<<"foo">>=><<"smartbar">>, <<"foo2">>=><<"smartbar2">>}).

ejson_to_map_test() ->
    EJ1 = {[{<<"foo">>, <<"bar">>}, {<<"foo2">>, <<"bar2">>}]},
    EJ2 = {[{<<"foo2">>, <<"bar2">>}, {<<"foo">>, <<"bar">>}]},
    M1 = ejson_to_map(EJ1),
    M2 = ejson_to_map(EJ2),
    ?assert(EJ1 /= EJ2), %HERE LIES THE PROBLEM HUDSON
    ?assert(M1 == M2). %GREAT SUCCESS!

get_platform_envs_and_config_test() ->
    try meck:new(util, [passthrough]) catch _:_ -> ok end,
    try meck:new(consul_interface, [passthrough]) catch _:_ -> ok end,

    %test no envs
    meck:expect(util, get_envs, fun() -> {"", false, <<"cdap_test">>} end),
    ?assert(get_platform_envs_and_config() == []),

    %test good case where the env variable is passed
    %needed monkeypatching
    FakeConfig = {[
                   {<<"autoderegisterafter">>, <<"10m">>},
                   {<<"bindingttw">>,5},
                   {<<"pipelinehealthlimit">>, 2},
                   {<<"hcinterval">>, <<"60s">>}
                 ]},
    FakeBrokerName = "cdap_broker_test",
    FakeConsulName = "myconsuldotcom",
    meck:expect(util, get_envs, fun() -> {FakeBrokerName, FakeConsulName, <<"cdap_test">>} end),
    meck:expect(consul_interface, consul_bind_config, fun(_XER, _MyName, _ConsulURL) -> {200, FakeConfig} end),
    meck:expect(consul_interface, consul_get_service_ip_port, fun(_XER, Appname, _ConsulURL) ->
                                                                      case Appname of
                                                                          <<"cdap_test">> -> {"666.666.666.666", 666}
                                                                      end
                                                              end),

    ?assert(get_platform_envs_and_config() == [FakeBrokerName, "http://myconsuldotcom:8500", <<"http://666.666.666.666:666">>, jiffy:decode(jiffy:encode(FakeConfig), [return_maps])]),

    %test bad case where env is not passed
    meck:expect(util, get_envs, fun() -> {FakeBrokerName, FakeConsulName, false} end),
    FakeConfigwCDAP = {[
                   {<<"autoderegisterafter">>, <<"10m">>},
                   {<<"bindingttw">>,5},
                   {<<"pipelinehealthlimit">>, 2},
                   {<<"hcinterval">>, <<"60s">>},
                   {<<"cdap_cluster_to_manage">>, [<<"666.666.666.666:666">>]}
                 ]},
    meck:expect(consul_interface, consul_bind_config, fun(_XER, _MyName, _ConsulURL) -> {200, FakeConfigwCDAP} end),
    ?assert(get_platform_envs_and_config() == [FakeBrokerName, "http://myconsuldotcom:8500", <<"http://666.666.666.666:666">>, jiffy:decode(jiffy:encode(FakeConfigwCDAP), [return_maps])]),

    meck:unload(util),
    meck:unload(consul_interface).

resolve_cbs_test() ->
    try meck:new(consul_interface, [passthrough]) catch _:_ -> ok end,
    meck:expect(consul_interface, consul_get_service_ip_port, fun(_XER, Appname, _ConsulURL) ->
                                                                      case Appname of
                                                                          "config_binding_service" -> {"666.666.666.666", 10000}
                                                                      end
                                                               end),
    ?assert(resolve_cbs("", "") == "http://666.666.666.666:10000"),
    meck:unload(consul_interface).


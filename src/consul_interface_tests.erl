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

-module(consul_interface_tests).
-include_lib("eunit/include/eunit.hrl").

-import(consul_interface, [
          consul_get_service/3,
          consul_read_kv/3,
          consul_get_configuration/3,
          consul_get_preferences/3,
          consul_bind_config/3
        ]).

consul_bind_config_test() ->
    try meck:new(httpabs, [passthrough]) catch _:_ -> ok end,
    try meck:new(util, [passthrough]) catch _:_ -> ok end,
    FakeReturn = {[{<<"i am here">>, <<"my son">>}]},
    meck:expect(util, resolve_cbs, fun(_XER, _ConsulURL) -> "http://666.666.666.666:10000" end), %dont try to test the consul part here
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:10000/service_component/salvationnotfound" -> {404, ""};
                                  "http://666.666.666.666:10000/service_component/salvation" -> {200, jiffy:encode(FakeReturn)}
                              end end),
    ?assert(consul_bind_config("", "salvation", "") == {200, FakeReturn}),
    ?assert(consul_bind_config("", "salvationnotfound", "") == {404, ""}),
    meck:unload(httpabs),
    meck:unload(util).

consul_read_kv_test() ->
    try meck:new(httpabs, [passthrough]) catch _:_ -> ok end,
    Val = base64:encode("salvation"),
    FakeReturn = jiffy:encode([{[{<<"Value">>, Val}]}]),
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:666/v1/kv/salvationnotfound" -> {404, ""};
                                  "http://666.666.666.666:666/v1/kv/salvation" -> {200, FakeReturn}
                              end end),
    ?assert(consul_read_kv("", <<"salvation">>, "http://666.666.666.666:666") == {200, <<"salvation">>}),
    ?assert(consul_read_kv("", <<"salvationnotfound">>, "http://666.666.666.666:666") == {404, ""}),
    meck:unload(httpabs).

consul_get_configuration_preferences_test() ->
    try meck:new(httpabs, [passthrough]) catch _:_ -> ok end,
    Val = base64:encode(jiffy:encode({[{<<"iam">>, <<"thebestestjson">>}]})),
    FakeReturn = jiffy:encode([{[{<<"Value">>, Val}]}]),
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:666/v1/kv/salvationnotfound" -> {404, ""};
                                  "http://666.666.666.666:666/v1/kv/salvation" -> {200, FakeReturn};
                                  "http://666.666.666.666:666/v1/kv/salvation:preferences" -> {200, FakeReturn};
                                  "http://666.666.666.666:666/v1/kv/salvationnotfound:preferences" -> {404, ""}
                              end end),
    ?assert(consul_get_configuration("", <<"salvation">>, "http://666.666.666.666:666") == {200, #{<<"iam">>=><<"thebestestjson">>}}),
    ?assert(consul_get_configuration("", <<"salvationnotfound">>, "http://666.666.666.666:666") == {404, ""}),
    ?assert(consul_get_preferences("", <<"salvation">>, "http://666.666.666.666:666") == {200, #{<<"iam">>=><<"thebestestjson">>}}),
    ?assert(consul_get_preferences("", <<"salvationnotfound">>, "http://666.666.666.666:666") == {404, ""}),

     meck:unload(httpabs).

consul_get_service_test() ->
    try meck:new(httpabs, [passthrough]) catch _:_ -> ok end,
    FakeConfig = jiffy:encode({[{<<"your key">>, <<"sir">>}]}),
    meck:expect(httpabs, get, fun(_XER, URL) -> case URL of
                                  "http://666.666.666.666:666/v1/catalog/service/salvationnotfound" -> {404, ""};
                                  "http://666.666.666.666:666/v1/catalog/service/salvation" -> {200, FakeConfig}
                              end end),
    ?assert(consul_get_service("", <<"salvation">>, "http://666.666.666.666:666") == #{<<"your key">> => <<"sir">>}),
    ?assertException(error, {badmatch,{404,[]}}, consul_get_service("", <<"salvationnotfound">>, "http://666.666.666.666:666")),
    meck:unload(httpabs).





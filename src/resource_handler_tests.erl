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

-module(resource_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("application.hrl").
-import(resource_handler, [
                            parse_put_body/1,
                            parse_reconfiguration_put_body/1,
                            handle_reconfigure_put/5,
                            handle_put/6,
                            handle_post_multidelete_app/4
                          ]).

put_test() ->
    Valid = {[
         {<<"cdap_application_type">>, <<"program-flowlet">>},
         {<<"namespace">>,         <<"ns">>},
         {<<"streamname">>,        <<"sn">>},
         {<<"jar_url">>,           <<"www.foo.com">>},
         {<<"artifact_name">>,     <<"art_name">>},
         {<<"artifact_version">>,  <<"art_ver">>},
         {<<"app_config">>,        {[{<<"foo">>,<<"bar">>}]}},
         {<<"app_preferences">>,   {[{<<"foop">>,<<"barp">>}]}},
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
                                        {<<"program_pref">>, {[{<<"foopprog">>,<<"barpprog">>}]}}]}
                                      ]}
         ]},
    %{Namespace, Streamname, JarURL, ArtifactName, ArtifactVersion, AppConfig, AppPreferences, ParsedServices, Programs, ParsedProgramPreferences}
    ExpectedL = {<<"ns">>, <<"sn">>, <<"www.foo.com">>, <<"art_name">>, <<"art_ver">>,
                 #{<<"foo">>=><<"bar">>},
                 #{<<"foop">>=><<"barp">>},
                [{<<"Greeting">>,<<"greet">>,<<"GET">>}],
                [#program{type = <<"flows">>, id = <<"WhoFlow">>}, #program{type = <<"services">>, id = <<"Greeting">>}],
                [{<<"flows">>,<<"WhoFlow">>,#{<<"foopprog">>=><<"barpprog">>}}]},
    Expected = {<<"program-flowlet">>, ExpectedL},
    ?assert(parse_put_body(jiffy:encode(Valid)) == Expected),

    ValidHydrator1 =
            {[
              {<<"cdap_application_type">>, <<"hydrator-pipeline">>},
              {<<"namespace">>, <<"ns">>},
              {<<"streamname">>, <<"sn">>},
              {<<"pipeline_config_json_url">>, "www.foo.com"}
            ]},
    ExpectedHy1 = {<<"hydrator-pipeline">>, {<<"ns">>,<<"sn">>,"www.foo.com",[]}},
    ?assert(parse_put_body(jiffy:encode(ValidHydrator1)) == ExpectedHy1),

    ValidHydrator2 =
            {[
              {<<"cdap_application_type">>, <<"hydrator-pipeline">>},
              {<<"namespace">>, <<"ns">>},
              {<<"streamname">>, <<"sn">>},
              {<<"pipeline_config_json_url">>, "www.foo.com"},
              {<<"dependencies">>, [
                                    {[
                                       {<<"artifact_extends_header">>, <<"system:cdap-data-pipeline[4.1.0,5.0.0)">>},
                                       {<<"artifact_name">>,           <<"art carney">>},
                                       {<<"artifact_version_header">>, <<"1.0.0-SNAPSHOT">>},
                                       {<<"artifact_url">>,            <<"www.foo.com/sup/baphomet.jar">>},
                                       {<<"ui_properties_url">>,       <<"www.foo2.com/sup/baphomet.jar">>}
                                    ]}
                                   ]}
            ]},
    %{hp, <<"hydrator-pipeline">>, {Namespace, Streamname, PipelineConfigJsonURL, ParsedDependencies}}
    ExpectedHy2 = {<<"hydrator-pipeline">>, {<<"ns">>,<<"sn">>,"www.foo.com",[{"system:cdap-data-pipeline[4.1.0,5.0.0)",<<"art carney">>,"1.0.0-SNAPSHOT",<<"www.foo.com/sup/baphomet.jar">>,<<"www.foo2.com/sup/baphomet.jar">>}]}},
    ?assert(parse_put_body(jiffy:encode(ValidHydrator2)) == ExpectedHy2),

    %Test the unexpected cases
    EmptyD = dict:new(),
    try meck:new(resource_handler, [passthrough]) catch _:_ -> ok end,
    meck:expect(resource_handler, appname_to_field_vals, fun(X, [<<"appname">>]) ->
                                                            case X of
                                                                <<"notexist">> -> none;
                                                                <<"exist">> -> [<<"exist">>]
                                                            end
                                                         end),


    %check already exists
    ?assert(handle_put("", EmptyD, "textxer", <<"exist">>, Valid, "www.validurl.com") == {400, "Put recieved on /application/:appname but appname is already registered. Call /application/:appname/reconfigure if trying to reconfigure or delete first", EmptyD}),

    InvalidType = jiffy:encode({[{<<"cdap_application_type">>, <<"NOT TODAY">>}]}),
    ?assert(parse_put_body(InvalidType) == unsupported),
    ?assert(handle_put("", EmptyD, "textxer", <<"notexist">>, InvalidType, "www.validurl.com") == {400,"Unsupported CDAP Application Type", EmptyD}),

    InvalidMissing = {[
         {<<"cdap_application_type">>, <<"program-flowlet">>},
         {<<"namespace">>,         <<"ns">>}
         ]},
    ?assert(parse_put_body(jiffy:encode(InvalidMissing)) == invalid),
    ?assert(handle_put("", EmptyD, "textxer", <<"notexist">>, InvalidMissing, "www.validurl.com") == {400, "Invalid PUT Body or unparseable URL", EmptyD}),

    InvalidMissing2 = {[{<<"malformed">>, <<"i am">>}]},
    ?assert(parse_put_body(jiffy:encode(InvalidMissing2)) == invalid),
    ?assert(handle_put("", EmptyD, "textxer", <<"notexist">>, InvalidMissing2, "www.validurl.com") == {400, "Invalid PUT Body or unparseable URL", EmptyD}),

    meck:unload(resource_handler).


reconfiguration_put_test() ->
    %test reconfiguring with an invalid PUT body (missing "reconfiguration_type")
    EmptyD = dict:new(),
    try meck:new(resource_handler, [passthrough]) catch _:_ -> ok end,
    meck:expect(resource_handler, appname_to_field_vals,  fun(X, _) ->
                                                              case X of
                                                                 <<"notexist">> -> none;
                                                                 <<"exist">> -> [<<"ns">>]
                                                              end
                                                           end),

    I1 = jiffy:encode({[{<<"config">>, <<"bar">>}]}),
    ?assert(parse_reconfiguration_put_body(I1) == invalid),
    ?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"exist">>, I1) == {400,"Invalid PUT Reconfigure Body",EmptyD}),

    %test reconfiguring with an invalid PUT body (missing app_config)
    I2 = jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-app-config">>}, {<<"foo">>, <<"bar">>}]}),
    ?assert(parse_reconfiguration_put_body(I2) == invalid),
    ?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"exist">>, I2) == {400,"Invalid PUT Reconfigure Body",EmptyD}),

    %test reconfiguring an invalid (unimplemented) type
    I3 = jiffy:encode({[{<<"config">>, <<"bar">>}, {<<"reconfiguration_type">>, <<"EMPTINESS">>}]}),
    ?assert(parse_reconfiguration_put_body(I3) == notimplemented),
    ?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"exist">>, I3) == {501,"This type of reconfiguration is not implemented",EmptyD}),

    Valid = jiffy:encode({[{<<"config">>, {[{<<"foo">>, <<"bar">>}]}}, {<<"reconfiguration_type">>,<<"program-flowlet-app-config">>}]}),
    ?assert(parse_reconfiguration_put_body(Valid) == {<<"program-flowlet-app-config">>,#{<<"foo">>=><<"bar">>}}),

    %no unit test for handle_reconfigure_put yet

    %test for valid but missing
    %?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"exist">>, I3, AppnameToNS) == {501,"This type of reconfiguration is not implemented",EmptyD}),
    ?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"notexist">>, Valid) == {404,"Reconfigure recieved but the app is not registered", EmptyD}),

    meck:unload(resource_handler).

delete_test() ->
    EmptyD = dict:new(),

    %test failure: appnames missing
    Invalid1 = jiffy:encode({[{<<"ids">>, [<<"hwtest">>]}]}),
    ?assert(handle_post_multidelete_app(notused, EmptyD, notused, Invalid1) == {400,"Invalid PUT Body", EmptyD}),

    %test invalid: not a list
    Invalid2 = jiffy:encode({[{<<"appnames">>, <<"hwtest">>}]}),
    ?assert(handle_post_multidelete_app(notused, EmptyD, notused, Invalid2) == {400,"Invalid PUT Body", EmptyD}),

    %mock out delete_app_helper(X, State, XER, Req)
    meck:new(resource_handler, [passthrough]),
    meck:expect(resource_handler,
                delete_app_helper,
                fun(Appname, State, _XER, _Req) ->
                   case Appname of
                       <<"noexist">> -> {404, "Tried to delete an application that was not registered", State};
                       <<"exist">>   -> {200, "", State}
                   end
                end),
    %est empty
    Empty = jiffy:encode({[{<<"appnames">>, []}]}),
    ?assert(handle_post_multidelete_app(notused, EmptyD, notused, Empty) == {200, "EMPTY PUT BODY", EmptyD}),

    %test one app that is registered (in the mock...) and one app that is not regustered
    Valid = jiffy:encode({[{<<"appnames">>, [<<"exist">>, <<"noexist">>]}]}),
    ?assert(handle_post_multidelete_app(notused, EmptyD, notused, Valid) == {200, <<"[200,404]">>, EmptyD}),

    meck:unload(resource_handler).

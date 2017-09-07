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
                            handle_reconfigure_put/6
                          ]).

parse_put_body_test() ->
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
    Expected = {pf, <<"program-flowlet">>, ExpectedL},
    ?assert(parse_put_body(jiffy:encode(Valid)) == Expected),

    ValidHydrator1 =
            {[
              {<<"cdap_application_type">>, <<"hydrator-pipeline">>},
              {<<"namespace">>, <<"ns">>},
              {<<"streamname">>, <<"sn">>},
              {<<"pipeline_config_json_url">>, "www.foo.com"}
            ]},
    ExpectedHy1 = {hp,<<"hydrator-pipeline">>,{<<"ns">>,<<"sn">>,"www.foo.com",[]}},
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
    ExpectedHy2 = {hp,<<"hydrator-pipeline">>,{<<"ns">>,<<"sn">>,"www.foo.com",[{"system:cdap-data-pipeline[4.1.0,5.0.0)",<<"art carney">>,"1.0.0-SNAPSHOT",<<"www.foo.com/sup/baphomet.jar">>,<<"www.foo2.com/sup/baphomet.jar">>}]}},
    ?assert(parse_put_body(jiffy:encode(ValidHydrator2)) == ExpectedHy2),

    InvalidType = {[{<<"cdap_application_type">>, <<"NOT TODAY">>}]},
    erlang:display(parse_put_body(jiffy:encode(InvalidType))),
    ?assert(parse_put_body(jiffy:encode(InvalidType)) == unsupported),

    InvalidMissing = {[
         {<<"cdap_application_type">>, <<"program-flowlet">>},
         {<<"namespace">>,         <<"ns">>}
         ]},
    ?assert(parse_put_body(jiffy:encode(InvalidMissing)) == invalid).

reconfiguration_put_test() ->
    %test reconfiguring with an invalid PUT body (missing "reconfiguration_type")
     AppnameToNS = fun(X) ->
                             case X of
                                <<"notexist">> -> none;
                                <<"exist">> -> <<"ns">>
                             end
                          end,
    EmptyD = dict:new(),

    I1 = jiffy:encode({[{<<"config">>, <<"bar">>}]}),
    ?assert(parse_reconfiguration_put_body(I1) == invalid),
    ?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"exist">>, I1, AppnameToNS) == {400,"Invalid PUT Reconfigure Body",EmptyD}),

    %test reconfiguring with an invalid PUT body (missing app_config)
    I2 = jiffy:encode({[{<<"reconfiguration_type">>, <<"program-flowlet-app-config">>}, {<<"foo">>, <<"bar">>}]}),
    ?assert(parse_reconfiguration_put_body(I2) == invalid),
    ?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"exist">>, I2, AppnameToNS) == {400,"Invalid PUT Reconfigure Body",EmptyD}),

    %test reconfiguring an invalid (unimplemented) type
    I3 = jiffy:encode({[{<<"config">>, <<"bar">>}, {<<"reconfiguration_type">>, <<"EMPTINESS">>}]}),
    ?assert(parse_reconfiguration_put_body(I3) == notimplemented),
    ?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"exist">>, I3, AppnameToNS) == {501,"This type of reconfiguration is not implemented",EmptyD}),

    Valid = jiffy:encode({[{<<"config">>, {[{<<"foo">>, <<"bar">>}]}}, {<<"reconfiguration_type">>,<<"program-flowlet-app-config">>}]}),
    ?assert(parse_reconfiguration_put_body(Valid) == {<<"program-flowlet-app-config">>,#{<<"foo">>=><<"bar">>}}),

    %no unit test for handle_reconfigure_put yet

    %test for valid but missing
    %?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"exist">>, I3, AppnameToNS) == {501,"This type of reconfiguration is not implemented",EmptyD}),
    ?assert(handle_reconfigure_put("", EmptyD, "testXER", <<"notexist">>, Valid, AppnameToNS) == {404,"Reconfigure recieved but the app is not registered", EmptyD}).


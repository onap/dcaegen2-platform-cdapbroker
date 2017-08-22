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

get_cdap_gui_port_from_version_test() ->
    ?assert(9999 == cdap_interface:get_cdap_gui_port_from_version("3.0.0")),
    ?assert(9999 == cdap_interface:get_cdap_gui_port_from_version("3.10.0")),
    ?assert(9999 == cdap_interface:get_cdap_gui_port_from_version("3.0.10")),
    ?assert(9999 == cdap_interface:get_cdap_gui_port_from_version("3.10.10")),
    ?assert(11011 == cdap_interface:get_cdap_gui_port_from_version(<<"4.0.0">>)),
    ?assert(11011 == cdap_interface:get_cdap_gui_port_from_version(<<"4.10.0">>)),
    ?assert(11011 == cdap_interface:get_cdap_gui_port_from_version(<<"4.0.10">>)),
    ?assert(11011 == cdap_interface:get_cdap_gui_port_from_version(<<"4.10.10">>)),
    ?assert(<<"UNKNOWN CDAP VERSION">> == cdap_interface:get_cdap_gui_port_from_version("5.0.0")).


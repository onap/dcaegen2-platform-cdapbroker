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

-module(workflows_tests).
-include_lib("eunit/include/eunit.hrl").

all_200s_else_showerror_test() ->
    ?assert({200, ""} == workflows:all_200s_else_showerror(fun(_) -> {200, "all good"} end, [1,"A", foo])),
    ?assert({500, "constant dissapointment"} ==  workflows:all_200s_else_showerror(fun(X) -> if X < 5 -> {200, "all good"}; true -> {500, "constant dissapointment"} end end, [0,10])).


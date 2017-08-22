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

-record(program, {type :: binary(), id :: binary()}).
-type lprogram() :: [#program{}].
-type status_code() :: 100..101 | 200..206 | 300..307 | 400..417 | 500..505. %stolen from leptus
-type httpstat() :: {status_code(), string()}.
-record(application,    {appname, apptype,  namespace, healthcheckurl, metricsurl, url, connectionurl, serviceendpoints, creationtime}).
-record(prog_flow_supp, {appname, programs::lprogram()}). 



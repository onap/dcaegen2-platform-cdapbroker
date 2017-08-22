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

%%%-------------------------------------------------------------------
%% @doc cdapbroker public API
%% @end
%%%-------------------------------------------------------------------

-module(cdapbroker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%for application state
-include("application.hrl").
-define(SC(L), util:concat(L)).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %starting inets to make request calls
    inets:start(),
    %from the HTTPC page: 
    %"If the scheme https is used, the SSL application must be started. 
    ssl:start(),

    %EELF hello
    audit:info("Audit log initlized"),
    metrics:info("Metrics log initlized"),
    error:info("Error log initlized"),

    %fail fast; check all failure conditions first
    PE = util:get_platform_envs_and_config(),
    case PE of 
        [] -> %crash and burn. Need to exit else supervisor will restart this endlessly.  
          exit("fatal error, either HOSTNAME or CONSUL_HOST or CONFIG_BINDING_SERVICE is not set as an env variable");
        [_, ConsulURL, CDAPUrl, BoundConfigMap] ->
          try
              S = dict:new(),
              S1 = dict:store("consulurl", ConsulURL, S),
              S2 = dict:store("configmap", BoundConfigMap, S1),
              State = dict:store("cdapurl", CDAPUrl, S2),
              
              %initialize database
              ok = util:initialize_database(),

              %print out currently registered apps at startup
              lager:info("Currently installed apps: ~p~n", [util:get_all_appnames_from_db()]),
              
              %start the REST server
              leptus:start_listener(http, [{'_', [{resource_handler, State}]}], [{port, 7777}, {ip, {0,0,0,0}}]),
                      
              %start the supervisor
              lager:info("Starting supervisor"),
              cdapbroker_sup:start_link()
        catch Class:Reason ->
            lager:error("~nError Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
            exit(Reason)
        end
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    %need to make sure there are no pending transcations in RAM not written to disk yet on shutdown.
    %Two hard problems in CS, this is one of then...
    lager:info("Stop recieved."),
    case mnesia:sync_log() of 
        ok -> ok;
        {error, Reason} ->
            lager:error(io_lib:format("While stopping, MNESIA could not by syncd due to: ~s. This means on bootup the database may be in a bad state!!", [Reason])),
            notok
    end.


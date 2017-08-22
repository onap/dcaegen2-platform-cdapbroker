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

-module(consul_interface).
-export([consul_register/8, 
         consul_deregister/3, 
         consul_get_configuration/3, 
         consul_get_preferences/3,
         consul_get_service_ip_port/3,
         consul_push_config/4,
         consul_push_preferences/4,
         consul_bind_config/3,
         consul_delete_config/3,
         consul_delete_preferences/3
        ]).

-define(SC(L), util:concat(L)).
-define(PrefKey(Appname), ?SC([Appname, ":preferences"])).

%the Erlang library linked on the Consul webpage does not seem to be equivelent to python-consul. It does something else: https://github.com/undeadlabs/discovery
%and this one seems to be just elixer: https://github.com/undeadlabs/consul-ex
%so, for now, I'm just doing some HTTP calls directly against the REST API. 

consul_get_service(XER, Appname, ConsulURL) ->
    %returns a list of maps
    URL = ?SC([ConsulURL, "/v1/catalog/service/", Appname]),
    {200, ReturnBody} = httpabs:get(XER, URL),
    jiffy:decode(ReturnBody, [return_maps]).

consul_write_kv(XER, Key, Value, ConsulURL) ->
    %generic helper function to write a value into Consul
    URL = ?SC([ConsulURL,"/v1/kv/", Key]),
    httpabs:put(XER, URL, "application/json", Value).

consul_read_kv(XER, Key, ConsulURL) ->
    %does a get on the KV store, see https://www.consul.io/docs/agent/http/kv.html
    %Appname MUST be a binary not a string!
    URL = ?SC([ConsulURL, "/v1/kv/", Key]),
    {RC, RB} = httpabs:get(XER, URL),
    case RC of 
        200 -> 
            [Drb] = jiffy:decode(RB,  [return_maps]),
            {200, base64:decode(maps:get(<<"Value">>, Drb))}; %NOTE! Does not do a JSON decode here in case you want to read non-JSON from Consul. Leaves the decode to the caller.
        _ ->
            {RC, RB}
    end.

consul_delete_kv(XER, Key, ConsulURL) ->
    %the config has not been jiffy'd prior to this function
    URL = ?SC([ConsulURL,"/v1/kv/", Key]),
    httpabs:delete(XER, URL).

%%%%%%%%%%%%%%%%%
%PUBLIC FUNCTIONS
%%%%%%%%%%%%%%%%%

consul_register(XER, Appname, ConsulURL, SDIP, SDPort, HealthURL, HCInterval, AutoDeregisterAfter) ->
    %uses the agent api to register this app as a service and it's healthcheck URL, which is this broker as a proxy.
    %/v1/agent/service/register from https://www.consul.io/docs/agent/http/agent.html#agent_service_register
    URL = ?SC([ConsulURL, "/v1/agent/service/register"]),
    Body = jiffy:encode(
     {[{<<"Name">>, Appname},
       {<<"Address">>, SDIP},
       {<<"Port">>, SDPort},
       {<<"Check">> , 
           {[{<<"HTTP">>, HealthURL}, {<<"Interval">>, HCInterval}, {<<"DeregisterCriticalServiceAfter">>, AutoDeregisterAfter}]}
       }
     ]}),
    httpabs:put(XER, URL, "application/json", Body).

consul_deregister(XER, Appname, ConsulURL) ->
    %/v1/agent/service/deregister/<serviceId> from https://www.consul.io/docs/agent/http/agent.html#agent_service_register
    URL = ?SC([ConsulURL, "/v1/agent/service/deregister/", Appname]),
    httpabs:put(XER, URL, "application/json", ""). %kinda weird that this isnt a DELETE

consul_get_configuration(XER, Appname, ConsulURL) ->
    %fetch configuration from consul, assumes it is a json
    %returns it as a map. can be encoded again.
    {RC, RB} = consul_read_kv(XER, Appname, ConsulURL),
    case RC of 
        200 -> {200, jiffy:decode(RB, [return_maps])}; %configuration is a JSON
        _   -> {RC, RB}
    end.

consul_get_preferences(XER, Appname, ConsulURL) ->
    %This function is currently only used in testing and is not used in resource_handler, but could be useful later
    %returns it as a map. can be encoded again.
    {RC, RB} = consul_read_kv(XER, ?PrefKey(Appname), ConsulURL),
    case RC of 
        200 -> {200, jiffy:decode(RB, [return_maps])}; %configuration is a JSON
        _   -> {RC, RB}
    end.

consul_get_service_ip_port(XER, Appname, ConsulURL) ->
    %use when you are expecting consul_get_service to return a list of exactly one service and all you want is ip:port
    M = lists:nth(1, consul_get_service(XER, Appname, ConsulURL)),
    {maps:get(<<"ServiceAddress">>, M), maps:get(<<"ServicePort">>, M)}.

consul_push_config(XER, Appname, ConsulURL, Config) ->
    %pushes Config into Consul under the key "Appname". 
    %TODO: Possibly this should be under "Appname:config" to be consistent with preferences but this came first and that's an invasive change.
    %the config has not been jiffy'd prior to this function
    consul_write_kv(XER, Appname, jiffy:encode(Config), ConsulURL).

consul_push_preferences(XER, Appname, ConsulURL, Preferences) ->
    %pushes the preferences into Consul under the key "Appname:preferences"
    %the config has not been jiffy'd prior to this function
    consul_write_kv(XER, ?PrefKey(Appname), jiffy:encode(Preferences), ConsulURL).

consul_delete_config(XER, Appname, ConsulURL) ->
    consul_delete_kv(XER, Appname, ConsulURL).

consul_delete_preferences(XER, Appname, ConsulURL) ->
    consul_delete_kv(XER, ?PrefKey(Appname), ConsulURL).

consul_bind_config(XER, Appname, ConsulURL) ->
    URL = ?SC([util:resolve_cbs(XER, ConsulURL), "/service_component/", Appname]),
    {ReturnCode, ReturnBody} = httpabs:get(XER, URL),
    case ReturnCode of 
       200 -> {200, jiffy:decode(ReturnBody)};
       _ -> {ReturnCode, ReturnBody} %do not try to decode if not correct
    end.

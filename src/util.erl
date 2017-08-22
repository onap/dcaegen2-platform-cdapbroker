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

-module(util).
-include("application.hrl").

-export([concat/1, 
         get_platform_envs_and_config/0, 
         resolve_cbs/2,
         initialize_database/0,
         get_all_appnames_from_db/0,
         get_my_version/0,
         get_programs_for_pfapp_from_db/1,
         gen_uuid/0,
         iso/0,
         iso_elapsed/2,
         to_str/1,
         ip_to_str/1,
         update_with_new_config_map/2,
         ejson_to_map/1
        ]).

%http://stackoverflow.com/questions/39757020/erlang-drying-up-stringbinary-concatenation
%NOTE! Does not work or bomb when an element in the list is an atom. Must be a string or binary. Maybe add a check for this
to_string(Value) when is_binary(Value) -> binary_to_list(Value);
to_string(Value) -> Value.
concat(List) ->
  lists:flatten(lists:map(fun to_string/1, List)).

resolve_cbs(XER, ConsulURL) ->
    %Ideally this function would dissapear if we get real DNS. This essentially is doing an SRV record lookup every time someone needs the bindng URL
    %This allows the broker to handle the case where the CBS moves IP or Ports
    %New as of 6/28/17: Uses the hardcoded short name for the CBS
     {IP, Port} = consul_interface:consul_get_service_ip_port(XER, "config_binding_service", ConsulURL),
     concat(["http://", IP, ":", integer_to_binary(Port)]).

get_platform_envs_and_config() ->
    %Get platform envs needed for broker operation, then fetch my config. 
    %If something critical fails, returns [], else [ConsulURL, CDAPUrl, BoundConfigMap]
    MyName = os:getenv("HOSTNAME"),
    ConsulHost = os:getenv("CONSUL_HOST"),
    case MyName == false orelse ConsulHost == false of 
        true -> [];
        false -> 
            %build Consul URL
            ConsulURL = concat(["http://", ConsulHost, ":8500"]),
            
            %Bind my own config map
            %generate my own XER here
            XER = gen_uuid(),
            {200, BoundConfig} = consul_interface:consul_bind_config(XER, MyName, ConsulURL),
            BoundConfigMap = jiffy:decode(jiffy:encode(BoundConfig), [return_maps]), %kind of an interesting way to turn an erlang proplist into a map
            
            %Here, we waterfall looking for "CDAP_CLUSTER_TO_MANAGE".
            %First, we will check for environmnental variables for a cluster *NAME*
            %If that is not found, then we will check out bound config for a fully bound URL
            %If that is also not found, let it crash baby.
            CDAPURL = case os:getenv("CDAP_CLUSTER_TO_MANAGE") of 
                 false -> 
                    list_to_binary(concat(["http://", lists:nth(1, maps:get(<<"cdap_cluster_to_manage">>, BoundConfigMap))])); %cbs returns ip:port. need http:// or will get "no adaptors found" error
                 CDAPName -> 
                    {IP, Port} = consul_interface:consul_get_service_ip_port(XER, CDAPName, ConsulURL),
                    list_to_binary(concat(["http://", IP, ":", integer_to_binary(Port)]))
                end,
            [MyName, ConsulURL, CDAPURL, BoundConfigMap]
    end.

initialize_database() ->
    %Create the database (currently MNesia) if it does not exist, and the application table.
    %Or, do nothing. 
    N = node(),
    lager:info(io_lib:format("Initializing database. My node name is ~s", [N])),

    %set MNesia dir
    application:set_env(mnesia, dir, "/var/mnesia/"),
    
    %stop if running, can't create schema if it is. Dont check status, OK if stopped
    mnesia:stop(),

    %create the schema if it does not already exist. Dont check status, ok if exists
    %erlang:display(mnesia:delete_schema([N])),
    mnesia:create_schema([N]),
    %start MNesia, assert it works
    
    ok = mnesia:start(), %start MNesia, bomb if alreay started, should not happen
    lager:info("Mnesia started"),
    
    %try to create the table, or if it exists, do nothing
    %erlang:display(mnesia:delete_table(application)),
    case mnesia:create_table(application,  [{attributes, record_info(fields, application)}, {disc_copies, [N]}]) of
        {aborted,{already_exists,application}} ->
            lager:info("Application table already exists");
        {atomic,ok} -> 
            lager:info(io_lib:format("Created application table on ~s", [N]))
    end,

    %try to create the app supplementaty table, or if it exists, do nothing
    %erlang:display(mnesia:delete_table(application)),
    case mnesia:create_table(prog_flow_supp,  [{attributes, record_info(fields, prog_flow_supp)}, {disc_copies, [N]}]) of
        {aborted,{already_exists, prog_flow_supp}} ->
            lager:info("prog_flow_supp table already exists");
        {atomic,ok} -> 
            lager:info(io_lib:format("Created prog_flow_supp table on ~s", [N]))
    end,

    %wait up to 30s for the table to come up. Usually instantaneous. If it takes more crash abd burn
    ok = mnesia:wait_for_tables([application, prog_flow_supp], 30000),
    ok.
     
get_all_appnames_from_db() ->
    {atomic, Apps} = mnesia:transaction(fun() -> mnesia:match_object(application, #application{_ = '_'}, read) end),
    lists:map(fun(X) -> {application, Appname,_,_,_,_,_,_,_,_} = X,
                        Appname
              end, Apps).

-spec get_programs_for_pfapp_from_db(binary()) -> lprogram().
get_programs_for_pfapp_from_db(Appname) ->
    {atomic, [#prog_flow_supp{appname = Appname, programs=Programs}]} = mnesia:transaction(fun() -> mnesia:match_object(prog_flow_supp, #prog_flow_supp{appname = Appname, _ = '_'}, read) end),
    Programs.

get_my_version() ->
    %stolen from the SO post I asked about: http://stackoverflow.com/questions/43147530/erlang-programmatically-get-application-version/43152182#43152182
    case lists:keyfind(cdapbroker, 1, application:loaded_applications()) of
       {_, _, Ver} -> list_to_binary(Ver);
       false       -> <<"error">>
    end.

gen_uuid() ->
    %generate an RFC compliant v1 uuid using lib
    uuid:to_string(uuid:uuid1()).

iso() ->
    %generate 8601 ts
    iso8601:format(erlang:timestamp()).

iso_elapsed(Endtime, Starttime) ->
    %%%...subtract two isos and return the number of seconds elapsed between Starttime and Endtime
    Edt = iso8601:parse(Endtime),
    Sdt = iso8601:parse(Starttime),
    Egs = calendar:datetime_to_gregorian_seconds(Edt),
    Sgs = calendar:datetime_to_gregorian_seconds(Sdt),
    Egs - Sgs.

to_str("") -> "";
to_str(Term) -> lists:flatten(io_lib:format("~p", [Term])).

-spec ip_to_str({inet:ip_address(), inet:port_number()}) -> binary().
%nasty.. I miss pythons x <= Foo <= Y syntax.. or something mathematical like Foo in [X..Y].. erlang not good 4 math
ip_to_str({{A,B,C,D}, Port}) when A >= 0 andalso A =< 255 andalso B >= 0 andalso B =< 255 andalso C >= 0 andalso C =< 255 andalso D >= 0 andalso D =< 255 andalso port >= 0 andalso Port =<65535 ->
    concat([to_str(A),".",to_str(B),".", to_str(C),".",to_str(D),":",to_str(Port)]);
ip_to_str({_,_}) -> invalid.

update_with_new_config_map(NewConfig, OldConfig) ->
    %helper for smart_reconfigure, broken out so we can unit test it.
    %
    %Takes in a new config, some keys in which may be shared with OldConfig, and returns a new map with the same keys as OldConfig, except values that had overlap were replaced by NewConfig
    %if no keys in NewConfig overlap with OldConfig, returns the atom 'nooverlap'
    %
    %This is very similar to the maps:merge/2 builtin but that will inject keys of newconfig that were not in oldconfig. We need a "RIGHT JOIN"
    NCKeys = maps:keys(NewConfig),
    ConfigOverlaps = [X || X <- NCKeys, maps:is_key(X, OldConfig)],
    case ConfigOverlaps of 
        [] -> nooverlap;
        _ -> 
            %we have an entry that should be in app config
            %build a new map with just the keys to update
            Pred = fun(X,_) -> lists:member(X, ConfigOverlaps) end,
            NewVals = maps:filter(Pred, NewConfig),
            maps:merge(OldConfig, NewVals)
    end.

ejson_to_map(E) ->
    %takes the jiffy "ejson: format of {[{<<"foo">>, <<"bar">>}, {<<"foo2">>, <<"bar2">>}]} and turns it into a map, 
    %usefu because ejsons do not appear to be order-independent-comparable, but maps are (e.g., two maps are equal if all their k+v are equal but agnostic to order)
    jiffy:decode(jiffy:encode(E), [return_maps]).

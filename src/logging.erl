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

-module(logging).
-export([
         audit/3,
         metrics/3,
         err/2
        ]).

-import(util, [iso/0, iso_elapsed/2, to_str/1, ip_to_str/1]).
%..lazy macros
-define(SC(L), util:concat(L)).
-define(PV(Name, PL),  proplists:get_value(Name, PL, "")).

%This module is intended to support the logging format standard for ONAP/ECOMP components. SOmetimes that is reffered to as "EELF".

%levels are none | debug | info | notice | warning | error | critical | alert | emergency.

%%%
%%%Helper functions
%%%
pid() ->  pid_to_list(self()).
%they wanted milleseconds but they are getting seconds rounded to ms because I don't have an erlang BIF that gives me this
elapsed(Endtime, Starttime) -> to_str(iso_elapsed(Endtime, Starttime)*1000).

start_end_elapsed(ArgPropl) ->
    %returns start time, end time,... and elapsed time
    EndT = iso(),
    StartT = ?PV(bts, ArgPropl),
    ElapT = elapsed(EndT, StartT), 
    {StartT, EndT, ElapT}.

%things this logging class can compute based on the Req so need not be in every logging function
server_add(Req) ->
  {MyUrl, _} = cowboy_req:host_url((leptus_req:get_req(Req))),
  %discard HTTP portion using Erlang binary matching
  <<_:7/binary, URL/binary>> = MyUrl,
  URL.

ip(Req) -> ip_to_str(leptus_req:peer(Req)).

path(Req) ->
    %get us the method and API path that was hit from the request
    {Path, {_,_,_,_,_,Method,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}} = cowboy_req:path(leptus_req:get_req(Req)),
    ?SC([Method, " ", Path]).

%%%
%%%External
%%%

audit(Sev, Req, ArgPropl) ->
    F = case Sev of 
        info    -> fun(M)->audit:info(M) end; %cant use the shorthand "fun -> audit:info/2" because of parse_transform
        warning -> fun(M)->audit:warning(M) end
    end,
    %The audit field list:
    %
    %1 BeginTimestamp                    Implemented (bts)
    %2 EndTimestamp                      Auto Injected when this is called
    %3 RequestID                         Implemented (xer)
    %4 serviceInstanceID
    %5 threadId                          Auto Injected... however this is a VM language and so this is the Erlang process PID, not the "OS level thread id". Unclear what they want here.
    %6 physical/virtual server name  
    %7 serviceName                       Implemented (from Req)
    %8 PartnerName
    %9 StatusCode                        
    %10 ResponseCode                     Implemented (rcode)
    %11 Response Description             Will not implement. This says "human readable description of the *code*. They don't want the response here. I won't do that because this is a generic function that gets called no matter what the code is, so I can't do that. But since this is supposed to be human readable, they can look up the code in the swagger spec. 
    %12 instanceUUID
    %13 Category log level               Implemented (Sev)
    %14 Severity
    %15 Server IP address                Implemented (from Req)
    %16 ElapsedTime                      Auto Injected but THIS IS SO DUMB TO BE IN THE STANDARD WASTING DISK SPACE THIS IS DIRECTLY COMPUTABLE FROM 1,2 WTF
    %17 Server
    %18 ClientIPaddress                  Implemented (from Req)
    %19 class name                       Implemented (mod), though docs say OOP, I am using the Erlang module here
    %20 Unused                           Implemented..
    %21 ProcessKey
    %22 CustomField1
    %23 CustomField2
    %24 CustomField3
    %25 CustomField4
    %26 detailMessage                    Implemented (msg)
    
    {StartT, EndT, ElapT} = start_end_elapsed(ArgPropl),
    %compute message
    Message = ?SC([StartT, "|", EndT, "|", ?PV(xer, ArgPropl), "||", pid(), "||", path(Req), "|||", to_str(?PV(rcode, ArgPropl)), "|see swagger spec||", to_str(Sev), "||", server_add(Req), "|",  ElapT, "||", ip(Req), "|", ?PV(mod, ArgPropl), "|||||||", ?PV(msg, ArgPropl)]),
    F(Message).

metrics(Sev, Req, ArgPropl) ->
    F = case Sev of 
        debug   -> fun(M)->metrics:debug(M) end;
        info    -> fun(M)->metrics:info(M) end;
        warning -> fun(M)->metrics:warning(M) end
    end,
    %The metrics field list:
    %SAME AS METRICS 1-8
    %_____
    %9 TargetEntity           Implemented (tgte)
    %10 TargetServiceName     Implemented (tgts)
    %11 Status Code           Implemented (tgtrsc)
    %_____
    %SAME AS METRICS 9 ->
    %total 29 fields
    
    {StartT, EndT, ElapT} = start_end_elapsed(ArgPropl), 
    %compute message
    Message = ?SC([StartT, "|", EndT, "|", ?PV(xer, ArgPropl), "||", pid(), "||", path(Req),  "||", ?PV(tgte, ArgPropl), "|", ?PV(tgts, ArgPropl), "|", to_str(?PV(tgtrsc, ArgPropl)), "|", to_str(?PV(rcode, ArgPropl)), "|see swagger spec||", to_str(Sev), "||", server_add(Req), "|", ElapT, "||", ip(Req), "|", ?PV(mod, ArgPropl), "||||||||", ?PV(msg, ArgPropl)]),
    F(Message).

err(Sev, ArgPropl) ->
    F = case Sev of 
        warning   -> fun(M)->error:warning(M) end;
        error     -> fun(M)->error:error(M) end;
        critical  -> fun(M)->error:critical(M) end;
        alert     -> fun(M)->error:alert(M) end;
        emergency -> fun(M)->error:emergency(M) end
    end,
    SevInLog = case Sev of
        warning -> "WARN";
        error   -> "ERROR";
        _       -> "FATAL"
    end,    
    %Error field list:
    %1 Timestamp            Auto Injected when this is called
    %2 RequestID            Implemented
    %3 ThreadId             Auto Injected... however this is a VM language and so this is the Erlang thread PID. Not the "OS level pid". Unclear what they want here.
    %4 ServiceName          Implemented
    %5 PartnerName
    %6 TargetEntity
    %7 TargetServiceName
    %8 ErrorCategory        Implemented
    %9 ErrorCode
    %10 ErrorDescription    This is what I'm using detailMessage for, these seem to be the same to me.
    %11 detailMessage       Implemented

    Message = ?SC([iso(), "|", ?PV(xer, ArgPropl), "|", pid(), "|", ?PV(servn, ArgPropl), "||||", SevInLog, "|||", ?PV(msg, ArgPropl)]),
    F(Message).


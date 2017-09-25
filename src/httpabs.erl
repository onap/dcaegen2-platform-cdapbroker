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

-module(httpabs).
-export([get/2,
         post/4, %I miss python's default arguments..
         post/5,
         put/4,
         delete/2
        ]).
-include("application.hrl").
-define(SC(L), util:concat(L)).

%NOTE
%Consider the Erlang statement:
%
%{ok, {{"HTTP/1.1",ReturnCode, State}, Head, Body}} = httpc:get(URL).
%CDAP returns error messages in the “Body” field above.
%
%However, Consul:
%1) Always (in all HTTP failures I’ve tested) returns Body == “500\n”
%2) Returns the error message in the State field
%
%Example:
%
%{{"HTTP/1.0",404,"Client Error: Not Found for url: http://consul.[...].com:8500/v1/kv/hwtestYOUHAVEFAILEDME:rel"},[{"date","Mon, 14 Nov 2016 14:41:03 GMT"},{"server","Werkzeug/0.11.11 Python/3.5.1"},{"content-length","4"},{"content-type","application/json"}],"500\n"}
%
%This means that error handling in HTTP is not consistent across CDAP and Consul.
%
%Thus below, on a failure, I return the concatenation of State and Body

%%%
%%%HELPER
%%%
-spec parse_response({error|ok, any()}, string()) -> httpstat().
parse_response({Status, Response}, URL) ->
    case Status of
        error ->
            lager:error("httpc error: cannot hit: ~s", [URL]),
            case Response of
                no_scheme               -> {400, io_lib:format("ERROR: The following URL is malformed: ~s", [URL])};
                {bad_body, _}           -> {400, "ERROR: The request Body is malformed"};
                {bad_body_generator,_}  -> {400, "ERROR: The request Body is malformed"};
                _  ->
                    lager:error(io_lib:format("Unexpected ERROR hitting ~s", [URL])),
                    {504, list_to_binary(io_lib:format("ERROR: The following URL is unreachable or the request was unable to be parsed due to an unknown error: ~s", [URL]))} %Are there other reasons other than bad body and unreachable that crash request? (Sneak peak: the answer is probably)
            end;
        ok ->
            {{_, ReturnCode, State}, _Head, Body} = Response,
            case ReturnCode of
                200 ->
                    {ReturnCode, Body};
                _ ->
                    lager:error("Error While hitting ~s, Non-200 status code returned. HTTP Code ~p, State ~s, ResponseBody ~s:", [URL, ReturnCode, State, Body]),
                    %see Note at the top of this file
                    RetBody = ?SC(["State: ", State, ". Return Body: ", Body]),
                    {ReturnCode, RetBody}
            end
    end.

sanitize(URL) ->
    %allow URL to look like "www.foo.com" or <<"www.foo.com">>, trim it
    case is_binary(URL) of
        true -> string:strip(binary_to_list(URL));
        false -> string:strip(URL)
    end.

%anywhere you see any() is essentially lazy typing.. fix these someday when time is abundant
-spec post(string(), string()|binary(), string(), any()) -> httpstat().
post(XER, URL, ContentType, Body) ->
    %post that sends the XER, no headers signature
    U = sanitize(URL),
    parse_response(httpc:request(post, {U, [{"x-ecomp-requestid", XER}], ContentType, Body}, [],[]), U).

-spec post(string(), string()|binary(), list(), string(), any()) -> httpstat().
post(XER, URL, Headers, ContentType, Body) ->
    %post that sends XER, appends the header onto the list of desired headers
    U = sanitize(URL),
    parse_response(httpc:request(post, {U, [{"x-ecomp-requestid", XER} | Headers], ContentType, Body}, [],[]), U).

-spec get(string(), string()|binary()) -> httpstat().
get(XER, URL) ->
    %http get that always sends the XER.. even if the server doesn't want it; maybe this will blow up on me one day.
    U = sanitize(URL),
    parse_response(httpc:request(get, {U, [{"x-ecomp-requestid", XER}]}, [], []), U).

-spec put(string(), string()|binary(), string(), any()) -> httpstat().
put(XER, URL, ContentType, Body) ->
    %http put that always sends the XER
    U = sanitize(URL),
    parse_response(httpc:request(put, {U, [{"x-ecomp-requestid", XER}], ContentType, Body}, [],[]), U).

-spec delete(string(), string()|binary()) -> httpstat().
delete(XER, URL) ->
    %http delete that always sends the XER
    U = sanitize(URL),
    parse_response(httpc:request(delete, {U, [{"x-ecomp-requestid", XER}]}, [],[]), U).

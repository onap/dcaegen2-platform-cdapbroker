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

-module(httpabs_tests).
-include_lib("eunit/include/eunit.hrl").
-import(httpabs, [
                   sanitize/1,
                   parse_response/2
                 ]
       ).

sanitize_test() ->
    ?assert(sanitize(<<"     www.foo.com       ">>) == "www.foo.com"),
    ?assert(sanitize("     www.foo.com      ") == "www.foo.com"),
    ?assert(sanitize(<<"www.foo.com">>) == "www.foo.com").

parse_response_test() ->
    NoURL = "THIS IS NOT EVEN A URL WHAT ARE YOU DOING TO ME",
    ?assert(parse_response({error,no_scheme},"THIS IS NOT EVEN A URL WHAT ARE YOU DOING TO ME") == {400, io_lib:format("ERROR: The following URL is malformed: ~s", [NoURL])}),
    ?assert(httpabs:put("testxer", NoURL, "application/json", jiffy:encode({[{<<"a">>, <<"b">>}]})) == {400, io_lib:format("ERROR: The following URL is malformed: ~s", [NoURL])}),

    %test httpabs bad body (not encoded as JSON)
    ReconfigMap = util:ejson_to_map({[{<<"foo">>, <<"bar">>}]}),
    BadBody = {[{<<"config">>, ReconfigMap}]},
    ?assert(httpabs:put("testxer", "www.foo.com", "application/json", BadBody) == {400,"ERROR: The request Body is malformed"}),
    ?assert(parse_response({error,{bad_body_generator, BadBody}}, "www.foo.com") == {400,"ERROR: The request Body is malformed"}),
    ?assert(parse_response({error,{bad_body, BadBody}}, "www.foo.com") == {400,"ERROR: The request Body is malformed"}),
    ?assert(parse_response({error,{this_was, "not expected"}}, "www.fubar.com") == {504,<<"ERROR: The following URL is unreachable or the request was unable to be parsed due to an unknown error: www.fubar.com">>}),

    %try a 200
    ?assert(parse_response({ok,{{"HTTP/1.1",200,"OK"},[{"cache-control","private, max-age=0"},{"date","Mon, 11 Sep 2017 15:05:11 GMT"},{"accept-ranges","none"},{"server","gws"},{"vary","Accept-Encoding"},{"content-length","46376"},{"content-type","text/html; charset=ISO-8859-1"},{"expires","-1"},{"p3p","..."},{"x-xss-protection","1; mode=block"},{"x-frame-options","SAMEORIGIN"},{"set-cookie","NID=111=nGQHl8ljJ3nXHmGmIZmGaTgoq3WAdbgWaxAUOQJm-0AaOkS64iiXtm-HojIFSpqowj7Nr-KpqS8o-oDOROq-4AaDs0J4M92V7yBOAJQYPkuK7wtVav0BhpOOYgCHysUN; expires=Tue, 13-Mar-2018 15:05:11 GMT; path=/; domain=.google.com; HttpOnly"},{"alt-svc","quic=\":443\"; ma=2592000; v=\"39,38,37,35\""}],"<!doctype html>..."}}, "https://google.com") == {200, "<!doctype html>..."}),

    % try a 404
    ?assert(parse_response({ok,{{"HTTP/1.1",404,"OK"},[{"cache-control","private, max-age=0"},{"date","Mon, 11 Sep 2017 15:05:11 GMT"},{"accept-ranges","none"},{"server","gws"},{"vary","Accept-Encoding"},{"content-length","46376"},{"content-type","text/html; charset=ISO-8859-1"},{"expires","-1"},{"p3p","..."},{"x-xss-protection","1; mode=block"},{"x-frame-options","SAMEORIGIN"},{"set-cookie","NID=111=nGQHl8ljJ3nXHmGmIZmGaTgoq3WAdbgWaxAUOQJm-0AaOkS64iiXtm-HojIFSpqowj7Nr-KpqS8o-oDOROq-4AaDs0J4M92V7yBOAJQYPkuK7wtVav0BhpOOYgCHysUN; expires=Tue, 13-Mar-2018 15:05:11 GMT; path=/; domain=.google.com; HttpOnly"},{"alt-svc","quic=\":443\"; ma=2592000; v=\"39,38,37,35\""}],""}}, "https://google.com") == {404,"State: OK. Return Body: "}).


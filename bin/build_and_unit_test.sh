#!/usr/local/bin/fish
rm -rf _build/;
rebar3 upgrade;
rebar3 release;
rebar3 eunit --cover;
rebar3 cover

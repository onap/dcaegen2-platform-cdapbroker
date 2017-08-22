#!/usr/local/bin/fish
rm -rf _build/; 
rebar3 upgrade; 
rebar3 release; 
dialyzer -r _build/default/lib/cdapbroker/ebin/

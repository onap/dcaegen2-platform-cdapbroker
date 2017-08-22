#!/usr/local/bin/fish
rm -rf /tmp/log/cdapbroker/*;
rebar3 release; 
set -x NEXUS_RAW_ROOT "XXXX";
set -x CDAP_CLUSTER_TO_MANAGE "cdap";
set -x CONSUL_HOST "XXXX";
set -x CONFIG_BINDING_SERVICE "config_binding_service";
set -x HOSTNAME "cdap_broker";
set -x REBAR3_ERL_ARGS "-sname cdapbroker@localhost";
rebar3 local install;
env DEBUG=1 ~/.cache/rebar3/bin/rebar3 ct;
rebar3 eunit

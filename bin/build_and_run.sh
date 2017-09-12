#!/usr/local/bin/fish
rm -rf /tmp/log/cdapbroker/*;
rm -rf _build/;
rebar3 upgrade;
rebar3 release;
set -x CDAP_CLUSTER_TO_MANAGE "cdap";
set -x CONSUL_HOST "XXXX";
set -x CONFIG_BINDING_SERVICE "config_binding_service"; 
set -x HOSTNAME "cdap_broker";
./_build/default/rel/cdapbroker/bin/cdapbroker

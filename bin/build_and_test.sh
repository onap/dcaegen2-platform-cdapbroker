#!/usr/local/bin/fish

# ================================================================================
# Copyright (c) 2017 AT&T Intellectual Property. All rights reserved.
# ================================================================================
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ============LICENSE_END=========================================================
#
# ECOMP is a trademark and service mark of AT&T Intellectual Property.

rm -rf /tmp/log/cdapbroker/*;
rm -rf _build/*
rebar3 upgrade;
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

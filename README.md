# cdapbroker
This repo is the thing in red:
![Alt text](doc/cdap_broker.png?raw=true)

The high level workflow for how this broker is to be used in the DCAE platform is as follows
<ol>
<li> CLoudify wants to deploy a new CDAP app. It deploys it via the broker. </li>
<li> This broker registers with Consul and lights up a metrics, health, and configuration change endpoint for the app </li>
<li> The broker pushes the app's configuration into Consul for the Ops view layer (documentation purposes)
<li> At periodic intervals the health endpoints (plural when mutliple CDAP apps are registered), provided by this broker, are pinged by consul </li>
<li> At peridic intervals, some metrics system (TBD) pulls then pushes metrics from the apps </li>
<li> Occasionaly some higher power decides to update configuration for a running CDAP app. They make the change via the broker. </li>
<li> Finally on a service undeploy Cloudify deletes the CDAP app via the broker </li>
</ol>

# Purpose & Responsibilities
The purposes of this broker are as follows:

1. To isolate CDAP developers from the complexity of the DCAE platform. The only thing CDAP developers should need to interface with are the CDAP APIs. They should not need to know about concepts like "service discovery" or "confiuration discovery". They should specify what external systems they should talk to in their configuration template and the rest should be hidden from them.  </li>

2. To minimize the onboarding process for CDAP developers. Their *only* deliverables should be a JAR (with built in metrics), and their configuration template. </li>

3. Provide Consul interfacing, metrics, healthchecks, and configuration updates bewtween CDAP applications and the rest of the platform. Specifically, this has northbound interfaces for
    <ol>
    <li> providing initial configuration of a CDAP app</li>
    <li> changing configuration of a CDAP app</li>
    <li> getting the custom metrics of a CDAP app</li>
    <li> healthchecking a CDAP app</li>
    </ol>
  And talks to the CDAP APIs in various ways southbound to achieve these objectives.

# CDAP service discovery
When registering an application (`appname`) with the broker, the broker registeres *itself* as the Address and Port of that CDAP application. That is because a CDAP application can have multiple services behind it, so there is no way to register a CDAP application in Consul. 

The CDAP client library for collectors (or anything upstream of a CDAP app) will be required to do the following:

1. do a consul service discovery GET on `appname`
2. Take that serviceaddress and port, and form `serviceaddress:port/application/appname`
3. Do a GET on that, which is the broker
4. Parse out `connectionurl` and `serviceendpoints`, which is input and output connection information for the CDAP app. See the swagger spec (in the swagger folder in this repo) for the schema of those.

# Special charcters: hackaround
CDAP application names cannot have special characters in them. 
When this broker talks southbound to CDAP, it maps `APPNAME` into `APPNAME_WITHOUT_SPECIAL_CHARACTERS`. So in CDAP you will see the latter. However,
     when communicating with this broker API, you should still pass in `APPNAME`. The conversion is handled internally.

# Logs
The broker conforms to ONAP/ECOMP logging requirements (aka EELF). The logs are written to `/tmp/log/cdapbroker/..`. If running in Docker, you'll want to mount this outside into a safe store. 

# Running and testing

## Runtime Assumptions 
In a real deployment setting, these assumptions are handled by the ONAP Docker plugin when this is launched. 
When doing local testing, you must set up these assumptions yourself. 

1. `HOSTNAME` is set as an env variable in this runtime environment and is a Consul key that holds this broker's configuration. 

2. `CONSUL_HOST` is set as an env variable and is either an IP address or a resolvable name (Via real DNS whereever you are running this). It should not include the port, the broker currently hardcodes 8500.

3. `CDAP_CLUSTER_TO_MANAGE` is an env variable, and cooresponds to a CDAP cluster registered in `CONSUL_HOST`. This is the CDAP cluster the broker will manage when it runs. The CDAP API port must be registered as well, that is, the broker does *not* hardcode "well known ports" and retrieves it from Consul. (Currently for CDAP 3 that port is 10000 and CDAP 4 it is 11011.)

4. `CONFIG_BINDING_SERVICE` is set as an env variable, and cooresponds to the name that the CBS is registered in `CONSUL_HOST`.

5. The user that runs this application needs write access to the directory '/var/mnesia'. This works in Docker, but on your local machine, this may not be owned by your local user.In this case, create the dictory and CHOWN it to your user. The broker writes it's database backup to that location. 

## The integration test suite
Bestides the unit tests, the broker comes with an integration suite that touches both Consul and CDAP (the ones pointed to via `CONSUL_HOST` and `CDAP_CLUSTER_TO_MANAGE`. It will actually deploy test apps, test configurations into Consul, and then delete them after the test suite is over. Please note that this is not a unit test, it touches the real systems the broker is connected to. It can be used to verify a signifigant portion of the DCAE, including CDAP, Consul, the broker, and the config binding service.

The integration test suite requires several test artifacts to be loaded into a Nexus Raw Webserver. In the test suite, the root of this server is read from the env `NEXUS_RAW_ROOT`. Thus, in order to run the integration suite, in addition to the above runtime assumptions, this env must be set.
(TODO: Talk about the specific artifacts to be loaded in)

## Docker

### Build
Note: doing a Docker build *automatically* runs the unit tests. In fact, the docker container will fail to build if any unit tests fail.

    docker build --rm -t cdapbroker:VERSION .

### Run

    docker run -v /tmp/log/cdapbroker/:/tmp/log/cdapbroker/ -d -e CDAP_CLUSTER_TO_MANAGE="cdap_cluster" -e HOSTNAME="cdap_broker" -e CONSUL_HOST="XXXX" -e CONFIG_BINDING_SERVICE="config_binding_service" -p 7777:7777 DOCKER_REGISTRY/cdapbroker:VERSION

(Fill in `CONSUL_HOST` and `DOCKER_REGISTRY`)
    
### Running the Integration Suite in Docker

    docker exec -it CONTAINER_ID_HERE /bin/bash
    export NEXUS_RAW_ROOT=XXXX
    rebar3 ct

## Local Development
The below is for building, running, and testing on your local machine. This requires Erlang 19+ and rebar3 on your machine. These scripts require the runtime assumptions mentioned above. 

### Running locally
To use this script, you will have to modify `CONSUL_HOST` for your enviornment. 

./bin/build_and_run.sh

### Running the integration test suite
To use this script, you will have to modify `CONSUL_HOST` and `NEXUS_RAW_ROOT` for your enviornment. 

    ./bin/build_and_test.sh

### Running the unit tests 

    ./bin/build_and_unit_test.sh

### Running the Dialyzer
This is a type checking tool that attempts to find errors based on types. 
Warning, right now very few things are typed/specs, so the usefulness of this is limited:

    ./bin/build_and_check.sh

# On "Leave No Trace"
The CDAP broker does not currently implement "leave no trace", because it is not really clear in DCAE when this should be done, or whether the orchestrator should be allowed to invoke such deadly operations, i.e., maybe this should be done by an operations team. 
Leave No Trace here means deleting streams and datasets after an application is deleted from the broker. 
The problem is that streams and datasets can be shared between applications, like a shared Kafka queue used by two CDAP applications. 
Streams and datasets are created by the CDAP deployment API when an application that uses a streamname or a dataset name for the first time, however subsequent apps can be deployed using those, without re-creating them. 
Moreover, subsequently deployed apps may be *expecting* that data already exists in some dataset. 
Consider the case of deplyoying A1 that creates S1, then deploying A2 that uses S1, then wanting to undeploy A1. Deleting the stream or it's data on the deletion of A1 would clobber A2. 

This leaves the question of who/what/when/where/why/how streams and datasets should be cleaned up from CDAP clusters in DCAE. 
For now, this is an open question. It can always be done on the CDAP management interface by an operations team in the meantime.  

# On Version Bumping (Development)
Currently the CDAP Broker Version is in three places in this repo..
1. rebar.config
2. src/cdapbroker.app.src
3. swagger spec
If you make a developmemt change, please bump in all places

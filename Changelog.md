# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) 
and this project adheres to [Semantic Versioning](http://semver.org/).

## [4.0.7] - Sep 12 2017
* Further unit testing
* Update dependency to a Leptus PR that fixes the CORS port issue

## [4.0.6] - Sep 11 2017
* Get meck working for better unit tests
* More unit tests
* Removal of some "integration tests" that are now covered by unit tests
* Small cleanups.

## [4.0.5] - Missing =(

## [4.0.4] - Aug 29 2017
* Cleanup cdap undeploy code and fix misleading log statements
* Attempt to add ONAP build artifacts

## [4.0.3] - Missing =(

## [4.0.2] - July 26 2017
* Make testing magical so that it runs on localhost or in Docker the same
* Code cleanups in workflows thanks to abstraction provided by Garry

## [4.0.1] - July 7 2017
* Work with config binding service 1.0.0.

## [4.0.0] - June 28 2017
* Policy reconfiguration work. There are two new reconfiguration endpoints: smart and app-preferences. 
* The smart interface allows you to send a JSON of keys that are either in app_preferences, app_config, or both. The broker figures out the overlaps, refreshes them in Consul, and makes the appropriate CDAP reconfiguration calls. The preferences API allows you to simply reset a CDAP app's application preferences (analagous to the call that was already in here for app_config).
* I broke the original app_config API to make all three reconfiguration APIs consistent. You give it "reconfiguration_type" and "config" for all three APIs. And hopefully for any future reconfiguration APIs, e.g., for hydrator pipelines. 
* Hardcodes the cute short name "config_binding_service", now that DCAE is using short names. Obliterates usage of the env variable and removes passing it around all over the place.

## [3.4.8] - May 16 2017
* support JARs hosted at https webservers
* trim JAR URLs to prevent problems due to whitespace
* add a primitive get_version.sh script for the build team to use.. needs to be actually implemented instead of printing a fixed version right now..

## [3.4.7] - May 15 2017
* EELF part 6... pretty much done at this point, some less-important logs could still be cleaned up maybe.
* All HTTP requests made from the broker to another HTTP API now include the XER. 
* All functions in `src/workflows` are now EELF compliant, and produce detailed metrics logs. Workflows is the workhorse of all the major API calls so this tracks pretty much all requests the broker makes to to it's job.  
* App Config reconfiguration is now a workflow

## [3.4.6] - May 12 2017
* All HTTP GETs issued by this broker now pass an/the XER... even if the downstream server is not expecting it #ExpectUs..

## [3.4.5] - May 10 2017
* error EELF field implemented, three important Metrics EELF fields added
* src/workflows has no more lager (non EELF) statements... though it could use *more* EELF statements because part of it is not covered by logs..

## [3.4.4] - Apr 28 2017
* four more EELF fields implemented, one ignored.

## [3.4.3] - Apr 27 2017
* Fixes audit records not being used at the end of API calls (they now are, and elapsed time is computed)
* Implements ISO8601 timestamps in all log records
* Implements a few more of the EELF fields, still more to go

## [3.4.2] - Apr 26 2017 
* Towards Implementing the "ONAP logging requirements" aka "EELF":
* Logs are now written to /tmp/log/cdapbroker/... and are in three files: error.log, audit.log, metrics.log (logs are still also written to stdout)
* X-ECOMP-RequestID is now parsed and logged, and generated if missing, for all main broker APIs. It does not yet generate it's own for the call to the CBS.
* EELF logging functions added to a new source file `src/logging`.
* The field list formats and delimiters in each of those follow version 1.0 of the ONAP logging requirements. However, many required fields are left blank right now, this needs to be fixed. Parsing the fields should work though as each has the correct positional arguments and correct number of positions. 
* Only functions in `src/resource_handler` are using the EELF logging functions right now, this needs to be fixed

## [3.4.1]
* Move tests, update README

## [3.4.0] 
* Only healthcheck programs that are part of an intial program-flowlet PUT. The use case for this was a CDAP application that had flows that were purposely not wanting to be started by the component developer (whether that is valid is another question...)
    The broker thought their application was in an unhealthy state because it was sourcing the programs to get the health of from CDAP itself, and not the intial PUT request. 
    This actually goes back to the way healthcheck used to work, but this now does it in a better way, because the application table is now used for multiple types of CDAP apps
* To accomplish the above, this creates a "supplementary" table for program-flowlet apps, that right now stores the programs that are in the intial PUT, but could be extended later with more properties.
* Paves the way for a "hydrator supplemental" table if that is ever needed
* Starts the painful process of adding function and type definitions so that the Erlang Dialyzer ("bolt on" static typing tool) is of some use; sick of huge positional tuples
* Fixes a bug where undeploy would not "plow through" if CDAP was totally unreachable (or gone)

## [3.3.0]
* Broker now includes it's own API version in information endpoint

## [3.2.1]
* Fix an error found in HTTP error reporting (a bad request type of error is now correctly caught)
* Fix errors in test cases found while testing a new version of CDAP. Tests are now more agnostic to json key ordering. 

## [3.2.0]
* Returns more information in the "infomation endpoint" ('/'). Specifically, now returns the CDAP GUI port and the CDAP cluster version.
This is mostly to aid component developers in testing that are hitting the broker via the DCAE CLI tool. 
* Introduces the first unit test (eunit; the rest of the test suite is ran via common_test).

## [3.1.0]
* Persistence. This is an important feature. This persists the MNesia database to disk so that you can stop and start the broker without losing state. NOTE: Catching Docker SIGTERM is not yet implemented. It turns out this is non-trivial to do in Erlang. So, in rare cases, if `docker stop` is called immediately after `delete`, the deletion may not persist to the database. 
* Erlang 19.3 is supposed to handle SIGTERM at the BEAM level. That is not out yet as Docker (it was just announced a week ago.). 
The proper way to shut down an Erlang node is to execute a command to the BEAM.
I don't know what Erlang developers did for 20 years, but to date the BEAM does not handle SIGTERM, so a wrapper script is needed if you want to kill the BEAM gracefully on a SIGTERM. 
This was needed because this broker runs in Docker. With database persistence, the broker has a graceful stop function that ensures any transactions in cache (RAM) are persisted to disk before the shutdown.
I need this function called, so I need SIGTERM to be handled in a way that gracefully terminates the VM, which in turn gracefully shuts down my application, which in turn calls this function. Supposedly, this will be no longer necessary soon: http://www.erlang.org/news/110

## [3.0.0]
* Waterfall CDAP_CLUSTER_TO_MANAGE
* Handle CBS moving while broker is up
* For program-flowlet apps, return configuration when info requested
* make /application only return the appnames (API change)
* make testing more stable by not using order-dependent Expected JSONs
* Reconfiguring with an invalid request returns a 4xx instead of 5xx now (bugfix)
* Dead programs/flowlets now triggers a failing healthcheck (bugfix)
* Rename "reconfigure_app_config" to make way for reconfiguring preferences (API change)

## [2.2.0] 
* Implements healthchecking for Pipelines based on Terry S's healthCheck.py script

## [2.1.0]
* Added a text file of some example commands that can be useful for demos and understanding this repo
* Fixed a bug in a workaround for https://issues.cask.co/browse/CDAP-7191?filter=-2
* Added the ability to deploy Pipelines that contain custom JARs

## [2.0.0]

* Add support for primitive CDAP Pipelines. Required an API break because the API caller has to specify whether the application is a program-flowlet style app or a hydrator pipeline. 
* Changelog did not exist before this...

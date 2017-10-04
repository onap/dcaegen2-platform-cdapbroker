FROM erlang:19.3
MAINTAINER tommy at research dot eh tee tee dot com

ENV TERM=xterm

#copy files into repo
ADD src /tmp/src
ADD test /tmp/test
ADD rebar.config /tmp
ADD config /tmp/config 

WORKDIR /tmp

#make sure there is no leftover release
RUN rm -rf _build/

#build
RUN rebar3 upgrade
RUN rebar3 release

#fail the docker build if unit tests fail
RUN rebar3 eunit 

#set the broker test type so the integration test can be run inside Docker
ENV BROKER_TEST_TYPE=DOCKER

#run
EXPOSE 7777

#see https://github.com/erlang/rebar3/issues/1593
ENTRYPOINT ["./_build/default/rel/cdapbroker/bin/cdapbroker"]
CMD ["foreground"]

#!/usr/bin/env bash
echo "if testing locally send SIGTERM to $$"

term_handler() {
  echo "Stopping the Erlang VM gracefully"
  #/usr/local/Cellar/erlang/19.1/lib/erlang/lib/erl_interface-3.9.1/bin/erl_call -c cdapbroker -s -a 'init stop' -n 'cdapbroker@localhost'
  /usr/local/lib/erlang/lib/erl_interface-3.9.2/bin/erl_call -c cdapbroker -s -a 'init stop' -n 'cdapbroker@localhost'
  echo "Erlang VM Stopped"
}

trap term_handler SIGQUIT SIGINT SIGTERM

./_build/default/rel/cdapbroker/bin/cdapbroker & 
PID=$!

echo "Erlang VM Started"
#wait $PID
while kill -0 $PID ; do wait $PID ; EXIT_STATUS=$? ; done
echo "Exiting Wrapper."
exit $EXIT_STATUS

General Computing Node
======================

`gen_node` is a concept code for a generic computing node server implemented in Erlang. It is based on a modified version of Joe Armstrong's Universal Server. It uses a `gen_server` behavior to wrap around its functionality.

Summary
=======

User of `gen_node` application can spawn any number of gen_node servers. By default, these processes will wait and do nothing. Once they get instructions on what to do, they'll "become" what they are told.

Compile and Run
===============

````
make
_rel/gen_node_release/bin/gen_node_release console
````

Example
=======

````
1> Double = fun Double() -> receive {_, reset} -> ok; {From, Args} -> From ! Args+Args, Double() end end.
#Fun<erl_eval.44.106461118>
2> Square = fun Square() -> receive {_, reset} -> ok; {From, Args} -> From ! Args*Args, Square() end end.
#Fun<erl_eval.44.106461118>
3> {ok, N, _} = gen_node:start_server().
{ok,<0.42.0>,#Ref<0.0.0.45>}
4> gen_node:become(N, Double).
ok
5> gen_node:send(N, 16).
ok
6> flush().
Shell got 32
ok
7> gen_node:reset(N).
ok
8> gen_node:become(N, Square).
ok
9> gen_node:send(N, 16).
ok
10> flush().
Shell got 256
ok
````

More Info
=========

For now, please find more info in this blog post:
http://unix0.wordpress.com/2014/04/17/elastic-applications-in-erlang/

General Computing Node
======================

`gen_node` is a concept code for a generic computing node server implemented in Erlang. It is based on a modified version of Joe Armstrong's Universal Server.

Summary
=======

User of `gen_node` application can spawn any number of gen_node servers. By default, these processes will wait and do nothing. Once they get instructions on what to do, they'll "become" what they are told.

Internal Implementation
=======================

Each `gen_node` server is implemented as a process pair.

The first process - the controlling process - is a `gen_server` under the control of the OTP supervisor. This process' main task is to keep track of the status of the "node". It's also used as a gateway for sending the task to the actual worker process.

The second process is the worker process that performs actual tasks and computations. It's linked to the controlling process. Its sole task is to wait for the instructions from the controlling process and perform the task given.

Supervision Tree
----------------

![gen_node supervision tree](/guide/supervision-tree.png?raw=true)

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
3> {ok, N} = gen_node:start_server().
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

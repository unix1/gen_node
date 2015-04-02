-module(gen_node).

-behaviour(application).

-export([start/2, stop/1]).
-export([start_server/0, start_server/1, stop_server/1]).
-export([become/2, get_state/1, get_states/0, reset/1, send/2]).

-type gen_node_ref() :: tuple(ok, pid(), reference()).
-type stop_server() :: ok | tuple(error, running | restarting | not_found
    | simple_one_for_one).
-type worker_state() :: #{state => ready | working, worker => pid()}.

%%%%% Admin API %%%%%
start(normal, _Args) ->
    gen_node_server_sup:start_link().

stop(_) ->
    ok.

%%%%% User API %%%%%

%% @doc Starts a gen_node server.
%% Returns the created gen_server `pid` and `id`.
-spec start_server() -> gen_node_ref().
start_server() ->
    gen_node_server_sup:start_server().

%% @doc Starts a gen_node server with a specified name. Otherwise, it works
%% the same as `start_server/0`. Note that the name you pass here will be
%% used as the gen_server child ID, and you will get it back in `reference`.
-spec start_server(Name :: atom()) -> gen_node_ref().
start_server(Name) when is_atom(Name) ->
    gen_node_server_sup:start_server(Name).

%% @doc Stops the given `gen_node_server`. Note that this will have an effect
%% of stopping the linked worker as well.
-spec stop_server(Name :: pid() | atom()) -> stop_server().
stop_server(Name) ->
    gen_node_server_sup:stop_server(Name).

%% @doc Tells worker to become a given `fun`.
-spec become(Name :: pid() | atom(), Fun :: fun()) -> ok.
become(Name, Fun) ->
    gen_node_server:become(Name, Fun).

%% @doc Gets current PID and state of the worker.
-spec get_state(Name :: pid()) -> worker_state().
get_state(Name) ->
    gen_node_server:get_state(Name).

%% @doc Gets PIDs and states of all workers.
-spec get_states() -> list(worker_state()).
get_states() ->
    lists:foldl(
        fun(Pid, Acc) -> [{Pid, get_state(Pid)}|Acc] end,
        [],
        lists:foldl(
            fun({_Ref, Pid, worker, _Mods}, Acc) -> [Pid|Acc] end,
            [],
            supervisor:which_children(gen_node_server_sup)
        )
    ).

%% @doc Sends a reset message to the worker.
%% NOTE Worker function must accept a reset message `{FromPid, reset}`
-spec reset(Name :: pid()) -> ok.
reset(Name) ->
    gen_node_server:reset(Name).

%% @doc Sends arbitrary messages to the worker in the form of `{Pid, Args}`
%% where `Pid` is the process ID of the caller.
%% This is not to be confused with `Name` argument which is the Pid of the
%% `gen_node_server`.
-spec send(Name :: pid(), Args :: any()) -> ok.
send(Name, Args) ->
    gen_node_server:send(Name, Args).

-module(general_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([gen_node_server_start/1]).
-export([gen_node_server_state_after_start/1]).
-export([gen_node_server_state_after_work/1]).
-export([gen_node_server_state_during_work/1]).
-export([gen_node_server_states/1]).
-export([gen_node_server_work/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        gen_node_server_start,
        gen_node_server_state_after_start,
        gen_node_server_state_during_work,
        gen_node_server_work,
        gen_node_server_state_after_work,
        gen_node_server_states
    ].

init_per_suite(Config) ->
    ok = application:start(gen_node),
    Config.

end_per_suite(_) ->
    application:stop(gen_node),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Utility functions
%% ============================================================================

double_fun() ->
    fun Double() -> receive {_, reset} -> ok; {From, Args} ->
            From ! Args+Args, Double() end end.

%% ============================================================================
%% Tests
%% ============================================================================

gen_node_server_start(_) ->
    {ok, _ChildId} = gen_node:start_server().

gen_node_server_state_after_start(_) ->
    {ok, ChildId} = gen_node:start_server(),
    #{state := ready} = gen_node:get_state(ChildId).

gen_node_server_state_during_work(_) ->
    {ok, ChildId} = gen_node:start_server(),
    ok = gen_node:become(ChildId, double_fun()),
    #{state := working} = gen_node:get_state(ChildId).

gen_node_server_work(_) ->
    {ok, ChildId} = gen_node:start_server(),
    ok = gen_node:become(ChildId, double_fun()),
    ok = gen_node:send(ChildId, 15),
    ok = receive
        30 ->
            ok;
        Message ->
            {unexpected_message, Message}
        after 1 ->
            timeout
    end.

gen_node_server_state_after_work(_) ->
    {ok, ChildId} = gen_node:start_server(),
    ok = gen_node:become(ChildId, double_fun()),
    ok = gen_node:reset(ChildId),
    #{state := ready} = gen_node:get_state(ChildId).

gen_node_server_states(_) ->
    {ok, ChildId1} = gen_node:start_server(),
    {ok, ChildId2} = gen_node:start_server(),
    ok = gen_node:become(ChildId2, double_fun()),
    States = gen_node:get_states(),
    {ChildId1, #{state := ready}} = lists:keyfind(ChildId1, 1, States),
    {ChildId2, #{state := working}} = lists:keyfind(ChildId2, 1, States).

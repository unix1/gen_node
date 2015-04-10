-module(gen_node_server).

-behaviour(gen_server).

-export([start_link/1, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([become/2, done/1, get_state/1, reset/1, send/2]).

-type worker_state() :: #{state => ready | working, worker => pid()}.

%%%%% Supervision functions %%%%%

start_link(Sup) ->
    gen_server:start_link(?MODULE, {Sup}, []).

init({_Sup}) ->
    WorkerPid = spawn_link(gen_node_worker, init, [self()]),
    {ok, #{worker => WorkerPid, state => ready, what => null}}.

%%%%% User functions %%%%%

%% @doc Makes the worker become the given `fun`.
-spec become(Pid :: pid(), F :: fun()) -> ok.
become(Pid, F) when is_pid(Pid), is_function(F, 0) ->
    ok = gen_server:call(Pid, {become, F}).

%% @doc Gets current PID and state of the worker.
-spec get_state(Pid :: pid()) -> worker_state().
get_state(Pid) ->
    {ok, State} = gen_server:call(Pid, {get_state}),
    State.

%% @doc Sends a reset message to the worker.
%% NOTE Worker function must accept a reset message `{FromPid, reset}`
-spec reset(Pid :: pid()) -> ok.
reset(Pid) ->
    ok = gen_server:call(Pid, {reset}).

%% @doc Sends arbitrary messages to the worker in the form of `{Pid, Args}`
%% where `Pid` is the process ID of the caller.
%% This is not to be confused with `Pid` argument which is the Pid of the
%% `gen_node_server`.
-spec send(Pid :: pid(), Args :: any()) -> ok.
send(Pid, Args) ->
    ok = gen_server:call(Pid, {send, Args}).

%%%%% Worker functions %%%%%

%% @doc Worker is done, update our status.
%% This would typically be called by the worker when it completes.
-spec done(Pid :: pid()) -> ok.
done(Pid) ->
    ok = gen_server:call(Pid, {done}).

%%%%% Server functions %%%%%

handle_call({become, F}, _From, S = #{worker := WorkerPid, state := ready}) ->
    WorkerPid ! {become, F},
    {reply, ok, S#{state := working, what => F}};
handle_call({get_state}, _From, S = #{worker := WorkerPid, state := State}) ->
    {reply, {ok, #{worker => WorkerPid, state => State}}, S};
handle_call({done}, _From, S = #{state := working}) ->
    {reply, ok, S#{state := ready, what := null}};
handle_call({reset}, {FromPid, _Tag}, S = #{worker := WorkerPid}) ->
    % we just send a reset message to the worker, we don't update its state
    % here, because it will send us a 'done' message when it has completed
    WorkerPid ! {FromPid, reset},
    {reply, ok, S};
handle_call({send, Args}, {FromPid, _Tag}, S = #{worker := WorkerPid, state := working}) ->
    WorkerPid ! {FromPid, Args},
    {reply, ok, S}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

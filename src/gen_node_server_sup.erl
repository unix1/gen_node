-module(gen_node_server_sup).
-export([start_link/0, init/1, start_server/0, start_server/1, stop_server/1]).
-behaviour(supervisor).

-define(CHILD(Id), {Id, % child id
        {gen_node_server, start_link, [self()]},
        permanent,
        5000, % shutdown time
        worker,
        [gen_node_server]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_server() ->
    ChildId = make_ref(),
    {ok, Pid} = supervisor:start_child(?MODULE, ?CHILD(ChildId)),
    {ok, Pid, ChildId}.

start_server(Name) ->
    supervisor:start_child(?MODULE, ?CHILD(Name)).

stop_server(Name) ->
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).

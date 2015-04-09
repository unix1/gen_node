-module(gen_node_server_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_server/0, stop_server/1]).

%%%%% Supervision functions %%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    ChildSpec = {gen_node_server,
                 {gen_node_server, start_link, [self()]},
                 permanent,
                 5000, % shutdown time
                 worker,
                 [gen_node_server]},
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.

%%%%% User functions %%%%%

start_server() ->
    {ok, Pid} = supervisor:start_child(?MODULE, []).

stop_server(Pid) ->
    supervisor:terminate_child(?MODULE, Pid),
    supervisor:delete_child(?MODULE, Pid).

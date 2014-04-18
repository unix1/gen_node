-module(gen_node).
-export([start/2, stop/1]).
-export([start_server/0, start_server/1, stop_server/1, become/2, reset/1, send/2]).
-behaviour(application).

%%%%% Admin API %%%%%
start(normal, _Args) ->
    gen_node_server_sup:start_link().

stop(_) ->
    ok.

%%%%% User API %%%%%

start_server() ->
    gen_node_server_sup:start_server().

start_server(Name) when is_atom(Name) ->
    gen_node_server_sup:start_server(Name).

stop_server(Name) ->
    gen_node_server_sup:stop_server(Name).

become(N, F) ->
    gen_node_server:become(N, F).

reset(N) ->
    gen_node_server:reset(N).

send(N, Args) ->
    gen_node_server:send(N, Args).
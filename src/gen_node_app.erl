-module(gen_node_app).
-export([start/2, stop/1]).
-export([start_server/0, start_server/1, stop_server/1]).
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
    % TODO check Name matches kv_xxx
    gen_node_server_sup:start_server(Name).

stop_server(Name) ->
    gen_node_server_sup:stop_server(Name).

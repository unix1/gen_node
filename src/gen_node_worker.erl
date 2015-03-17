-module(gen_node_worker).
-export([init/1]).

init(ServerPid) ->
    loop(#{server => ServerPid}).

loop(State = #{server := ServerPid}) ->
    receive
        {become, F} ->
            F()
    end,
    ok = gen_node_server:done(ServerPid),
    loop(State).

-module(gen_node_server).
-behaviour(gen_server).
-export([start_link/1, start_link/2, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([become/2, get_state/1, reset/1, send/2]).

%%%%% Supervision functions %%%%%

%% start without name
start_link(Sup) ->
    gen_server:start_link(?MODULE, {Sup}, []).

%% start with locally registered name
start_link(Name, Sup) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, {Sup}, []).

init({_Sup}) ->
    {ok, #{state => ready, what => null}}.

%%%%% User functions %%%%%

become(Name, F) ->
    ok = gen_server:call(Name, {become, F}),
    ok = gen_server:cast(Name, {start_working, F}).

get_state(Name) ->
    {ok, State} = gen_server:call(Name, {get_state}),
    State.

reset(Name) ->
    ok = gen_server:cast(Name, reset).

send(Name, Args) ->
    Name ! {self(), Args},
    receive
        X -> X
    end.

%%%%% Server functions %%%%%

handle_call({become, F}, _From, S = #{state := ready}) ->
    {reply, ok, S#{state := working, what => F}};
handle_call({get_state}, _From, S = #{state := State}) ->
    {reply, {ok, State}, S}.

handle_cast({start_working, F}, S = #{state := working, what := F}) ->
    F(),
    {noreply, S#{state := ready, what => null}};
handle_cast(reset, S) ->
    {noreply, S#{state := ready, what => null}}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

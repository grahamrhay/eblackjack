-module(blackjack).

-behaviour(gen_server).

-export([start_link/0, start/0, new_game/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() ->
    application:start(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call(new_game, _From, State) ->
    {ok, GamePid} = supervisor:start_child(bj_game_sup, []),
    {reply, {ok, GamePid}, State}.

handle_cast(_Msg, N) ->
    {noreply, N}.

handle_info(_Msg, N) ->
    {noreply, N}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

new_game() ->
    gen_server:call(?MODULE, new_game).

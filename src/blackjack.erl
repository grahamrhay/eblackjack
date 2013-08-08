-module(blackjack).

-behaviour(gen_server).

-export([start_link/0, start/0, new_game/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { deck }).

start() ->
    application:start(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(new_game, _From, State) ->
    io:format("New game!~n", []),
    {reply, [], State};

handle_call(_, _From, State) ->
    {reply, [], State}.

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

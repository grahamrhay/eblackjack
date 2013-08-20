-module(bj_game).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0, hit/1, stick/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {dealer, cards}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    io:format("New game!~n", []),
    {ok, DealerPid} = supervisor:start_child(bj_dealer_sup, []),
    ok = bj_dealer:shuffle(DealerPid),
    {ok, Cards} = bj_dealer:deal(DealerPid),
    {ok, #state{dealer = DealerPid, cards = Cards}}.

handle_call(hit, _From, #state{cards = Cards} = State) -> 
    case blackjack_deck:bust(Cards) of
        true -> {reply, {error, bust}, State};
        false -> handle_hit(State)
   end;

handle_call(stick, _From, #state{cards = Cards} = State) -> 
    case blackjack_deck:bust(Cards) of
        true -> {reply, {error, bust}, State};
        false -> handle_stick(State)
   end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

hit(Pid) ->
    gen_server:call(Pid, hit).

stick(Pid) ->
    gen_server:call(Pid, stick).

handle_hit(#state{dealer = DealerPid, cards = Cards} = State) ->
    io:format("Hit me!~n", []),
    {ok, NewCards} = bj_dealer:hit(DealerPid, Cards),
    io:format("New cards: ~p~n", [NewCards]),
    io:format("Possible scores: ~p~n", [blackjack_deck:possible_scores(NewCards)]), 
    NewState = State#state{cards = NewCards},
    case blackjack_deck:bust(NewCards) of
        false -> {reply, ok, NewState};
        true -> {reply, {error, bust}, NewState}
    end.

handle_stick(#state{dealer = DealerPid, cards = Cards} = State) ->
    io:format("Sticking~n", []),
    io:format("Dealers turn~n", []),
    {reply, bj_dealer:play(DealerPid, Cards), State}.

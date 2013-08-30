-module(bj_hand).

-behaviour(gen_fsm).

-export([start_link/1]).
-export([hit/1, play/2]).
-export([init/1, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4, terminate/3]).

-record(state, {cards, bet, dealer}).

start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

hit(Pid) ->
    gen_fsm:sync_send_event(Pid, hit).

init([Cards, Bet, Dealer]) ->
    {ok, play, #state{cards = Cards, bet = Bet, dealer = Dealer}}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

play(hit, #state{dealer = Dealer, cards = Cards} = State) ->
    {Result, NewCards} = hit(Dealer, Cards),
    NewState = State#state{cards = NewCards},
    case Result of
        bust -> {reply, bust, bust, NewState};
        ok -> {reply, ok, play, NewState}
    end;

play(double, #state{bet = Bet, cards = Cards, dealer = Dealer} = State) when length(Cards) =:= 2 ->
    {Result, NewCards} = hit(Dealer, Cards),
    NewState = State#state{cards = NewCards, bet = Bet * 2},
    case Result of
        bust -> {reply, bust, bust, NewState};
        ok -> {reply, ok, play, NewState}
    end.

hit(Dealer, Cards) ->
    {ok, NewCard} = bj_dealer:hit(Dealer),
    io:format("New card: ~p~n", [NewCard]),
    NewCards = [NewCard | Cards],
    io:format("Possible scores: ~p~n", [bj_deck:possible_scores(NewCards)]), 
    case bj_deck:bust(NewCards) of
        true -> {bust, NewCards};
        false -> {ok, NewCards}
    end.

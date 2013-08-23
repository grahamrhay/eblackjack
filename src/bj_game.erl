-module(bj_game).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0, bet/2, hit/1, stick/1, double/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {phase = taking_bets, bet = 0, dealer, cards = []}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, DealerPid} = supervisor:start_child(bj_dealer_sup, []),
    {ok, #state{dealer = DealerPid, phase = taking_bets}}.

handle_call({bet, Amount}, _From, #state{phase = Phase, dealer = DealerPid} = State) ->
    case Phase of
        taking_bets -> 
            ok = bj_dealer:shuffle(DealerPid),
            {ok, Cards, DealerCard} = bj_dealer:deal(DealerPid),
            NewState = State#state{phase = players_turn, cards = Cards, bet = Amount},
            {reply, {ok, Cards, DealerCard}, NewState};
        _ -> 
            {reply, {error, Phase}, State}
    end;

handle_call(hit, _From, #state{phase = Phase} = State) -> 
    case Phase of
        players_turn ->
            io:format("Hit me!~n", []),
            hit_me(State);
        _ ->
            {reply, {error, Phase}, State}
    end;

handle_call(double, _From, #state{phase = Phase} = State) -> 
    case Phase of
        players_turn ->
            io:format("Double down!~n", []),
            double_down(State);
        _ ->
            {reply, {error, Phase}, State}
    end;

handle_call(stick, _From, #state{phase = Phase} = State) -> 
    case Phase of
        players_turn -> handle_stick(State);
        _ -> {reply, {error, Phase}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

bet(Pid, Amount) ->
    gen_server:call(Pid, {bet, Amount}).

hit(Pid) ->
    gen_server:call(Pid, hit).

stick(Pid) ->
    gen_server:call(Pid, stick).

double(Pid) ->
    gen_server:call(Pid, double).

hit_me(#state{dealer = DealerPid, cards = Cards} = State) ->
    {ok, NewCard} = bj_dealer:hit(DealerPid),
    io:format("New card: ~p~n", [NewCard]),
    NewCards = [NewCard | Cards],
    io:format("Possible scores: ~p~n", [bj_deck:possible_scores(NewCards)]), 
    NewState = State#state{cards = NewCards},
    case bj_deck:bust(NewCards) of
        false -> {reply, ok, NewState};
        true -> {reply, {error, bust}, #state{dealer = DealerPid}}
    end.

double_down(#state{bet = Bet} = State) ->
    case hit_me(State) of
        {reply, {error, bust}, NewState} -> {reply, {error, bust}, NewState};
        {reply, ok, NewState} -> handle_stick(NewState#state{bet = Bet * 2})
    end.

handle_stick(#state{dealer = DealerPid, cards = Cards, bet = Bet}) ->
    io:format("Sticking~n", []),
    io:format("Dealers turn~n", []),
    Result = case bj_dealer:play(DealerPid, Cards) of
        blackjack ->
            Winnings = Bet + (Bet * 1.5), % 3:2 for blackjack
            {win, Winnings};
        win ->
            Winnings = Bet * 2, % 2:1 for a win
            {win, Winnings};
        push -> {push, Bet}; % stake back for a draw
        lose -> lose % nothing for a loss
    end,
    {reply, Result, #state{dealer = DealerPid}}.

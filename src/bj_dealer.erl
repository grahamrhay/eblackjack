-module(bj_dealer).

-behaviour(gen_server).

-export([start_link/0, shuffle/1, deal/1, hit/2, play/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {deck, cards}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    random:seed(now()), 
    {ok, #state{}}.

handle_call(shuffle, _From, State) ->
    Deck = bj_deck:new(),
    {reply, ok, State#state{deck = Deck}};

handle_call(deal, _From, #state{deck = Deck} = State) ->
    {NewDeck, PlayerCards, DealerCards} = initial_deal(Deck),
    io:format("Player cards: ~p~n", [PlayerCards]),
    io:format("Possible scores: ~p~n", [bj_deck:possible_scores(PlayerCards)]),
    io:format("Dealer cards: ~p~n", [DealerCards]),
    io:format("Possible scores: ~p~n", [bj_deck:possible_scores(DealerCards)]), 
    [_HoleCard, UpCard] = DealerCards,
    {reply, {ok, PlayerCards, UpCard}, State#state{deck = NewDeck, cards = DealerCards}};

handle_call({hit, Cards}, _From, #state{deck = Deck} = State) ->
    {NewDeck, NewCards} = bj_deck:hit(Deck, Cards),
    {reply, {ok, NewCards}, State#state{deck = NewDeck}};

handle_call({play, PlayerCards}, _From, #state{deck = Deck, cards = DealerCards} = State) ->
    {NewDealerCards, NewDeck} = dealers_turn(DealerCards, Deck),
    NewState = State#state {deck = NewDeck, cards = NewDealerCards},
    case bj_deck:bust(NewDealerCards) of
        true -> 
            io:format("Dealer bust, you win!~n", []),
            {reply, win, NewState};
        false ->
            declare_winner(PlayerCards, NewDealerCards, NewState)
   end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

shuffle(Pid) ->
    gen_server:call(Pid, shuffle).

deal(Pid) ->
    gen_server:call(Pid, deal).

hit(Pid, Cards) ->
    gen_server:call(Pid, {hit, Cards}).

play(Pid, PlayerCards) ->
    gen_server:call(Pid, {play, PlayerCards}).

initial_deal(Deck) ->
    initial_deal(Deck, [], [], 2).

initial_deal(Deck, PlayerCards, DealerCards, 0) ->
    {Deck, PlayerCards, DealerCards};

initial_deal(Deck, PlayerCards, DealerCards, NumOfCards) ->
    [Card1, Card2 | Remainder] = Deck,
    initial_deal(Remainder, [Card1 | PlayerCards], [Card2 | DealerCards], NumOfCards - 1).

dealer_plays(Cards) ->
    lists:all(fun(Score) -> Score < 17 end, bj_deck:possible_scores(Cards)).

dealers_turn(Cards, Deck) ->
    case dealer_plays(Cards) of
        true ->
            io:format("Dealer hit~n", []),
            {NewDeck, NewCards} = bj_deck:hit(Deck, Cards),
            io:format("New dealer cards: ~p~n", [NewCards]),
            io:format("Possible scores: ~p~n", [bj_deck:possible_scores(NewCards)]),
            dealers_turn(NewCards, NewDeck);
        false ->
            {Cards, Deck}
    end.

declare_winner(PlayerCards, DealerCards, State) ->
    case bj_deck:winner(PlayerCards, DealerCards) of
        player ->
            io:format("You win!~n", []),
            {reply, win, State};
        dealer ->
            io:format("You lose :(~n", []),
            {reply, lose, State};
        push ->
            io:format("A draw!~n", []),
            {reply, push, State}
    end. 

-module(bj_dealer_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../src/blackjack.hrl").

-define(CARD(Card, Suit), #bj_card{card = Card, suit = Suit}).

shuffle_test() ->
    {reply, ok, {state, Deck, _}} = bj_dealer:handle_call(shuffle, from, {state, undefined, undefined}),
    ?assertEqual(true, is_list(Deck)),
    ?assertEqual(52, length(Deck)).

initial_deal_player_cards_test() ->
    Card1 = ?CARD(1, clubs),
    Card3 = ?CARD(3, diamonds),
    Deck = [Card1, ?CARD(2, hearts), Card3, ?CARD(4, spades)],
    {reply, {ok, PlayerCards}, _} = bj_dealer:handle_call(deal, from, {state, Deck, undefined}),
    ?assertEqual([Card3, Card1], PlayerCards).

initial_deal_dealer_cards_test() ->
    Card2 = ?CARD(2, clubs),
    Card4 = ?CARD(4, diamonds),
    Deck = [?CARD(1, hearts), Card2, ?CARD(3, spades), Card4],
    {reply, {ok, _}, {state, _, DealerCards}} = bj_dealer:handle_call(deal, from, {state, Deck, undefined}),
    ?assertEqual([Card4, Card2], DealerCards).

hit_test() ->
    Card = ?CARD(8, clubs),
    Deck = [Card],
    {reply, {ok, Cards}, {state, NewDeck, _}} = bj_dealer:handle_call({hit, []}, from, {state, Deck, undefined}),
    ?assertEqual([], NewDeck),
    ?assertEqual([Card], Cards).

play_dealer_bust_test() ->
    PlayerCards = [?CARD(king, spades), ?CARD(8, hearts)],
    DealerCards = [?CARD(10, clubs), ?CARD(6, diamonds)],
    Deck = [?CARD(8, clubs)], % 24 for the dealer
    {reply, Result, _} = bj_dealer:handle_call({play, PlayerCards}, from, {state, Deck, DealerCards}),
    ?assertEqual(win, Result).

play_player_win_test() ->
    PlayerCards = [?CARD(king, spades), ?CARD(9, hearts)],
    DealerCards = [?CARD(10, clubs), ?CARD(4, diamonds)],
    Deck = [?CARD(4, clubs)], % 24 for the dealer
    {reply, Result, _} = bj_dealer:handle_call({play, PlayerCards}, from, {state, Deck, DealerCards}),
    ?assertEqual(win, Result).

play_dealer_win_test() ->
    PlayerCards = [?CARD(king, spades), ?CARD(9, hearts)],
    DealerCards = [?CARD(10, clubs), ?CARD(5, diamonds)],
    Deck = [?CARD(5, clubs)], % 24 for the dealer
    {reply, Result, _} = bj_dealer:handle_call({play, PlayerCards}, from, {state, Deck, DealerCards}),
    ?assertEqual(lose, Result).

play_push_test() ->
    PlayerCards = [?CARD(king, spades), ?CARD(9, hearts)],
    DealerCards = [?CARD(10, clubs), ?CARD(5, diamonds)],
    Deck = [?CARD(4, clubs)], % 24 for the dealer
    {reply, Result, _} = bj_dealer:handle_call({play, PlayerCards}, from, {state, Deck, DealerCards}),
    ?assertEqual(push, Result).

-module(blackjack_deck_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../src/blackjack.hrl").

new_test() ->
    Deck = blackjack_deck:new(),
    ?assertEqual(52, length(Deck)),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#blackjack_card.suit =:= clubs end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#blackjack_card.suit =:= hearts end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#blackjack_card.suit =:= spades end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#blackjack_card.suit =:= diamonds end, Deck))).

possible_scores_test() ->
    ?assertEqual([1, 11], blackjack_deck:possible_scores([#blackjack_card { card = ace }])),
    ?assertEqual([9], blackjack_deck:possible_scores([#blackjack_card { card = 9 }])),
    ?assertEqual([10], blackjack_deck:possible_scores([#blackjack_card { card = jack }])),
    ?assertEqual([10], blackjack_deck:possible_scores([#blackjack_card { card = queen }])),
    ?assertEqual([10], blackjack_deck:possible_scores([#blackjack_card { card = king }])),
    ?assertEqual([2, 12, 22], blackjack_deck:possible_scores([#blackjack_card { card = ace }, #blackjack_card { card = ace }])),
    ?assertEqual([19, 29], blackjack_deck:possible_scores([#blackjack_card { card = ace }, #blackjack_card { card = 8 }, #blackjack_card { card = king }])).

hit_test() ->
    Card = #blackjack_card { card = ace, suit = spades },
    Deck = [Card],
    {NewDeck,NewCards} = blackjack_deck:hit(Deck, []),
    ?assertEqual([], NewDeck),
    ?assertEqual([Card], NewCards).

bust_test() ->
    ?assertEqual(false, blackjack_deck:bust([#blackjack_card { card = ace }])), % 1
    ?assertEqual(false, blackjack_deck:bust([#blackjack_card { card = king }, #blackjack_card { card = king }])), % 20
    ?assertEqual(false, blackjack_deck:bust([#blackjack_card { card = ace }, #blackjack_card { card = king }, #blackjack_card { card = 3 }])), % 14, 24
    ?assertEqual(true, blackjack_deck:bust([#blackjack_card { card = jack }, #blackjack_card { card = jack }, #blackjack_card { card = 2 }])). % 22

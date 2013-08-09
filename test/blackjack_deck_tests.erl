-module(blackjack_deck_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../src/blackjack.hrl").

new_test() ->
    Deck = blackjack_deck:new(),
    ?assertEqual(52, length(Deck)).

possible_scores_test() ->
    ?assertEqual([1, 11], blackjack_deck:possible_scores([#blackjack_card { card = ace }])),
    ?assertEqual([9], blackjack_deck:possible_scores([#blackjack_card { card = 9 }])),
    ?assertEqual([10], blackjack_deck:possible_scores([#blackjack_card { card = jack }])),
    ?assertEqual([10], blackjack_deck:possible_scores([#blackjack_card { card = queen }])),
    ?assertEqual([10], blackjack_deck:possible_scores([#blackjack_card { card = king }])),
    ?assertEqual([2, 12, 22], blackjack_deck:possible_scores([#blackjack_card { card = ace }, #blackjack_card { card = ace }])),
    ?assertEqual([19, 29], blackjack_deck:possible_scores([#blackjack_card { card = ace }, #blackjack_card { card = 8 }, #blackjack_card { card = king }])).

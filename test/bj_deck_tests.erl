-module(bj_deck_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../src/blackjack.hrl").

-define(CARD(Card), #blackjack_card{ card = Card }).

new_test() ->
    Deck = bj_deck:new(),
    ?assertEqual(52, length(Deck)),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#blackjack_card.suit =:= clubs end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#blackjack_card.suit =:= hearts end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#blackjack_card.suit =:= spades end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#blackjack_card.suit =:= diamonds end, Deck))).

possible_scores_test() ->
    ?assertEqual([1, 11], bj_deck:possible_scores([?CARD(ace)])),
    ?assertEqual([9], bj_deck:possible_scores([?CARD(9)])),
    ?assertEqual([10], bj_deck:possible_scores([?CARD(jack)])),
    ?assertEqual([10], bj_deck:possible_scores([?CARD(queen)])),
    ?assertEqual([10], bj_deck:possible_scores([?CARD(king)])),
    ?assertEqual([2, 12, 22], bj_deck:possible_scores([?CARD(ace), ?CARD(ace)])),
    ?assertEqual([19, 29], bj_deck:possible_scores([?CARD(ace), ?CARD(8), ?CARD(king)])).

hit_test() ->
    Card = ?CARD(ace),
    Deck = [Card],
    {NewDeck,NewCards} = bj_deck:hit(Deck, []),
    ?assertEqual([], NewDeck),
    ?assertEqual([Card], NewCards).

bust_test() ->
    ?assertEqual(false, bj_deck:bust([?CARD(ace)])), % 1
    ?assertEqual(false, bj_deck:bust([?CARD(king), ?CARD(king)])), % 20
    ?assertEqual(false, bj_deck:bust([?CARD(ace), ?CARD(king), ?CARD(3)])), % 14, 24
    ?assertEqual(true, bj_deck:bust([?CARD(jack), ?CARD(jack), ?CARD(2)])). % 22

dealer_plays_test() ->
    ?assertEqual(true, bj_deck:dealer_plays([?CARD(king)])), % 16
    ?assertEqual(false, bj_deck:dealer_plays([?CARD(8), ?CARD(9)])), % hard 17
    ?assertEqual(false, bj_deck:dealer_plays([?CARD(ace), ?CARD(6)])), % soft 17
    ?assertEqual(false, bj_deck:dealer_plays([?CARD(king), ?CARD(10)])). % 20

winner_test() ->
    ?assertEqual(dealer, bj_deck:winner([?CARD(king), ?CARD(jack)], [?CARD(7), ?CARD(7), ?CARD(7)])), % 20 vs 21
    ?assertEqual(player, bj_deck:winner([?CARD(king), ?CARD(jack)], [?CARD(10), ?CARD(7)])), % 20 vs 17
    ?assertEqual(push, bj_deck:winner([?CARD(king), ?CARD(jack)], [?CARD(7), ?CARD(7), ?CARD(6)])), % 20 vs 20
    ?assertEqual(dealer, bj_deck:winner([?CARD(king), ?CARD(jack)], [?CARD(ace), ?CARD(7), ?CARD(3)])), % 20 vs soft 21
    ?assertEqual(dealer, bj_deck:winner([?CARD(king), ?CARD(jack)], [?CARD(ace), ?CARD(10), ?CARD(10)])). % 20 vs hard 21

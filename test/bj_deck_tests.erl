-module(bj_deck_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../src/blackjack.hrl").

-define(CARD(Card), #bj_card{ card = Card }).

new_test() ->
    Deck = bj_deck:new(),
    ?assertEqual(52, length(Deck)),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#bj_card.suit =:= clubs end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#bj_card.suit =:= hearts end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#bj_card.suit =:= spades end, Deck))),
    ?assertEqual(13, length(lists:filter(fun(C) -> C#bj_card.suit =:= diamonds end, Deck))).

possible_scores_test() ->
    ?assertEqual([1, 11], bj_deck:possible_scores([?CARD(ace)])),
    ?assertEqual([9], bj_deck:possible_scores([?CARD(9)])),
    ?assertEqual([10], bj_deck:possible_scores([?CARD(jack)])),
    ?assertEqual([10], bj_deck:possible_scores([?CARD(queen)])),
    ?assertEqual([10], bj_deck:possible_scores([?CARD(king)])),
    ?assertEqual([2, 12, 22], bj_deck:possible_scores([?CARD(ace), ?CARD(ace)])),
    ?assertEqual([19, 29], bj_deck:possible_scores([?CARD(ace), ?CARD(8), ?CARD(king)])),
    ?assertEqual([blackjack], bj_deck:possible_scores([?CARD(ace), ?CARD(king)])),
    ?assertEqual([blackjack], bj_deck:possible_scores([?CARD(10), ?CARD(ace)])),
    ?assertEqual([11,21], bj_deck:possible_scores([?CARD(ace), ?CARD(7), ?CARD(3)])).

hit_test() ->
    Card = ?CARD(ace),
    Deck = [Card],
    {NewDeck,NewCard} = bj_deck:hit(Deck),
    ?assertEqual([], NewDeck),
    ?assertEqual(Card, NewCard).

bust_test() ->
    ?assertEqual(false, bj_deck:bust([?CARD(ace)])), % 1
    ?assertEqual(false, bj_deck:bust([?CARD(king), ?CARD(king)])), % 20
    ?assertEqual(false, bj_deck:bust([?CARD(ace), ?CARD(king), ?CARD(3)])), % 14, 24
    ?assertEqual(true, bj_deck:bust([?CARD(jack), ?CARD(jack), ?CARD(2)])). % 22

result_test() ->
    ?assertEqual(dealer_win, bj_deck:result([?CARD(king), ?CARD(jack)], [?CARD(7), ?CARD(7), ?CARD(7)])), % 20 vs 21
    ?assertEqual(player_win, bj_deck:result([?CARD(king), ?CARD(jack)], [?CARD(10), ?CARD(7)])), % 20 vs 17
    ?assertEqual(push, bj_deck:result([?CARD(king), ?CARD(jack)], [?CARD(7), ?CARD(7), ?CARD(6)])), % 20 vs 20
    ?assertEqual(dealer_win, bj_deck:result([?CARD(king), ?CARD(jack)], [?CARD(ace), ?CARD(7), ?CARD(3)])), % 20 vs soft 21
    ?assertEqual(dealer_win, bj_deck:result([?CARD(king), ?CARD(jack)], [?CARD(ace), ?CARD(10), ?CARD(10)])), % 20 vs hard 21
    ?assertEqual(blackjack, bj_deck:result([?CARD(king), ?CARD(ace)], [?CARD(10), ?CARD(9)])), % blackjack vs 19
    ?assertEqual(push, bj_deck:result([?CARD(king), ?CARD(ace)], [?CARD(ace), ?CARD(10)])), % blackjack vs blackjack
    ?assertEqual(dealer_win, bj_deck:result([?CARD(king), ?CARD(queen)], [?CARD(ace), ?CARD(10)])). % 20 vs blackjack

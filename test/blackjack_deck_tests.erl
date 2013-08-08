-module(blackjack_deck_tests).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
    Deck = blackjack_deck:new(),
    ?assertEqual(52, length(Deck)).

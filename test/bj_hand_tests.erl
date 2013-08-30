-module(bj_hand_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/blackjack.hrl").

-define(CARD(Card, Suit), #bj_card{card = Card, suit = Suit}).

play_hit_bust_test() ->
    Cards = [?CARD(5, clubs), ?CARD(8, diamonds)],
    NextCard = ?CARD(king, hearts),
    FakeDealer = spawn(fun() -> 
        receive
            {_,{Pid, Ref},_} -> Pid ! {Ref, NextCard}
        end
    end),
    State = {state, Cards, 10, FakeDealer}, 
    {reply, Reply, NextState, _} = bj_hand:play(hit, State),
    ?assertEqual(bust, NextState),
    ?assertEqual(bust, Reply).

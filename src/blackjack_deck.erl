-module(blackjack_deck).

-include_lib("../src/blackjack.hrl").

-export([new/0]).

new() ->
    Cards = lists:seq(1, 13),
    Suits = [spades, clubs, hearts, diamonds],
    Deck = lists:append(lists:map(fun(C) -> lists:map(fun(S) -> #blackjack_card{suit=S, card=C} end, Suits) end, Cards)),
    shuffle(Deck).

shuffle(Deck) ->
    % Fisher-Yates
    shuffle(Deck, []).

shuffle([], Acc) -> Acc;

shuffle(Deck, Acc) ->
    SplitPoint = random:uniform(length(Deck)) - 1,
    {Leading, [H|T]} = lists:split(SplitPoint, Deck),
    shuffle(Leading ++ T, [H | Acc]).

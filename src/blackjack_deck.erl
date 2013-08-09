-module(blackjack_deck).

-include_lib("../src/blackjack.hrl").

-export([new/0, initial_deal/1, possible_scores/1]).

new() ->
    Cards = [ace] ++ lists:seq(2, 10) ++ [jack, queen, king],
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

initial_deal(Deck) ->
    initial_deal(Deck, [], [], 2).

initial_deal(Deck, PlayerCards, DealerCards, 0) ->
    {Deck, PlayerCards, DealerCards};

initial_deal(Deck, PlayerCards, DealerCards, NumOfCards) ->
    [Card1, Card2 | Remainder] = Deck,
    initial_deal(Remainder, [Card1 | PlayerCards], [Card2 | DealerCards], NumOfCards - 1).

possible_scores(Cards) ->
    possible_scores(Cards, [0]).

possible_scores([], Scores) ->
    Scores;

possible_scores(Cards, Scores) ->
    [FirstCard | Rest] = Cards,
    PossibleScores = case FirstCard#blackjack_card.card of
        ace -> lists:usort(lists:map(fun(S) -> S + 1 end, Scores) ++ lists:map(fun(S) -> S + 11 end, Scores));
        jack -> lists:map(fun(S) -> S + 10 end, Scores);
        queen -> lists:map(fun(S) -> S + 10 end, Scores);
        king -> lists:map(fun(S) -> S + 10 end, Scores);
        Card -> lists:map(fun(S) -> S + Card end, Scores)
    end,
    possible_scores(Rest, PossibleScores).

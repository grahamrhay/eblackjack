-module(bj_deck).

-include_lib("../src/blackjack.hrl").

-export([new/0, possible_scores/1, hit/1, bust/1, result/2]).

new() ->
    Cards = [ace] ++ lists:seq(2, 10) ++ [jack, queen, king],
    Suits = [spades, clubs, hearts, diamonds],
    Deck = lists:append(lists:map(fun(C) -> lists:map(fun(S) -> #bj_card{suit=S, card=C} end, Suits) end, Cards)),
    shuffle(Deck).

shuffle(Deck) ->
    % Fisher-Yates
    shuffle(Deck, []).

shuffle([], Acc) -> Acc;

shuffle(Deck, Acc) ->
    SplitPoint = random:uniform(length(Deck)) - 1,
    {Leading, [H|T]} = lists:split(SplitPoint, Deck),
    shuffle(Leading ++ T, [H | Acc]).

possible_scores(Cards) ->
    Scores = possible_scores(Cards, [0]),
    case Scores of
        [11,21] when length(Cards) =:= 2 -> [blackjack];
        _ -> Scores
    end.

possible_scores([], Scores) ->
    Scores;

possible_scores(Cards, Scores) ->
    [FirstCard | Rest] = Cards,
    AllPossibleScores = lists:append(lists:map(fun(S) -> lists:map(fun(Val) -> Val + S end, card_values(FirstCard)) end, Scores)),
    PossibleScores = lists:usort(AllPossibleScores), % remove any duplicate scores
    possible_scores(Rest, PossibleScores).

card_values(Card) ->
    case Card#bj_card.card of
        ace -> [1, 11];
        jack -> [10];
        queen -> [10];
        king -> [10];
        Val -> [Val]
    end. 

hit(Deck) ->
    [NextCard | NewDeck] = Deck,
    {NewDeck, NextCard}.

bust(Cards) ->
    lists:all(fun(Score) -> Score > 21 end, possible_scores(Cards)).

result(PlayerCards, DealerCards) ->
    PlayerScore = highest_valid_score(PlayerCards),
    DealerScore = highest_valid_score(DealerCards),
    case PlayerScore of
        blackjack ->
            case DealerScore of
                blackjack -> push;
                _ -> blackjack
            end;
        _ ->
            case DealerScore of
                blackjack -> dealer_win;
                _ ->
                    case PlayerScore > DealerScore of
                        true -> player_win;
                        false ->
                            case DealerScore > PlayerScore of
                                true -> dealer_win;
                                false -> push
                            end
                    end
            end
    end.

highest_valid_score(Cards) ->
    Scores = possible_scores(Cards),
    case Scores of
        [blackjack] -> blackjack;
        _ -> lists:max(lists:filter(fun(Score) -> Score =< 21 end, Scores))
    end.

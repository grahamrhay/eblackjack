-module(blackjack).

-behaviour(gen_server).

-export([start_link/0, start/0, new_game/0, hit_me/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { deck, player_cards, dealer_cards }).

start() ->
    application:start(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    random:seed(now()),
    {ok, []}.

handle_call(new_game, _From, _State) ->
    io:format("New game!~n", []),
    Deck = blackjack_deck:new(),
    io:format("Deck: ~p~n", [Deck]),
    {RemainderOfDeck, PlayerCards, DealerCards} = blackjack_deck:initial_deal(Deck),
    io:format("Player cards: ~p~n", [PlayerCards]),
    io:format("Possible scores: ~p~n", [blackjack_deck:possible_scores(PlayerCards)]),
    io:format("Dealer cards: ~p~n", [DealerCards]),
    io:format("Possible scores: ~p~n", [blackjack_deck:possible_scores(DealerCards)]),
    {reply, ok, #state{ deck = RemainderOfDeck, player_cards = PlayerCards, dealer_cards = DealerCards }};

handle_call(hit, _From, #state{ deck = Deck, player_cards = PlayerCards} = State) -> 
    case blackjack_deck:bust(PlayerCards) of
        true -> {reply, {error, bust}, State};
        false -> 
            io:format("Hit me!~n", []),
            {NewDeck, NewPlayerCards} = blackjack_deck:hit(Deck, PlayerCards),
            io:format("New cards: ~p~n", [NewPlayerCards]),
            io:format("Possible scores: ~p~n", [blackjack_deck:possible_scores(NewPlayerCards)]), 
            NewState = State#state{deck = NewDeck, player_cards = NewPlayerCards },
            case blackjack_deck:bust(NewPlayerCards) of
                false -> {reply, ok, NewState};
                true -> {reply, {error, bust}, NewState}
            end
    end;

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, N) ->
    {noreply, N}.

handle_info(_Msg, N) ->
    {noreply, N}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

new_game() ->
    gen_server:call(?MODULE, new_game).

hit_me() ->
    gen_server:call(?MODULE, hit).

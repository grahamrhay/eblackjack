-module(blackjack).

-behaviour(gen_server).

-export([start_link/0, start/0, new_game/0]).

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
    {ok, #state{}}.

handle_call(new_game, _From, _State) ->
    io:format("New game!~n", []),
    Deck = blackjack_deck:new(),
    {Deck2,PlayerCards} = deal(Deck, []),
    {Deck3,DealerCards} = deal(Deck2, []),
    {Deck4, PlayerCards2} = deal(Deck3, PlayerCards),
    io:format("Player cards: ~p~n", [PlayerCards2]),
    {Deck5, DealerCards2} = deal(Deck4, DealerCards),
    io:format("Dealer cards: ~p~n", [DealerCards2]),
    {reply, [], #state{ deck = Deck5, player_cards = PlayerCards2, dealer_cards = DealerCards2 }};

handle_call(_, _From, State) ->
    {reply, [], State}.

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

deal(Deck, Cards) ->
    [NextCard | Remainder] = Deck,
    {Remainder, [NextCard | Cards]}.

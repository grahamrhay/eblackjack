-module(bj_dealer_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = simple_one_for_one,
    ChildSpec = ?CHILD(bj_dealer, worker),
    {ok, { {RestartStrategy, 5, 10}, [ChildSpec]} }.

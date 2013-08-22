eBlackjack
==========

Spike of an Erlang/OTP Blackjack server. (Tested against R15B01).

Installing Erlang
-----------------

Take a look at the guide [here](http://docs.basho.com/riak/latest/ops/building/installing/erlang/).

Build
-----
    ./rebar compile

Test
----
    ./rebar eunit

Run
---
    ./start.sh

Usage
-----
Start the server:

    blackjack:start().

Start a new game:

    {ok, Pid} = blackjack:new_game().

Make a bet:

    bj_game:bet(Pid, 10).

Hit:

    bj_game:hit(Pid).

Stick:

    bj_game:stick(Pid).

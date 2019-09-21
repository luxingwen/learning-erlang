-module(m8ball).

-behaviour(application).

-export([start/2, stop/1]).

-export([ask/1]).


start(normal, []) ->
	io:format("start normal-->"),
	m8ball_sup:start_link();

start({takeover, OtherNode}, []) ->
	io:format("take over other Node = ~p~n", [OtherNode]),
	m8ball_sup:start_link().

stop(_State) ->
	ok.

ask(Question) ->
	m8ball_server:ask(Question).

	
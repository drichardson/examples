%
% Erlang Programming Exercise 4-1: An Echo Server
%

-module(echo).
-export([start/0,print/1,stop/0,echo_proc/0]).

start() ->
	register(echo, spawn(?MODULE, echo_proc, [])),
	ok.
	
print(Term) ->
	echo ! {print, Term},
	ok.

stop() ->
	echo ! stop,
	ok.

echo_proc() ->
	receive
		stop -> ok;
		{print, Term} ->
			io:format("~w~n", [Term]),
			echo_proc()
	end.
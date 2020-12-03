% Run main with
% $ escript frequency.erl

%-export ([runner/0]).

main(Arguments) ->
	io:format("String is ~s~n", [string:join(Arguments, ", ")]),
	frequency:start(),
	spawn(runner),
	spawn(runner),
	spawn(runner),
	spawn(runner),
	spawn(runner),
	spawn(runner),
	spawn(runner),
	spawn(runner),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:deallocate(13)]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("~w~n", [frequency:allocate()]),
	io:format("Waiting a bit..."),
	timer:sleep(5000),
	io:format("Done~n"),
	frequency:stop(),
	halt(0).

runner() ->
	Freq=frequency:allocate(),
	io:format("~w: allocate ~w~n", [Freq]),
	timer:sleep(2000),
	io:format("~w: deallocate ~w~n", [frequency:deallocate(Freq)]).

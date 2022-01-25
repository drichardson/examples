-module(afile_client).
-export([run/0]).


run() ->
	Server = afile_server:start("."),
	Server ! {self(), list_dir},
	receive
		{_, {ok, Items}} -> io:format("Got directories: ~s", [string:join(Items, " ")])
	end,
	io:format("1~n"),
	Filename = "Makefile.BAD",
	Server ! {self(), get_file, Filename},
	io:format("2~n"),
	receive
		{Pid, {ok, Body}} -> io:format("CONTENT from ~w~n====~s BEGIN~n~s====~s END~n",
					       [Pid, Filename, Body, Filename]);
		{Pid, {error, Reason}} -> io:format("FAILURE from ~w: ~w~n", Pid, Reason);
		Err -> io:format("OTHER ~w~n", [Err])
	end,
	io:format("3~n"),
	init:stop().



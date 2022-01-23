-module(afile_client).
-export([run/0]).

run() ->
	Server = afile_server:start("."),
	Server ! {self(), list_dir},
	receive
		{_, {ok, Items}} -> io:format("Got directories: ~s", [string:join(Items, " ")])
	end,
	io:format("1~n"),
	Server ! {self(), get_file, "afile_client.erl"},
	io:format("2~n"),
	receive
		{_, {ok, Body}} -> io:format("CONTENT~n~s", [Body]);
		_ -> io:format("OTHER~n")
	end,
	io:format("3~n").



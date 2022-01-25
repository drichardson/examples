-module(afile_runner).
-export([run/0]).

run() ->
	Server = afile_server:start("."),
	{ok, Items} = afile_client:ls(Server),
	io:format("Got directories: ~s", [string:join(Items, " ")]),
	Filename = "Makefile",
	{ok, Body} = afile_client:get_file(Server, Filename),
	io:format("====BEGIN ~s====~n~s====END ~s====~n",
		  [Filename, Body, Filename]),
	init:stop().

-module(afile_server).
-export([start/1, loop/1]).

start(Dir) -> spawn(afile_server, loop, [Dir]).

loop(Dir) ->
	receive
		{Client, list_dir} -> 
			io:format("SERVER: list-dir~n"),
			Client ! {self(), file:list_dir(Dir)};
		{Client, get_file, File} ->
			io:format("SERVER: get_file~n"),
			Path = filename:join(Dir, File),
			Client ! {self(), file:read_file(Path)}
	end,
	loop(Dir).

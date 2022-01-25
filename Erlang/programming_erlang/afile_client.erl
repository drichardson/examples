-module(afile_client).
-export([ls/1, get_file/2]).


ls(Server) ->
	Server ! {self(), list_dir},
	receive
		{_, Items} -> Items
	end.

get_file(Server, File) ->
	Server ! {self(), get_file, File},
	receive
		{_, Content} -> Content
	end.

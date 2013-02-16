%
% Exercise 5-1: A Database Server
%
-module (my_db).
-export ([start/0,stop/0,write/2,delete/1,read/1,match/1]).
-export ([init/0]).

start() ->
	% NOTE: If start is called more than once, init will continue to be spawned but it
	% will not be registered with my_db which means it will never be killed. Not sure how to
	% handle that situation.
	register(my_db, spawn(?MODULE, init, [])),
	ok.

init() ->
	loop(db:new()).

loop(Db) ->
	receive
		{write, Key, Element} -> loop(db:write(Key, Element, Db));
		{delete, Key} -> loop(db:delete(Key, Db));
		{read, Key, Pid} -> Pid ! db:read(Key, Db), loop(Db);
		{match, Element, Pid} -> Pid ! db:match(Element, Db), loop(Db);
		stop -> db:destroy(Db)
	end.

write(Key, Element) ->
	my_db ! {write, Key, Element},
	ok.

delete(Key) ->
	my_db ! {delete, Key},
	ok.

read(Key) ->
	my_db ! {read, Key, self()},
	receive
		Result -> Result
	end.

match(Element) ->
	my_db ! {match, Element, self()},
	receive
		Result -> Result
	end.

stop() ->
	my_db ! stop,
	ok.


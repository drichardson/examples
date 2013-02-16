-module(records).
-export([run/0]).

-record(person, {first_name, last_name, sex}).

-record(side, {p1=4, p2=4, p3=4, p4=4, p5=4, p6=4, mancala=0}).
-record(board, {side1=#side{}, side2=#side{}}).

run() ->
	Doug=#person{first_name="Doug", last_name="Richardson", sex=male},
	io:format("Doug is ~w~n", [Doug]),
	io:format("Doug's first name is ~s, last name is ~s, sex is ~w~n", [Doug#person.first_name, Doug#person.last_name, Doug#person.sex]),
	io:format("#person.first_name is ~w~n", [#person.first_name]),
	Board=#board{},
	io:format("Board is ~w~n", [Board]),
	io:format("Board side1 is ~w, side2 is ~w~n", [Board#board.side1, Board#board.side2]),
	io:format("Board side1 p1 is ~w, side2 mancala is ~w~n", [Board#board.side1#side.p1, Board#board.side1#side.mancala]),
	ok.
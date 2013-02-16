-module (board).
-export ([new/0, print/1, mancala/2, game_over/1]).

new() ->
	{4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0}.
	%{4, 4, 4, 4, 4, 4, 11, 0, 0, 0, 0, 0, 0, 12}.

mancala(Board, Player) ->
	MancalaIndex = case Player of
		player1 -> 7;
		player2 -> 14
	end,
	element(MancalaIndex, Board).

print(Board) ->
	{P1, P2, P3, P4, P5, P6, M1, P8, P9, P10, P11, P12, P13, M2} = Board,
	io:format(
		"---------------------------~n"
		"  ~w, ~w, ~w, ~w, ~w, ~w   ~n"
		"~w                  ~w ~n"
		"  ~w, ~w, ~w, ~w, ~w, ~w   ~n"
		"---------------------------~n",
		[P8, P9, P10, P11, P12, P13, M1, M2, P6, P5, P4, P3, P2, P1]),
	ok.

game_over(Board) ->
	case Board of
		{0, 0, 0, 0, 0, 0, _, _, _, _, _, _, _, _} -> true;
		{_, _, _, _, _, _, _, 0, 0, 0, 0, 0, 0, _} -> true;
		_ -> false
	end.
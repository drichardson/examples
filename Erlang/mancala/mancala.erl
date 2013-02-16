-module (mancala).
-export ([play/0]).

play() ->
	io:put_chars("Welcome to Mancala. Enter -1 to quit or 0 for an AI suggested move.\n"),
	take_turn(board:new(), player1).
	
take_turn(Board, Player) ->
	GameOver = board:game_over(Board),
	if
		GameOver ->
			display_final_score(Board);
		true ->
			PitNumber=get_valid_move(Board, Player),
			case PitNumber of
				-1 -> io:format("Later quitter~n");
				_ ->
				{NextPlayer, Board1} = moves:move(Board, Player, PitNumber),
				take_turn(Board1, NextPlayer)
			end
	end.

get_valid_move(Board, Player) ->
	board:print(Board),
	io:format("~w's turn. ", [Player]),
	{Selection, _Rest} = string:to_integer(io:get_line("Select Pit: ")),
	case {Player, Selection} of
		{_, -1} ->
			% Quit command
			-1;
		{_, 0} ->
			% AI suggest move command
			ai:suggest_move(Board,Player);
		_ ->
			case is_valid_move(Board, Player, Selection) of
				true ->
					Selection;
				_ ->
					io:format("Invalid move ~w for ~w~n", [Selection, Player]),
					get_valid_move(Board, Player)
			end
	end.

is_valid_move(Board, player1, Pit) when Pit >= 1, Pit =< 6 ->
	element(Pit, Board) > 0;
is_valid_move(Board, player2, Pit) when Pit >= 8, Pit =< 13 ->
	element(Pit, Board) > 0;
is_valid_move(_, _, _) ->
	false.

display_final_score(Board) ->
	board:print(Board),
	Player1Score = board:mancala(Board, player1),
	Player2Score = board:mancala(Board, player2),
	if
		Player1Score > Player2Score -> Winner = "Player 1 wins.";
		Player2Score > Player1Score -> Winner = "Player 2 wins.";
		true -> Winner = "Draw."
	end,
	io:format("Game over. ~s Player 1: ~w, Player 2: ~w~n", [Winner, Player1Score, Player2Score]).
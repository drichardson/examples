-module (ai).
-export ([suggest_move/2]).
-include ("debug_logging.hrl").

% TODO: You'd think increasing the search depth would make the AI player better, but it doesn't in my
% limited testing. Figure out why. Seems like there may be a bug in the min step of the algorithm.
-define(MAX_SEARCH_DEPTH, 0).

suggest_move(Board, Player) ->
	{BestMove, _Value}=max_move(Board, Player, 0, ?MAX_SEARCH_DEPTH),
	BestMove.

% Entry point into the minimax algorithm. Calls into min_move, which calls back into max_move until
% there are no moves left to evaluate or the max search depth is reached.
max_move(Board, Player, MaxDepth, MaxDepth) ->
	Moves=generate_moves(Board, Player),
	minmax(Board, Player, Moves, MaxDepth, MaxDepth, fun score/4, fun high_to_low_score_sort/2);
max_move(Board, Player, CurrentDepth, MaxDepth) ->
	Moves=generate_moves(Board, Player),
	minmax(Board, Player, Moves, CurrentDepth, MaxDepth, fun min_move/4, fun high_to_low_score_sort/2).

min_move(Board, Player, MaxDepth, MaxDepth) ->
	Moves=generate_moves(Board, other_player(Player)),
	minmax(Board, Player, Moves, MaxDepth, MaxDepth, fun score/4, fun low_to_high_score_sort/2);
min_move(Board, Player, CurrentDepth, MaxDepth) ->
	Moves=generate_moves(Board, other_player(Player)),
	minmax(Board, Player, Moves, CurrentDepth, MaxDepth, fun max_move/4, fun low_to_high_score_sort/2).

% minmax/7 is a template function used to build a minimax algorithm. It is used to parameterize
% the Max and Min parts of the minimax algorithm, since the structure of most of the algorithm
% is the same.
% TODO: Make this concurrent
minmax(Board, Player, Moves, CurrentDepth, MaxDepth, ScoreFunction, SortFunction) ->
	ScoredMoves=lists:map(fun({SelectedPit, BoardForSelectedPit}) ->
			{_UnusedPit, Score}=ScoreFunction(BoardForSelectedPit, Player, CurrentDepth+1, MaxDepth),
			{SelectedPit, Score}
		end, Moves),
	SortedScoredMoves = lists:sort(SortFunction, ScoredMoves),
	?DEBUGLOG("SortedScored at depth ~w is ~w~n", [CurrentDepth, SortedScoredMoves]),
	Result=case SortedScoredMoves of
		[FirstResult|_] -> FirstResult;
		_ ->
			?DEBUGLOG("Can't make any moves for player ~w and board ~w~n", [Player, Board]), 
			{none, score(Board, Player)}
	end,
	?DEBUGLOG("Returning move of ~w, depth ~w~n", [Result, CurrentDepth]),
	Result.	

% the user landed in their own mancala and therefore got to move again.
% Returns possible moves for Player as a list of tuples [ {Pit1, Board1}, {Pit2, Board2} ],
% where PitX may be equal to PitY because the same pit selection
% can lead to multiple board outcomes when PitX leads to the last stone in a move landing in the mancala. The Board
% in each tuple is the final state of the board. For example, suppose player1 has 3 pits with stones and pit 6,
% the one next to the mancala, has a single stone in it. If the user selects pit 6, they will then be able to
% pick from one of the other two mancalas to move, which results in 2 different board outcomes for the initial
% pit selection. For that example, generated moves would return something like this:
% [ {1, Board1}, {3, Board2}, {6, Board3}, {6, Board4} ]
generate_moves(Board, Player) ->
	?DEBUGLOG("generate_moves/2: ~w, ~w~n", [Board, Player]),
	NonZeroPits=indexes_of_non_zero_pits(Board, Player),
	% Make the move, and if it's still the same player, generate more moves.
	Moves=lists:flatten(lists:map(fun(Pit) -> generate_moves(Board, Player, Pit) end, NonZeroPits)),
	Moves.

% Returns a list tuples [ {Pit1, NewBoard1} ] or [ {Pit1, NewBoard1}, {Pit2, NewBoard2} ]
generate_moves(Board, Player, Pit) ->
	?DEBUGLOG("generate_moves/3: ~w, ~w, ~w~n", [Board, Player, Pit]),
	{NewPlayer, NewBoard} = moves:move(Board, Player, Pit),
	case NewPlayer of
		Player ->
			% Same player, keep generating moves.
			GeneratedMoves=generate_moves(NewBoard, Player),
			?DEBUGLOG("  generated moves: ~w~n", [GeneratedMoves]),
			lists:map(fun({_Pit, Board1}) ->
					% We only care about the first move that led to this board, so swap the other pit out for this one.
					{Pit, Board1}
				end, GeneratedMoves);
		_ ->
			% Player changed, done.
			[{Pit, NewBoard}]
	end.

other_player(player1) -> player2;
other_player(player2) -> player1.

% score/4 is an adapter to score/2 that returns a value that matches minimax/7
score(Board, Player, _, _) ->
	{none, score(Board, Player)}.

% score/2 counts the number of pieces in each player's mancala. This
% should be improved using hueristics to take into account boards that, for example, give a
% player two turns or allow capture of another player's pieces.
score(Board, player1) ->
	board:mancala(Board, player1) - board:mancala(Board, player2);
score(Board, player2) ->
	board:mancala(Board, player2) - board:mancala(Board, player1).

% Sort functions used by max_move and min_move
high_to_low_score_sort({_,Score1},{_,Score2}) ->
	Score1 >= Score2.

low_to_high_score_sort({_,Score1},{_,Score2}) ->
	Score1 =< Score2.

% index_list/1 takes list [A, B, C] and returns [{1, A}, {2, B}, {3, C}]. That is, it returns
% a list of 2-tuples where each tuple is {IndexOfElementX, ElementX}.
index_list(List) ->
	element(1, lists:mapfoldl(fun(X, Acc) -> {{Acc, X}, Acc+1} end, 1, List)).

% indexes_of_non_zero_pits/2 returns the list of pits that have stones for the given Player.
indexes_of_non_zero_pits(Board, Player) ->
	Pits=index_list(tuple_to_list(Board)),
	PitsForPlayer = case Player of
		player1 ->
			lists:sublist(Pits, 1, 6);
		player2 ->
			lists:sublist(Pits, 8, 6)
	end,
	NonZeroPits=lists:filter(fun({_Index, Value}) -> Value > 0 end, PitsForPlayer),
	{NonZeroIndexes, _}=lists:unzip(NonZeroPits),
	NonZeroIndexes.

-module (moves).
-export ([move/3]).

% Board is a 14 tuple of integers, where each integer represents the number of stones in the pit.
% Pit 1 is the player 1s rightmost pit, Pit 7 is player 1s mancala, Pit 8 is player 2s rightmost pit,
% and Pit 14 is player 2s mancala.

% Returns tuple {NextPlayer, NewBoard}, where NextPlayer player1 if player 1 should go next and player2 otherwise.
move(Board, player1, PitNumber) when PitNumber >= 1, PitNumber =< 6 ->
	{Stones,Board2}=takeStonesFromPit(Board, PitNumber),
	dropStones(Board2, player1, nextPit(PitNumber), Stones);
move(Board, player2, PitNumber) when PitNumber >= 8, PitNumber =< 13 ->
	{Stones,Board2}=takeStonesFromPit(Board, PitNumber),
	dropStones(Board2, player2, nextPit(PitNumber), Stones).

dropStones(Board, player1, 14, Stones) ->
	% player1 should skip player 2's mancala
	dropStones(Board, player1, 1, Stones);
dropStones(Board, player2, 7, Stones) ->
	% player2 should skip player 1's mancala
	dropStones(Board, player2, 8, Stones);
dropStones(Board, player1, 7, 1) ->
	% if player 1 lands in their own mancala, it's player 1's turn again
	{player1, addStonesToPit(Board, 7, 1)};
dropStones(Board, player2, 14, 1) ->
	% if player 2 lands in their own mancala, it's player 2's turn again
	{player2, addStonesToPit(Board, 14, 1)};
dropStones(Board, player1, PitNumber, 1) when PitNumber >= 1, PitNumber =< 6 ->
	% if player 1 lands on their own side, then steal player 2's corresponding stones
	% if player 1's side is empty, otherwise don't steal.
	case element(PitNumber, Board) of
		0 ->
			% Steal everything from the player2's corresponding side.
			{Player2StonesAcross, Board1} = takeStonesFromPit(Board, pitAcross(PitNumber)),
			Board2=addStonesToPit(Board1, 7, Player2StonesAcross),
			{player2, addStonesToPit(Board2, PitNumber, 1)};
		_ ->
			% Already have stones in the pit, so don't steal anything from the other side.
			{player2, addStonesToPit(Board, PitNumber, 1)}
	end;
dropStones(Board, player2, PitNumber, 1) when PitNumber >= 8, PitNumber =< 13 ->
	% if player 2 lands on their own side, then steal player 1's corresponding stones
	% if player 2's side is empty, otherwise don't steal.
	case element(PitNumber, Board) of
		0 ->
			% Steal everything from the player2's corresponding side.
			{Player1StonesAcross, Board1} = takeStonesFromPit(Board, pitAcross(PitNumber)),
			Board2=addStonesToPit(Board1, 14, Player1StonesAcross),
			{player1, addStonesToPit(Board2, PitNumber, 1)};
		_ ->
			% Already have stones in the pit, so don't steal anything from the other side.
			{player1, addStonesToPit(Board, PitNumber, 1)}
	end;
dropStones(Board, player1, PitNumber, 1) ->
	% Player 1 lands somewhere else, next players turn.
	{player2, addStonesToPit(Board, PitNumber, 1)};
dropStones(Board, player2, PitNumber, 1) ->
	% Player 2 lands somewhere else, next players turn.
	{player1, addStonesToPit(Board, PitNumber, 1)};
dropStones(Board, Player, PitNumber, Stones) when Stones >= 1 ->
	% Drop a stone and move to the next pit.
	dropStones(addStonesToPit(Board, PitNumber, 1), Player, nextPit(PitNumber), Stones-1).

pitAcross(PitNumber) ->
	14-PitNumber.

nextPit(14) -> 1;
nextPit(PitNumber) when PitNumber >=1, PitNumber =< 13 -> PitNumber+1.

addStonesToPit(Board, PitNumber, Stones) ->
	setelement(PitNumber, Board, element(PitNumber, Board)+Stones).

takeStonesFromPit(Board, PitNumber) ->
	{element(PitNumber, Board), setelement(PitNumber, Board, 0)}.

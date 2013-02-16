%
% Erlang Programming, Exercise 3-10: Text Processing
%

-module(text_processing).
-export([fill/2,justify/2]).

% Take unstructured text and transform it into filled text. When you hit a blank line, stop filling.
% Filling means keep putting a word on the current line until the next word would exceed the number
% of available columns.
fill(UnstructuredText, Columns) ->
	Words = string:tokens(UnstructuredText, " \n\t"),
	fill_words(Words, Columns, 1).
	
fill_words(Words, Columns, CurrentColumn) ->
	case Words of
		[Word|Rest] ->
			if
				length(Word) + CurrentColumn > Columns ->
					if
						CurrentColumn == 1 ->
							% Print it anyway.
							io:format("~s", [Word]),
							fill_words(Rest, Columns, 1);
						true ->
							io:format("~n"),
							fill_words(Words, Columns, 1)
					end;
				CurrentColumn == 1 ->
					io:format("~s", [Word]),
					fill_words(Rest, Columns, CurrentColumn + length(Word));
				true ->
					io:format(" ~s", [Word]),
					fill_words(Rest, Columns, CurrentColumn + length(Word) + 1)
			end;							
		[] -> io:format("~n")
	end.

justify(UnstructuredText, Columns) -> UnstructuredText.
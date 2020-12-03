%
% Erlang Programming, Exercise 3-9, Indexing
%
% A raw document is a list of lines (i.e., strings).
% A document is a list of words.
% Write functions to:
% a) read a raw document from a file
% b) convert a raw document to a document
% c) index a document

-module(indexing).
-export([read_raw_document/1,convert_to_word_list/1,index/1,print_readable_index/1]).

-define(WORD_DELIMITERS, " \n\t.;?:!|()[]=|\\/<>\"*&^%$#@`").

read_raw_document(Filename) ->
	get_lines(Filename).

get_lines(Filename) ->
	{ok, Device} = file:open(Filename, [read]),
	Result = get_lines(Device, []),
	file:close(Device),
	Result.

get_lines(Device, Acc) ->
	case io:get_line(Device, "") of
		eof -> Acc;
		Line -> get_lines(Device, lists:append(Acc, [Line]))
	end.
	
convert_to_word_list(RawDocument) -> convert_to_word_list(RawDocument, []).
convert_to_word_list([], Acc) -> Acc;
convert_to_word_list([Line|RawDocument], Acc) -> convert_to_word_list(RawDocument, lists:append(Acc, string:tokens(Line, ?WORD_DELIMITERS))).

index(Wordlist) -> lists:sort(dict:to_list(index(Wordlist, dict:new(), 1))).

index([Word|Wordlist], Dict, WordNumber) ->
	Dict2 = dict:append(string:to_lower(Word), WordNumber, Dict),
	index(Wordlist, Dict2, WordNumber+1);
index([], Dict, _) -> Dict.

print_readable_index(IndexList) -> print_index_range_list(index_list_to_index_range_list(IndexList)).

print_index_range_list(IndexRangeList) ->
	lists:foreach(fun({Word, RangeList}) ->
		io:format("~s: ~s~n", [Word, range_list_to_string(RangeList)])
	end, IndexRangeList).
	
range_list_to_string(RangeList) ->
	string:join(lists:map(fun(Item) ->
		case Item of
			{First, First} -> integer_to_list(First);
			{First, Second} -> string:concat(string:concat(integer_to_list(First), "-"), integer_to_list(Second))
		end
	end, RangeList), ",").

index_list_to_index_range_list(NormalizedIndexList) ->
	case NormalizedIndexList of
		[{Word, OcurrenceList}|Rest] ->
			[{Word, occurence_list_to_range_list(OcurrenceList)}|index_list_to_index_range_list(Rest)];
		[] -> []
	end.

occurence_list_to_range_list(OccurenceList) -> range_list_from_sorted_list(lists:sort(OccurenceList), {}).

range_list_from_sorted_list(SortedOccurenceList, CurrentRange) ->
	case SortedOccurenceList of
		[Occurence | Tail] ->
			PreviousOccurence = Occurence - 1,
			case CurrentRange of
				{} -> range_list_from_sorted_list(Tail, {Occurence, Occurence});
				{First, PreviousOccurence} -> range_list_from_sorted_list(Tail, {First, Occurence});
				{_, _} -> [CurrentRange | range_list_from_sorted_list(Tail, {Occurence, Occurence})]
			end;
		[] -> [CurrentRange]
	end.
	
	
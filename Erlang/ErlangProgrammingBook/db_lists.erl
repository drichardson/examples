% Erlang Programming Exercise 3-7

-module(db_lists).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

% List of tuples of the format {Key, Element}.
new() -> [].

destroy(_) -> ok.

write(Key, Element, L) -> lists:keystore(Key, 1, L, {Key, Element}).

delete(Key, L) -> lists:keydelete(Key, 1, L).

read(Key, L) ->
	Result = lists:keyfind(Key, 1, L),
	if
		is_tuple(Result) ->
			{_, Element} = Result,
			{ok, Element};
		true ->
			{error, instance}
	end.

match(_, []) -> [];
match(Element, [{Key, Element} | Tail]) -> [Key | match(Element, Tail)];
match(Element, [_|Tail]) -> match(Element, Tail).
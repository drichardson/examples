% Erlang Programming Exercise 3-4.

-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

% List of tuples of the format {Key, Element}.
new() -> [].

destroy(_) -> ok.

write(Key, Element, []) -> [{Key, Element}];
write(Key, Element, [{Key, _}|Tail]) -> [{Key, Element}|Tail];
write(Key, Element, [Head|Tail]) -> [Head|write(Key, Element, Tail)].

delete(_, []) -> [];
delete(Key, [{Key,_}|Tail]) -> Tail;
delete(Key, [Head|Tail]) -> [Head|delete(Key, Tail)].

read(_, []) -> {error, instance};
read(Key, [{Key,Element}|_]) -> {ok, Element};
read(Key, [_|Tail]) -> read(Key, Tail).

match(_, []) -> [];
match(Element, [{Key, Element} | Tail]) -> [Key | match(Element, Tail)];
match(Element, [_|Tail]) -> match(Element, Tail).
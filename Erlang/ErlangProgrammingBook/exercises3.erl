% Erlang Programming 

-module(exercises3).
-export([sum/1, sum/2, create/1, reverse_create/1, print_integers/1, print_even_integers/1, filter/2, reverse/1, concatenate/1, flatten/1, quicksort/1, mergesort/1]).

%
% Exercise 3-1: Evaluating Expressions
%
sum(1) -> 1;
sum(N) -> N + sum(N-1).

sum(M, M) -> M;
sum(N, M) when N < M -> N + sum(N+1, M).

%
% Exercise 3-2: Creating Lists
%
create_acc(1, L) -> [1 | L];
create_acc(N, L) when N > 1 -> create_acc(N-1, [N|L]).

create(N) -> create_acc(N, []).

reverse_create(1) -> [1];
reverse_create(N) when N > 1 -> [N | reverse_create(N-1)].

print_it(N) -> io:format("Number:~p~n", [N]).


%
% Exercise 3-3: Side Effects
%
print_integers(1) -> print_it(1);
print_integers(N) when N > 1 ->
	print_integers(N-1),
	print_it(N).

print_even_integers(1) -> ok;
print_even_integers(N) when N rem 2 == 0 ->
	print_even_integers(N-1),
	print_it(N);
print_even_integers(N) ->
	print_even_integers(N-1).

%
% Exercise 3-5: Manipulating Lists
%
filter([], _) -> [];
filter([H|T], N) when H =< N -> [H | filter(T, N)];
filter([_|T], N) -> filter(T, N).

reverse_acc([], L) -> L;
reverse_acc([H|T], L) -> reverse_acc(T, [H|L]).

reverse(L) -> reverse_acc(L, []).

expand_list([], Tail) -> Tail;
expand_list([H|T], Tail) -> [H | expand_list(T, Tail)].

concatenate([]) -> [];
concatenate([L1 | Tail]) -> expand_list(L1, concatenate(Tail)).

flatten([]) -> [];
flatten([H|T]) -> concatenate([ flatten(H), flatten(T) ]);
flatten(H) -> [H].

%
% Exercise 3-6: Sorting Lists
%

% Quicksort
lessThan(_, []) -> [];
lessThan(V, [H|T]) when H < V -> [H | lessThan(V, T)];
lessThan(V, [_|T]) -> lessThan(V, T).

greaterThanOrEqualTo(_, []) -> [];
greaterThanOrEqualTo(V, [H|T]) when H >= V -> [H | greaterThanOrEqualTo(V, T)];
greaterThanOrEqualTo(V, [_|T]) -> greaterThanOrEqualTo(V, T).

quicksort([]) -> [];
quicksort([Pivot | Tail]) ->
	concatenate([quicksort(lessThan(Pivot, Tail)), [Pivot], quicksort(greaterThanOrEqualTo(Pivot, Tail))]).

% Merge sort
splitEven([], _) -> [];
splitEven([H|T], Counter) when Counter rem 2 == 0 -> [H | splitEven(T, Counter + 1)];
splitEven([_|T], Counter) -> splitEven(T, Counter + 1).

merge([H1|L1], [H2|L2]) when H1 > H2 -> [H1 | merge(L1, [H2|L2])];
merge(L1, [H2|L2]) -> [H2 | merge(L1, L2)];
merge([H1|L1], L2) -> [H1 | merge(L1, L2)];
merge([], []) -> [].

mergesort([]) -> [];
mergesort([H|[]]) -> [H];
mergesort(L) -> merge(mergesort(splitEven(L, 0)), mergesort(splitEven(L, 1))).









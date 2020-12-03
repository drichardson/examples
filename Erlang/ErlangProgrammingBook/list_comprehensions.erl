-module(list_comprehensions).
-export ([perms/1,splits/1,insert/3,qsort/1]).

perms([]) ->
	[[]];
perms([X|Xs]) ->
	[ insert(X, As, Bs) || Ps <- perms(Xs), {As, Bs} <- splits(Ps) ].

splits([]) ->
	[{[],[]}];
splits([X|Xs] = Ys) ->
	[ {[], Ys} | [ { [X|As], Bs} || {As, Bs} <- splits(Xs) ] ].

insert(X, As, Bs) ->
	lists:append([As, [X], Bs]).

qsort([]) -> [];
qsort([X|Xs]) ->
	qsort([Y || Y <- Xs, Y =< X]) ++ [X] ++ qsort([Y || Y <- Xs, Y > X]).
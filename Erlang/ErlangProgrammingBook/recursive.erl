% Erlang Programming Exercise - Recursion

-module(recursive).
-export([bump/1, reverse/1, merge/2, average/1, average2/1]).

bump([]) -> [];
bump([Head | Tail]) -> [Head + 1 | bump(Tail)].

reverse(L) -> reverse_acc(L, []).

reverse_acc([], Acc) -> Acc;
reverse_acc([Head | Tail], Acc) -> reverse_acc(Tail, [Head | Acc]).

merge(Xs, Yx) -> reverse(mergeL(Xs, Yx, [])).

mergeL([X|Xs], Ys, Zs) -> mergeR(Xs, Ys, [X | Zs]);
mergeL([],[],Zs) -> Zs.

mergeR(Xs, [Y|Ys], Zs) -> mergeL(Xs, Ys, [Y | Zs]);
mergeR([], [], Zs) -> Zs.

average(List) -> sum(List) / length(List).

sum([]) -> 0;
sum([Head | Tail]) -> Head + sum(Tail).

average2(List) -> average2_acc(List, 0, 0).

average2_acc([], _, Length) when Length == 0 -> 0;
average2_acc([], Sum, Length) -> Sum / Length;
average2_acc([Head|Tail], Sum, Length) -> average2_acc(Tail, Head + Sum, Length + 1).


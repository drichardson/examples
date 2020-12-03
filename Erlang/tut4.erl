% Tutorial 4 from http://www.erlang.org/doc/getting_started/seq_prog.html
-module (tut4).
-export ([list_length/1, list_length_tr/1]).

% Note: this implementation of list_length is not tail recursive.
list_length([]) ->
    0;
list_length([_ | Rest]) ->
    1 + list_length(Rest).

% Note: this implementation of list_lenght_tr is tail recursive.
list_length_tr(L) -> list_length_tr(L, 0).

list_length_tr([], Acc) -> Acc;
list_length_tr([_|Rest], Acc) -> list_length_tr(Rest, Acc+1).

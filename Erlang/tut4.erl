% Tutorial 4 from http://www.erlang.org/doc/getting_started/seq_prog.html
-module (tut4).
-export ([list_length/1]).

% Note: this implementation of list_length is not tail recursive.
list_length([]) ->
    0;
list_length([First | Rest]) ->
    1 + list_length(Rest).
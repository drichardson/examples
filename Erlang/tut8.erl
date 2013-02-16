% Tutorial 8 from http://www.erlang.org/doc/getting_started/seq_prog.html
-module (tut8).
-export ([reverse/1]).

reverse(List) ->
    reverse(List, []).
    
reverse([Head|Rest], Reversed_List) ->
    reverse(Rest, [Head | Reversed_List]);
reverse([], Reversed_List) ->
    Reversed_List.
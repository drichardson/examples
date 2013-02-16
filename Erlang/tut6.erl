% Tutorial 6 from http://www.erlang.org/doc/getting_started/seq_prog.html
-module (tut6).
-export ([list_max/1]).

list_max([Head|Rest]) ->
    list_max(Rest, Head).
list_max([], Res) ->
    Res;
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    list_max(Rest, Head);
list_max([Head|Rest], Result_so_far) ->
    list_max(Rest, Result_so_far).
    
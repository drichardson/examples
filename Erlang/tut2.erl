% Tutorial 2 from http://www.erlang.org/doc/getting_started/seq_prog.html
-module (tut2).
-export ([convert/2]).
convert(M, inch) ->
    M / 2.54;
convert(N, centimeter) ->
    N * 2.54.
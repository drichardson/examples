% Tutorial 1 from http://www.erlang.org/doc/getting_started/seq_prog.html
-module (tut1).
-export ([fac/1,mult/2]).
fac(1) ->
    1;
fac(N) ->
    N * fac(N-1).
mult(X, Y) ->
    X * Y.
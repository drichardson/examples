% Tutorial 3 from http://www.erlang.org/doc/getting_started/seq_prog.html
- module(tut3).
- export([convert_length/1]).
convert_length({N, inch}) ->
    { N * 2.54, centimeter };
convert_length({M, centimeter}) ->
    { M / 2.54, inch }.

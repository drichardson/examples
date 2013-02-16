% Tutorial 5 from http://www.erlang.org/doc/getting_started/seq_prog.html
-module (tut5).
-export ([format_temps/1]).

format_temps([]) ->
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celcius(City)),
    format_temps(Rest).
    
convert_to_celcius({Name, {c, Temp}}) ->
    {Name, {c, Temp}}; % no conversion needed
convert_to_celcius({Name, {f, Temp}}) ->
    {Name, {c, (Temp - 32) * 5 / 9 }}. % convert to c

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).
    
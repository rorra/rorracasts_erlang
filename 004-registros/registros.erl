-module(registros).
-compile(export_all).
-include("registros.hrl").

construir_kinetic() ->
    #auto{modelo=kinetic, marca=ford, anio=2015}.

es_0km(#auto{anio=2015} = A) ->
    io:format("El auto ~w ~w es un 0km\n", [A#auto.marca, A#auto.modelo]),
    true;
es_0km(#auto{marca=M, modelo=O}) ->
    io:format("El auto ~w ~w no es un 0km\n", [M, O]),
    false.


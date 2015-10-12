-module(conjuntos).
-compile(export_all).

test() ->
    %% Constructor
    _S1 = sets:new(),
    %% Constructor desde una lista
    S2 = sets:from_list([1, 5, 3, 3]),
    %% Set a lista
    io:format("El conjunto a lista es ~w\n", [sets:to_list(S2)]),
    %% Agregar un elemento
    S3 = sets:add_element(27, S2),
    io:format("Despues de agregar 27, el conjunto es ~w\n", [sets:to_list(S3)]),
    %% Eliminar un elemento
    S4 = sets:del_element(3, S3),
    io:format("Despues de eliminar 3, el conjunto es ~w\n", [sets:to_list(S4)]),
    %% Tamanio de un conjunto
    io:format("El tamanio del conjunto es ~w\n", [sets:size(S4)]),
    %% Unir dos conjuntos
    S5 = sets:union(S4, sets:from_list([10, 11])),
    io:format("El nuevo conjunto luego de unirlo con uno de (10, 11) es ~w\n", [sets:to_list(S5)]),
    %% Interseccion
    S6 = sets:intersection(S5, sets:from_list([1, 2, 3, 4, 5])),
    io:format("La itnerseccion con (1, 2, 3, 4, 5) es ~w\n", [sets:to_list(S6)]),
    %% Resta
    S7 = sets:subtract(S5, sets:from_list([1, 2, 3, 4, 5])),
    io:format("La resta con (1, 2, 3, 4, 5) es ~w\n", [sets:to_list(S7)]),
    %% Filtrado
    S8 = sets:filter(fun(X) -> X =< 5 end, S5),
    io:format("S5 filtrado es ~w\n", [sets:to_list(S8)]),
    S5.
    

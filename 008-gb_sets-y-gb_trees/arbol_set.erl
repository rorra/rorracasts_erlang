-module(arbol_set).
-compile(export_all).

test() ->
    %% Constructor
    _GB1 = gb_sets:new(),
    %% Constructor desde lista
    GB2 = gb_sets:from_list([23, 4, 44, 18, 22, 14]),
    %% Arbol a lista
    io:format("El arbol es ~w\n", [gb_sets:to_list(GB2)]),
    %% Insertar un elemento
    GB3 = gb_sets:add_element(20, GB2),
    io:format("El arbol despues de insertar 20 es ~w\n", [gb_sets:to_list(GB3)]),
    %% Eliminar un elemento
    GB4 = gb_sets:del_element(44, GB3),
    io:format("El arbol despues de eliminar 44 es ~w\n", [gb_sets:to_list(GB4)]),
    %% Verificar si existe un elemento
    case gb_sets:is_element(4, GB4) of
	true ->
	    io:format("El elemento 4 existe\n");
	false ->
	    io:format("el elemento 4 no existe\n")
    end,
    %% Interseccion
    GB5 = gb_sets:intersection(GB4, gb_sets:from_list([4, 20, 200])),
    io:format("La interseccion con [4, 20, 200] es ~w\n", [gb_sets:to_list(GB5)]),
    %% Resta
    GB6 = gb_sets:subtract(GB4, gb_sets:from_list([4, 20])),
    io:format("La resta con [4, 20] es ~w\n", [gb_sets:to_list(GB6)]),
    %% Iterador, recorrer el arbol mas rapido que usando to_list
    Iterador = gb_sets:iterator(GB4),
    iterar(Iterador),
    GB4.

iterar(Iterador) ->
    case gb_sets:next(Iterador) of
	{Elemento, NuevoIterador} ->
	    io:format("Recorriendo el arbol, elemento ~w\n", [Elemento]),
	    iterar(NuevoIterador);
	none ->
	    none
	end.

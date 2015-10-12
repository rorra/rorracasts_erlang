-module(arbol_dic).
-compile(export_all).

test() ->
    %% Constructor
    GB1 = gb_trees:empty(),
    %% Agregar elementos
    GB2 = gb_trees:insert(key3, [valor_3], GB1),
    GB3 = gb_trees:insert(key4, [valor_4], GB2),
    GB4 = gb_trees:insert(key1, [valor_1], GB3),
    %% Arbol a lista
    io:format("El arbol es ~w\n", [gb_trees:to_list(GB4)]),
    %% Modificar un elemento
    GB5 = gb_trees:update(key4, [nuevo_valor_de_4], GB4),
    io:format("El arbol despues de modificar key4 es: ~w\n", [gb_trees:to_list(GB5)]),
    %% Eliminamos un elemento
    GB6 = gb_trees:delete(key4, GB5),
    io:format("Al eliminar key4 el arbol es: ~w\n", [gb_trees:to_list(GB6)]),
    %% Verificar si existe un elemnto
    case gb_trees:lookup(key1, GB5) of
	{value, Valor} ->
	    io:format("key1 es ~w\n", [Valor]);
	none ->
	    io:format("key1 no existe")
    end,
    %% Obtener el valor sabiendo que existe la llave
    io:format("key4 es ~w\n", [gb_trees:get(key4, GB5)]),
    %% Usando un iterador
    Iterador = gb_trees:iterator(GB5),
    iterar(Iterador),
    GB5.

iterar(Iterador) ->
    case gb_trees:next(Iterador) of
	{Clave, Valor, NuevoIterador} ->
	    io:format("Recorriendo el arbol, elemento con clave ~w y valor ~w\n", [Clave, Valor]),
	    iterar(NuevoIterador);
	none ->
	    none
    end.

-module(pl).
-compile(export_all).

test() ->
    %% Creamos una lista del tipo [{X, Y}] when X :: atom(), Y :: term().
    PL = [{a, valor_de_a}, {b, valor_de_b}, {c, valor_de_c}, {b, otro_valor_de_b}],    
    %% Agregamos un elemento a la lista
    PL1 = PL ++ [{h, test}],
    %% Modificamos un elemento
    PL2 = lists:keyreplace(c, 1, PL1, { d, [valor_de_d, otro_mas] }),
    %% Buscamos un elemento, la busqueda es lineal
    io:format("El elemento de la lista cuya llave es b es ~w\n", [proplists:lookup(b, PL2)]),
    %% Buscamos un elemento con clave repetida
    io:format("Primer elemento con clave b es ~w\n", [proplists:lookup(b, PL2)]),
    io:format("Todos los elementos con clave b son ~w\n", [proplists:lookup_all(b, PL2)]),
    %% Verificamos si existe la llave
    case proplists:is_defined(no_definida, PL2) of
	true ->
	    io:format("la llave no_definida esta definida\n");
	false ->
	    io:format("la llave no_definida no esta definida\n")
    end,
    case proplists:is_defined(a, PL2) of
	true ->
	    io:format("la llave a esta definida\n");
	false ->
	    io:format("la llave a no esta definida\n")
    end,
    %% Eliminamos los elementos con llave a
    proplists:delete(a, PL2).


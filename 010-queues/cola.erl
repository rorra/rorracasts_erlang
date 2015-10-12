-module(cola).
-compile(export_all).

test()->
    %% Constrcutor
    _C1 = queue:new(),
    %% Constructor desde lista
    C2 = queue:from_list([1, 2, 3]),
    %% Cola a lista
    io:format("La cola es ~w\n", [queue:to_list(C2)]),
    %% Insertar al final de la cola
    C3 = queue:in(final, C2),
    io:format("Al insertar el elemento final en la cola, la cola es ~w\n", [queue:to_list(C3)]),
    %% Insertar al principio de la cola
    C4 = queue:in_r(principio, C3),
    io:format("Al inserter el elemento principio al principio de la cola, la cola es ~w\n", [queue:to_list(C4)]),
    %% Sacar del principio de la cola
    case queue:out(C4) of
	{{value, Valor1}, C5} ->
	    io:format("Se saco el elemento ~w del principio de la cola\n", [Valor1]),
	    io:format("La nueva cola es: ~w\n", [queue:to_list(C5)]);
	{empty, _C5} ->
	    io:format("La cola esta vacia\n")
    end,
    %% Sacar un elemento del final de la cola
    {{value, Valor2}, C6} = queue:out_r(C4),
    io:format("Se saco el elemento ~w del final de la cola\n", [Valor2]),
    io:format("La nueva cola es ~w\n", [queue:to_list(C6)]),
    %% Verificar si hay un elemento en la cola
    case queue:member(2, C6) of
	true ->
	    io:format("El elemento 2 esta en la cola\n");
	false ->
	    io:format("El elemento 2 no esta en la cola\n")
    end,
    C7 = queue:filter(fun(X) -> is_integer(X) end, C4),
    io:format("La nueva cola filtrada con solo enteros es ~w\n", [queue:to_list(C7)]),
    C7.
			       
    
	    

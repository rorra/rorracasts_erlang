-module(diccionario).
-compile(export_all).

test() ->
    %% Constructor
    _D1 = dict:new(),
    %% Constructor desde una lista
    D2 = dict:from_list([{key3, [valor3]}, {key2, [valor2]}, {key1, valor}]),
    %% Diccionario a lista
    io:format("El diccionario a lista es ~w\n", [dict:to_list(D2)]),
    %% Agregar un item
    D3 = dict:store(key4, 5, D2),
    %% Agregar un valor a un item existente
    D4 = dict:append(key3, otro_valor, D3),
    %% Agregar una lista de valores a un item existente
    D5 = dict:append_list(key2, [un_valor, otro_valor], D4),
    %% Modificar un valor
    D6 = dict:update(key4, fun(X) -> X + 10 end, D5),
    %% Verificar si existe una clave
    case dict:find(key2, D6) of
	{ok, Valor} ->
	    io:format("La clave key2 existe y el valor es ~w\n", [Valor]);
	error ->
	    io:format("La clave key2 no existe\n")
    end,
    case dict:find(foo, D6) of
	{ok, Valor1} ->
	    io:fommat("La clave foo existe y el valor es ~w\n", [Valor1]);
	error ->
	    io:format("La clave foo no existe\n")
    end,
    %% Acceso a un item sabiendo que existe la clave
    io:format("El valor que hay en la clave key1 es ~w\n", [dict:fetch(key1, D6)]),
    %% Iterando a traves del diccionario
    dict:map(fun(K, V) -> io:format("~w -> ~w\n", [K, V]) end, D6),
    %% Eliminar una llave
    D7 = dict:erase(key4, D6),
    D8 = dict:filter(fun(_Key, Value) -> is_atom(Value) end, D7),
    io:format("Diccionario filtrado: ~w\n", [dict:to_list(D8)]),
    D7.
    

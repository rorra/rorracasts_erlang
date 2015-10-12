-module(test_ets).
-compile(export_all).
-include_lib("stdlib/include/ms_transform.hrl").

test_insercion() ->
    %% Vamos a testear cada uno de los ets
    lists:foreach(fun test_insercion_h/1, [set, ordered_set, bag, duplicate_bag]).

test_insercion_h(TipoETS) ->
    %% Creamos la tabla
    %% Las opciones son:
    %% set | ordered_set | bag | duplicate_bag
    %% public | protected | private
    %% named_table
    %% {keyposs, N} (por defecto es 1)
    %% {writte_concurrency, true | false} (por defecto false)
    %% {read_concurrency, true | false} (por defecto false)
    %% compressed
    Tabla = ets:new(test, [TipoETS]),
    %% Insertamos valores
    ets:insert(Tabla, {29129999, "Rodrigo", "Argentina", 10}),
    ets:insert(Tabla, {28321321, "Juan", "Argentina", 8}),
    ets:insert(Tabla, [{23232123, "Matias", "Argentina", 4},
		       {11111113, "Ramos", "Brazil", 5}]),
    %% ETS a lista
    Lista = ets:tab2list(Tabla),
    io:format("~-25w => ~p\n", [TipoETS, Lista]),
    ets:delete(Tabla).
	       
test() ->
    %% Creamos un ets
    T = ets:new(test, [bag]),
    %% Insertamos algunos valores
    ets:insert(T, [
		   {1, 2, 3, 4},
		   {1, 2, 3, 1},
		   {2, 3, 2, 3},
		   {4, 1, 1, 4},
		   {2, 2, 2, 2}]),
    %% Buscamos por la key 2
    io:format("Busqueda de key 2: ~p\n", [ets:lookup(T, 2)]),
    %% Buscar con matcher pattern
    io:format("Matches de $1, $2, $3, $4: ~p\n", [ets:match(T, {'$1', '$2', '$3', '$4'})]),
    io:format("Matches de $20, $4, _, $300: ~p\n", [ets:match(T, {'$20', '$4', '_', '$300'})]),
    io:format("Matches de $1, $2, _, $1: ~p\n", [ets:match(T, {'$1', '$2', '_', '$1'})]),
    io:format("Matches de $1, 1, _, $1: ~p\n", [ets:match(T, {'$1', 1, '_', '$1'})]),
    %% Buscar con select
    %% Formato es: [{Patron Comparacion 1, Guarda 1, Valores de retorno 1},
    %%              {Patron Comparacion N, Guarda N, Valores de retorno N}].
    MS1 = ets:fun2ms(fun({A, B, C, D}) when A == D -> {B, C} end),
    io:format("Select con MS1: ~p\n", [ets:select(T, MS1)]),
    MS2 = ets:fun2ms(fun({A, B, C, D}) when A == D, B < C -> {A, B, C, D} end),
    io:format("Select con MS2: ~p\n", [ets:select(T, MS2)]),
    MS3 = ets:fun2ms(fun({A, B, C, D}) when A == D; B > C -> {A, B, C, D} end),
    io:format("Select con MS3: ~p\n", [ets:select(T, MS3)]),
    %% Eliminar la tabla
    ets:delete(T).

			      
			      
			      

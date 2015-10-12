-module(valuacion).
-compile(export_all).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(valuacion, {indice,
		    anio_valuacion,
		    codigo_marca,
		    marca,
		    familia,
		    modelo,
		    anio_vehiculo,
		    valuacion}).

test() ->
    %% Creamos un ets
    ets:new(valuacion_fiscal, [set, public, named_table, {keypos, 2}]),
    %% Abrimos el archivo con informacion y parseamos las lineas
    {ok, File} = file:open("valuacion-fiscal-2014.csv", [read, {encoding, unicode}]),
    {T1, _} = timer:tc(valuacion, parsear, [File, file:read_line(File), 1]),
    io:format("Tiempo de parseo/insercion: ~f segundos\n", [T1 / 1000000]),
    %% Mostrar informacion de la tabla
    io:format("~p\n", [ets:info(valuacion_fiscal)]),
    %% Hacer un acceso directo
    io:format("el elemento en el indice 16424 es ~p\n", [ets:lookup(valuacion_fiscal, 16424)]),
    %% Eliminar un elemento por su clave
    ets:delete(valuacion_fiscal, 16424),
    io:format("el elemento en el indice 16424 despues de borrarlo es ~p\n", [ets:lookup(valuacion_fiscal, 16424)]),
    %% Buscamos los fiesta kinetic con match specification
    Kinetic1 = buscar_con_ms(),
    io:format("Select con match specification es: ~p\n", [Kinetic1]),
    %% Buscamos con qlc
    Kinetic2 = buscar_con_qlc(),
    io:format("Select con QLC: ~p\n", [Kinetic2]),
    %% Tiempos de busqueda
    {T2, _} = timer:tc(valuacion, buscar_con_ms, []),
    {T3, _} = timer:tc(valuacion, buscar_con_qlc, []),
    io:format("Busqueda con MS: ~f\n", [T2 / 1000000]),
    io:format("Busqueda con QLC: ~f\n", [T3 / 1000000]),
    %% Eliminamos la tabla
    ets:delete(valuacion_fiscal).

buscar_con_ms() ->
    MS = [{{'$1', '$2', '$3', '$4', "FORD", "FIESTA", '$7', '$8', '$9'},
	   [],
	   [{{'$1', '$2', '$3', '$4', "FORD", "FIESTA", '$7', '$8', '$9'}}]}],
    Fiesta = ets:select(valuacion_fiscal, MS),
    lists:filter(fun(X) -> string:str(X#valuacion.modelo, "KINETIC") > 0 end, Fiesta).

buscar_con_qlc() ->
    OH = qlc:q([{A, B, C, D, E, F, G, H, I} || {A, B, C, D, E, F, G, H, I} <- ets:table(valuacion_fiscal),
					       E == "FORD",
					       F == "FIESTA",
					       string:str(G, "KINETIC") > 0]),
    Cursor = qlc:cursor(OH),
    qlc:next_answers(Cursor).
					       

parsear(F, eof, _Index) ->
    file:close(F);
parsear(F, {ok, Linea}, Index) ->
    parsear_linea(Linea, Index),    
    parsear(F, file:read_line(F), Index + 1).

parsear_linea(Linea, Index) ->
    Linea2 = lists:delete(10, Linea),
    Campos = string:tokens(Linea2, ";"),
    RemoverEspacios = fun(X) -> string:strip(X) end,
    Campos2 = lists:map(RemoverEspacios, Campos),
    Item = list_to_tuple([valuacion, Index | Campos2 ]),
    ets:insert(valuacion_fiscal, Item).


    

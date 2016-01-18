-module(irc_server).
-define(PUERTO, 6667).
-export([init/0, init/1, accept/1]).

init() ->
    init(6667).

init(Puerto) ->
    io:format("Iniciando servidor IRC en el puerto ~p~n", [Puerto]),
    case gen_tcp:listen(Puerto, [{packet, line}, {reuseaddr, true}, {active, once}]) of
        {ok, LSocket} ->
	    spawn(?MODULE, accept, [LSocket]);
	{error, Problema} -> 
	    io:format("No se puede escuchar conexiones en el puerto ~p, motivo: ~p~n", [Puerto, Problema])
    end.

accept(LSocket) ->
    io:format("Escuchando conexiones~n"),
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    Pid = spawn(irc_client, init, [Socket]),
	    gen_tcp:controlling_process(Socket, Pid),
	    accept(LSocket);
	{error, Problema} ->
	    io:format("Error al aceptar una nueva conexion: ~p~n", [Problema])
    end.

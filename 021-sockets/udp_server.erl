-module(udp_server).
-compile(export_all).

-define(PUERTO, 3000).

start() ->
    spawn(?MODULE, listen, [?PUERTO]).

listen(Puerto) ->
    Opciones = [{active, true}, {mode, binary}],
    {ok, Socket} = gen_udp:open(Puerto, Opciones),
    io:format("Escuchando en el puerto ~w~n", [Puerto]),
    loop(Socket).

loop(Socket) ->
    receive
	{udp, Socket, Host, Puerto, Datos} ->
	    io:format("Se recibio ~p del host ~p en el puerto ~p~n", [Datos, Host, Puerto]),
	    gen_udp:send(Socket, Host, Puerto, [<<"Recibido: ">>, Datos]),
	    loop(Socket);
	stop ->
	    ok;
	Mensaje ->
	    io:format("Error, se recibio ~p~n", [Mensaje]),
	    loop(Socket)
    end.

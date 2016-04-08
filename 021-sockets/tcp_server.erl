-module(tcp_server).
-compile(export_all).

-define(PUERTO, 3000).

start() ->
    spawn(?MODULE, listen, [?PUERTO]).

listen(Puerto) ->
    Opciones = [{active, true}, {mode, binary}],
    {ok, Socket} = gen_tcp:listen(Puerto, Opciones),
    io:format("Escuchando en el puerto ~w~n", [Puerto]),
    aceptador(Socket).

aceptador(SocketEscuchador) ->
    {ok, Socket} = gen_tcp:accept(SocketEscuchador),
    spawn(fun() -> aceptador(SocketEscuchador) end),
    {ok, {Host, Puerto}} = inet:peername(Socket),
    loop(Socket, Host, Puerto).

loop(Socket, Host, Puerto) ->
    receive
	{tcp, Socket, <<"salir", _/binary>>} ->
	    io:format("Se recibio salir del host ~p del puerto ~p~n", [Host, Puerto]),
	    gen_tcp:close(Socket);
	{tcp, Socket, Datos} ->
	    io:format("Se recibio ~p del host ~p del puerto ~p~n", [Datos, Host, Puerto]),
	    gen_tcp:send(Socket, [<<"Recibido: ">>, Datos]),
	    loop(Socket, Host, Puerto);
	{tcp_closed, _Socket} ->
	    io:format("Se cerro la conexion del host ~p del puerto ~p", [Host, Puerto]);
	Mensaje ->
	    io:format("Error, se recibio ~p~n", [Mensaje]),
	    loop(Socket, Host, Puerto)
    end.
		   











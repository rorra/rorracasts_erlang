-module(udp_client).
-compile(export_all).

-define(PUERTO, 3010).
-define(SERVIDOR_PUERTO, 3000).
-define(SERVIDOR_HOST, {127, 0, 0, 1}).

start() ->
    spawn(?MODULE, listen, [?PUERTO]).

listen(Puerto) ->
    Opciones = [{active, true}, {mode, binary}],
    {ok, Socket} = gen_udp:open(Puerto, Opciones),
    io:format("Escuchando en el puerto ~wn~n", [Puerto]),
    loop(Socket).

loop(Socket) ->
    receive
	{enviar, Mensaje} ->
	    gen_udp:send(Socket, ?SERVIDOR_HOST, ?SERVIDOR_PUERTO, Mensaje),
	    loop(Socket);
	{udp, Socket, Host, Puerto, Datos} ->
	    io:format("Se recibio ~s del host ~w en el puerto ~w~n", [binary_to_list(Datos), Host, Puerto]),
	    loop(Socket);
	stop ->
	    ok;
	Mensaje ->
	    io:format("Error, se recibio ~p~n", [Mensaje]),
	    loop(Socket)
    end.

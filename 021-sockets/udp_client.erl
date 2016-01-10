-module(udp_client).
-compile(export_all).

-define(PUERTO, 3010).
-define(SERVIDOR_PUERTO, 3000).
-define(SERVIDOR_HOST, {127, 0, 0, 1}).

start() ->
    spawn(?MODULE, listen, [?PUERTO]).

listen(Puerto) ->
    Opciones = [{active, true}, binary],
    {ok, Socket} = gen_udp:open(Puerto, Opciones),
    io:format("Esuchando en el puerto ~w~n", [Puerto]),
    loop(Socket).

loop(Socket) ->
    receive 
	{udp, Socket, Host, Puerto, Bin}  ->
	    io:format("Se recibio ~s del host ~w del puerto ~w~n", [binary_to_list(Bin), Host, Puerto]);
	{enviar, Mensaje} ->
	    gen_udp:send(Socket, ?SERVIDOR_HOST, ?SERVIDOR_PUERTO, Mensaje);
	{_Desde, terminate} ->
	    ok;
	Mensaje ->
	    io:format("Error, se recibio ~p~n", [Mensaje])
    end,
    loop(Socket).
    
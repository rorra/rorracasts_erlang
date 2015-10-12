-module(tcp_client).
-compile(export_all).

-define(SERVIDOR_PUERTO, 3000).
-define(SERVIDOR_HOST, {127, 0, 0, 1}).

start() ->
    spawn(?MODULE, conectar, [?SERVIDOR_HOST, ?SERVIDOR_PUERTO]).

conectar(Host, Puerto) ->
    Opciones = [{active, true}, binary],
    {ok, Socket} = gen_tcp:connect(Host, Puerto, Opciones),
    io:format("Se conecto al servidor ~w en el puerto ~w~n", [Host, Puerto]),
    loop(Socket).

loop(Socket) ->
    {ok, {Host, Puerto}} = inet:peername(Socket),
    receive 
	{tcp, Socket, Bin} ->
	    io:format("Se recibio ~s del host ~w del puerto ~w~n", [binary_to_list(Bin), Host, Puerto]),
	    loop(Socket);
	{enviar, Mensaje} ->
	    gen_tcp:send(Socket, Mensaje),
	    loop(Socket);
	{_Desde, terminate} ->
	    gen_tcp:close(Socket),
	    ok;
	Mensaje ->
	    io:format("Error, se recibio ~p~n", [Mensaje]),
	    loop(Socket)
    end.
    

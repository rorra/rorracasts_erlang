-module(udp_server).
-compile(export_all).

-define(PUERTO, 3000).

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
	    io:format("Se recibio ~p del host ~p del puerto ~p~n", [Bin, Host, Puerto]),
	    gen_udp:send(Socket, Host, Puerto, [<<"Recibido: ">>, Bin]);
	{_Desde, terminate} ->
	    ok;
	Mensaje ->
	    io:format("Error, se recibio ~p~n", [Mensaje])
    end,
    loop(Socket).

	    
	    


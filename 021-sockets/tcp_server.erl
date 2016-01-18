-module(tcp_server).
-compile(export_all).

-define(PUERTO, 3000).

start() ->
    spawn(?MODULE, listen, [?PUERTO]).

listen(Puerto) ->
    spawn(fun() ->
		  Opciones = [{active, true}, binary],
		  {ok, Socket} = gen_tcp:listen(Puerto, Opciones),
		  io:format("Esuchando en el puerto ~w~n", [Puerto]),
		  spawn(fun() -> aceptador(Socket) end),
		  timer:sleep(infinity)
	  end).
    
		       

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
	{tcp, Socket, Bin}  ->
	    io:format("Se recibio ~p del host ~p del puerto ~p~n", [Bin, Host, Puerto]),
	    gen_tcp:send(Socket, [<<"Recibido: ">>, Bin]),
	    loop(Socket, Host, Puerto);
	{tcp_closed, _Puerto} ->
	    io:format("Se cerro la conexion del host ~p del puerto ~p", [Host, Puerto]);
	Mensaje ->
	    io:format("Error, se recibio ~p~n", [Mensaje]),
	    loop(Socket, Host, Puerto)
    end.

	    
	    


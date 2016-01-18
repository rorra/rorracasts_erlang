-module(irc_client).
-export([init/1]).

init(Socket) ->
    {ok, {Host, Puerto}} = inet:peername(Socket),
    io:format("Recibida conexion del host ~p y puerto ~p~n", [Host, Puerto]),
    loop(Socket, Host, Puerto).

loop(Socket, Host, Puerto) ->
    receive
        {tcp, Socket, Dato} ->
	    io:format("Se recibio ~p del host ~p del puerto ~p~n", [Dato, Host, Puerto]),
	    inet:setopts(Socket, [{active, once}]),
	    loop(Socket, Host, Puerto);
        {tcp_closed, _Puerto} ->
            io:format("Se cerro la conexion del host ~p del puerto ~p", [Host, Puerto]);
        Mensaje ->
            io:format("Error, se recibio ~p~n", [Mensaje]),
	    inet:setopts(Socket, [{active, once}]),
            loop(Socket, Host, Puerto)
    end.

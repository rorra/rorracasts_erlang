-module(procesos2).
-export([start/0, area/2]).
-import(math, [pi/0]).

%%  Cliente

start() ->
    spawn(fun loop/0).

area(PID, Mensaje) ->
    PID ! {self(), Mensaje},
    receive
	{ok, Area} ->
	    io:format("El area es ~w~n", [Area]);
	{error, Mensaje} ->
	    io:format("Error al enviar el mensaje: ~w~n", [Mensaje])
    end.
	
%% Servidor

loop() ->
    receive
	{Desde, {circulo, Radio}} ->
	    Desde ! {ok, (pi() * Radio * Radio)},
	    loop();
	{Desde, {cuadrado, Lado}} ->
	    Desde ! {ok, Lado * Lado},
	    loop();
	{Desde, {rectangulo, Alto, Ancho}} ->
	    Desde ! {ok, Alto * Ancho},
	    loop();
	{Desde, Mensaje} ->
	    Desde ! {error, Mensaje},
	    loop();
	_ ->
	    loop()
    end.
	    

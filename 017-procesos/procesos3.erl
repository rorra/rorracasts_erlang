-module(procesos3).
-export([start/0, area/2, loop/0]).
-import(math, [pi/0]).

%%  Cliente

start() ->
    spawn(?MODULE, loop, []).

area(PID, Mensaje) ->
    PID ! {self(), Mensaje},
    receive
	{PID, {ok, Area}} ->
	    io:format("El area es ~w~n", [Area]);
	{PID, {error, Mensaje}} ->
	    io:format("Error al enviar el mensaje: ~w~n", [Mensaje])
    after 1000 ->
	    timeout
    end.
	
%% Servidor

loop() ->
    receive
	{Desde, {circulo, Radio}} ->
	    Desde ! {self(), {ok, (pi() * Radio * Radio)}},
	    loop();
	{Desde, {cuadrado, Lado}} ->
	    Desde ! {self(), {ok, Lado * Lado}},
	    loop();
	{Desde, {rectangulo, Alto, Ancho}} ->
	    Desde ! {self(), {ok, Alto * Ancho}},
	    loop();
	{Desde, Mensaje} ->
	    Desde ! {self(), {error, Mensaje}},
	    loop();
	_ ->
	    loop()
    end.
	    

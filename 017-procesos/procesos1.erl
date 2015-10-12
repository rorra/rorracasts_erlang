-module(procesos1).
-export([loop/0]).
-import(math, [pi/0]).

loop() ->
    receive
	{circulo, Radio} ->
	    io:format("El area del circulo es ~w~n", [pi() * Radio * Radio]),
	    loop();
	{cuadrado, Lado} ->
	    io:format("El area del cuadrado es ~w~n", [Lado * Lado]),
	    loop();
	{rectangulo, Alto, Ancho} ->
	    io:format("El area del rectangulo es ~w~n", [Alto * Ancho]),
	    loop();
	Mensaje ->
	    io:format("No se que hacer con el mensaje ~w~n", [Mensaje]),
	    loop()
    end.
	    

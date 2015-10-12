-module(multiplicador).
-compile(export_all).
-vsn(1.0).

loop() ->
    receive
	{multiplicar, Nro} ->
	    io:format("Resultado: ~w~n", [Nro * 5]),
	    loop();
	upgradear ->
	    multiplicador:loop();
	Mensaje ->
	    io:format("Error: ~w~n", [Mensaje]),
	    loop()
    end.
	    
	    

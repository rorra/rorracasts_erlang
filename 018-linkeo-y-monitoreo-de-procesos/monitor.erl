-module(monitor).
-compile(export_all).

start_monitor() ->
    spawn(fun() ->
		  Pid = spawn(fun loop/0),
		  register(mult, Pid),
		  erlang:monitor(process, Pid),
		  receive
		      {'DOWN', _Referencia, process, DesdePid, Razon} ->
			  io:format("El proceso ~w murio y estaba siendo monitoreado, debido a ~p~n", [DesdePid, Razon]),
			  start_monitor()
		  end
	  end).

loop() ->
    receive
	{Pid, {multiplicar, Nro}} ->
	    Pid ! {resultado, Nro * 2},
	    loop();
	Otro ->
	    io:format("Error: ~p~n", [Otro]),
	    loop()
    end.

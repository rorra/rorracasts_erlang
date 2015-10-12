-module(links).
-compile(export_all).

start() ->
    spawn(fun loop/0).

start_link() ->
    spawn_link(fun loop/0).

start_supervisor() ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  Pid = spawn_link(fun loop/0),
		  register(mult, Pid),
		  receive
		      {'EXIT', DesdePid, Razon} ->
			  io:format("El proceso ~w murio debido a ~p~n", [DesdePid, Razon]),
			  start_supervisor()
		  end
	  end).


loop() ->
    receive
	{Pid, {multiplicar, Nro}} ->
	    Pid ! {resultado, Nro * 2},
	    loop();
	Otro ->
	    io:format("Error, recibi el mensaje: ~w~n", [Otro]),
	    loop()
    end.

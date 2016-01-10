-module(cliente).
-export([consultar/1, depositar/2, retirar/2]).

consultar(Cliente) ->
    request({consultar, Cliente}).

depositar(Cliente, Importe) ->
    request({depositar, Cliente, Importe}).

retirar(Cliente, Importe) ->
    request({retirar, Cliente, Importe}).

request(Mensaje) ->
    Servidor = global:whereis_name(banco),
    case Servidor of 
	undefined ->
	    {error, node_down};
	_ ->
	    Ref = erlang:monitor(process, Servidor),
	    Servidor ! {self(), Mensaje},
	    receive
		{Servidor, Respuesta} ->
		    erlang:demonitor(Ref),
		    Respuesta;
		{'DOWN', Ref, process, _Pid, _Motivo} ->
		    io:format("El servidor se cayo"),
		    {error, node_down};
		_ ->
		    io:format("Mensaje no esperado"),
		    {error, unknown}
	    end
    end.

	    
			 

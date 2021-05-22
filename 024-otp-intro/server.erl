-module(server).
-export([start/2, sync_call/2, async_call/2, reply/2]).

%% Interfaz publica

start(Modulo, EstadoInicial) ->
    spawn(fun() -> init(Modulo, EstadoInicial) end).

sync_call(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {sync, self(), Ref, Msg},
    receive
	{Ref, Response} -> Response
    after 1000 ->
	    erlang:error(timeout)
    end.

async_call(Pid, Msg) ->
    Pid ! {async, Msg}.

reply({Pid, Ref}, Msg) ->
    Pid ! {Ref, Msg}.

%% Parte privada

init(Modulo, EstadoInicial) ->
    loop(Modulo, Modulo:init(EstadoInicial)).

loop(Modulo, Estado) ->
    receive
	{sync, Pid, Ref, Mensaje} ->
	    NuevoEstado = Modulo:handle_call(Mensaje, {Pid, Ref}, Estado),
	    loop(Modulo, NuevoEstado);
	{async, Mensaje} ->
	    NuevoEstado = Modulo:handle_cast(Mensaje, Estado),
	    loop(Modulo, NuevoEstado)
	end.

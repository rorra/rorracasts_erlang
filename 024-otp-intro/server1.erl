-module(server1).
-export([start/2, sync_call/2, async_call/2, reply/2]).

%% Interfaz publica

start(Modulo, EstadoInicial) ->
    spawn(fun() -> init(Modulo, EstadoInicial) end).

sync_call(Pid, Mensaje) ->
    Ref = erlang:make_ref(),
    Pid ! {sync, self(), Ref, Mensaje},
    receive
	{Ref, Reply} -> Reply
    after 1000 ->
	    erlang:error(timeout)
    end.

async_call(Pid, Mensaje) ->
    Pid ! {async, Mensaje},
    ok.

reply({Pid, Ref}, Mensaje) ->
    Pid ! {Ref, Mensaje}.

%% Parte privada

init(Modulo, EstadoInicial) ->
    loop(Modulo, Modulo:init(EstadoInicial)).

loop(Modulo, Estado) ->
    receive
	{sync, Pid, Ref, Mensaje} ->
	    loop(Modulo, Modulo:handle_call(Mensaje, {Pid, Ref}, Estado));
	{async, Mensaje} ->
	    loop(Modulo, Modulo:handle_cast(Mensaje, Estado))
    end.

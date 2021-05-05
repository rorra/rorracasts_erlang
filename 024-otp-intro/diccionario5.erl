-module(server1).
-export([start/0, get/2, get_keys/1, store/3, clear/1]).

%% Interfaz del modulo

start() ->
    spawn(fun() -> init(maps:new()) end).

get(Pid, Key) ->
    sync_call(Pid, {get, Key}).

get_keys(Pid) ->
    sync_call(Pid, {get_keys}).

store(Pid, Key, Value) ->
    async_call(Pid, {store, Key, Value}).

clear(Pid) ->
    async_call(Pid, {clear}).

%% Helpers

sync_call(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, Msg},
    receive
	{ok, Ref, Reply} ->
	    Reply
    after 1000 ->
	    erlang:error(timeout)
    end.

async_call(Pid, Msg) ->
    Pid ! Msg.

%% Funciones internas del servidor

init(Diccionario) ->
    loop(Diccionario).

loop(Diccionario) ->
    receive
	{Pid, Ref, {get, Key}} ->
	    case maps:find(Key, Diccionario) of
		{ok, Value} -> Pid ! {ok, Ref, Value};
		_ -> Pid ! {ok, Ref, null}
	    end,
	    loop(Diccionario);
	{Pid, Ref, {get_keys}} ->
	    Pid ! {ok, Ref, maps:keys(Diccionario)},
	    loop(Diccionario);
	{store, Key, Value} -> 
	    NewDiccionario = maps:put(Key, Value, Diccionario),
	    loop(NewDiccionario);
	{clear} ->
	    loop(maps:new());
	_ ->
	    io:format("Mensaje con error~n")
    end.

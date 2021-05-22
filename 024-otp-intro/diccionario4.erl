-module(diccionario4).
-export([start/0, store/3, get/2, clear/1, get_keys/1]).

%% Interfaz del modulo

start() ->
    spawn(fun() -> init(maps:new()) end).

store(Pid, Key, Value) ->
    async_call(Pid, {store, Key, Value}).

clear(Pid) ->
    async_call(Pid, {clear}).

get(Pid, Key) ->
    sync_call(Pid, {get, Key}).

get_keys(Pid) ->
    sync_call(Pid, {get_keys}).

%% Helpers

sync_call(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, Msg},
    receive
	{Ref, Response} -> Response
    after 1000 ->
	    erlang:error(timeout)
    end.

async_call(Pid, Msg) ->
    Pid ! Msg.

reply(Pid, Ref, Msg) ->
    Pid ! {Ref, Msg}.

%% Funciones internas del modulo

init(Estado) ->
    loop(Estado).

loop(Diccionario) ->
    receive
	{store, Key, Value} ->
	    NewDiccionario = maps:put(Key, Value, Diccionario),
	    loop(NewDiccionario);
	{clear} ->
	    loop(maps:new());
	{Pid, Ref, {get, Key}} ->
	    case maps:find(Key, Diccionario) of
		{ok, Value} -> reply(Pid, Ref, {Value});
		_ -> Pid ! reply(Pid, Ref, {null})
	    end,
	    loop(Diccionario);
	{Pid, Ref, {get_keys}} ->
	    reply(Pid, Ref, {maps:keys(Diccionario)}),
	    loop(Diccionario);
	_ ->
	    loop(Diccionario)
    end.

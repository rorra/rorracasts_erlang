-module(diccionario2).
-export([start/0, store/3, get/2, clear/1, get_keys/1]).

%% Interfaz del modulo

start() ->
    spawn(fun() -> init(maps:new()) end).

store(Pid, Key, Value) ->
    Pid ! {store, Key, Value}.

clear(Pid) ->
    Pid ! {clear}.

get(Pid, Key) ->
    Pid ! {self(), get, Key},
    receive
	Response -> Response
    end.

get_keys(Pid) ->
    Pid ! {self(), get_keys},
    receive
	Response -> Response
    end.

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
	{Pid, get, Key} ->
	    case maps:find(Key, Diccionario) of
		{ok, Value} -> Pid ! Value;
		_ -> Pid ! null
	    end,
	    loop(Diccionario);
	{Pid, get_keys} ->
	    Pid ! maps:keys(Diccionario),
	    loop(Diccionario);
	_ ->
	    loop(Diccionario)
    end.

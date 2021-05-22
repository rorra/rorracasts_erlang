-module(diccionario1).
-export([start/0, store/3, get/2]).

%% Interfaz del modulo

start() ->
    spawn(fun() -> init(maps:new()) end).

store(Pid, Key, Value) ->
    Pid ! {store, Key, Value}.

get(Pid, Key) ->
    Pid ! {self(), get, Key},
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
	{Pid, get, Key} ->
	    case maps:find(Key, Diccionario) of
		{ok, Value} -> Pid ! Value;
		_ -> Pid ! null
	    end,
	    loop(Diccionario);
	_ ->
	    loop(Diccionario)
    end.

-module(diccionario1).
-export([start/0, get/2, store/3]).

%% Interfaz del modulo

start() ->
    spawn(fun() -> init(maps:new()) end).

get(Pid, Key) ->
    Pid ! {self(), get, Key},
    receive
	{ok, Response} -> Response
    end.

store(Pid, Key, Value) ->
    Pid ! {store, Key, Value}.

%% Funciones internas del servidor

init(Diccionario) ->
    loop(Diccionario).

loop(Diccionario) ->
    receive
	{Pid, get, Key} ->
	    case maps:find(Key, Diccionario) of
		{ok, Value} -> Pid ! {ok, Value};
		_ -> Pid ! {ok, null}
	    end,
	    loop(Diccionario);
	{store, Key, Value} -> 
	    NewDiccionario = maps:put(Key, Value, Diccionario),
	    loop(NewDiccionario)
    end.

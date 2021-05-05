-module(diccionario2).
-export([start/0, get/2, store/3]).

%% Interfaz del modulo

start() ->
    spawn(fun() -> init(maps:new()) end).

get(Pid, Key) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, get, Key},
    receive
	{ok, Ref, Response} -> Response
    after 1000 ->
	    erlang:error(timeout)
    end.

store(Pid, Key, Value) ->
    Pid ! {store, Key, Value}.

%% Funciones internas del servidor

init(Diccionario) ->
    loop(Diccionario).

loop(Diccionario) ->
    receive
	{Pid, Ref, get, Key} ->
	    case maps:find(Key, Diccionario) of
		{ok, Value} -> Pid ! {ok, Ref, Value};
		_ -> Pid ! {ok, Ref, null}
	    end,
	    loop(Diccionario);
	{store, Key, Value} -> 
	    NewDiccionario = maps:put(Key, Value, Diccionario),
	    loop(NewDiccionario)
    end.

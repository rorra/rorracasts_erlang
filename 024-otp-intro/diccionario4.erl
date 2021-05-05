-module(diccionario4).
-export([start/0, get/2, get_keys/1, store/3, clear/1]).

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

get_keys(Pid) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, get_keys},
    receive
	{ok, Ref, Response} -> Response
    after 1000 ->
	    erlang:error(timeout)
    end.

store(Pid, Key, Value) ->
    Pid ! {store, Key, Value}.

clear(Pid) ->
    Pid ! {clear}.

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
	{Pid, Ref, get_keys} ->
	    Pid ! {ok, Ref, maps:keys(Diccionario)},
	    loop(Diccionario);
	{store, Key, Value} -> 
	    NewDiccionario = maps:put(Key, Value, Diccionario),
	    loop(NewDiccionario);
	{clear} ->
	    loop(maps:new())
    end.

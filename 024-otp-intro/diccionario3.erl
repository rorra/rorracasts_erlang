-module(diccionario3).
-export([start/0, store/3, get/2, clear/1, get_keys/1]).

%% Interfaz del modulo

start() ->
    spawn(fun() -> init(maps:new()) end).

store(Pid, Key, Value) ->
    Pid ! {store, Key, Value}.

clear(Pid) ->
    Pid ! {clear}.

get(Pid, Key) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, get, Key},
    receive
	{Ref, Response} -> Response
    after 1000 ->
	    erlang:error(timeout)
    end.

get_keys(Pid) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, get_keys},
    receive
	{Ref, Response} -> Response
    after 1000 ->
	    erlang:error(timeout)
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
	{Pid, Ref, get, Key} ->
	    case maps:find(Key, Diccionario) of
		{ok, Value} -> Pid ! {Ref, Value};
		_ -> Pid ! {Ref, null}
	    end,
	    loop(Diccionario);
	{Pid, Ref, get_keys} ->
	    Pid ! {Ref, maps:keys(Diccionario)},
	    loop(Diccionario);
	_ ->
	    loop(Diccionario)
    end.

-module(diccionario5).
-export([start/0, store/3, get/2, clear/1, get_keys/1]).
-export([init/1, handle_call/3, handle_cast/2]).

%% Interfaz del modulo

start() ->
    server:start(?MODULE, [maps:new()]).

store(Pid, Key, Value) ->
    server:async_call(Pid, {store, Key, Value}).

clear(Pid) ->
    server:async_call(Pid, {clear}).

get(Pid, Key) ->
    server:sync_call(Pid, {get, Key}).

get_keys(Pid) ->
    server:sync_call(Pid, {get_keys}).

%% Funciones internas del modulo

init([Estado]) ->
    Estado.

handle_cast({store, Key, Value}, Diccionario) ->
    maps:put(Key, Value, Diccionario);
handle_cast({clear}, _Diccionario) ->
    maps:new().

handle_call({get, Key}, PidRef, Diccionario) ->
    case maps:find(Key, Diccionario) of
	{ok, Value} -> server:reply(PidRef, Value);
	_ -> server:reply(PidRef, null)
    end,
    Diccionario;
handle_call({get_keys}, PidRef, Diccionario) ->
    server:reply(PidRef, maps:keys(Diccionario)),
    Diccionario.

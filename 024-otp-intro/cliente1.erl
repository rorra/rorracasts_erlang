-module(cliente1).

-export([start/0, store/3, get/2, get_keys/1, clear/1]).
-export([init/1, handle_call/3, handle_cast/2]).

%% API

start() ->
    server1:start(?MODULE, [maps:new()]).

store(Pid, Key, Value) ->
    server1:async_call(Pid, {store, Key, Value}).

get(Pid, Key) ->
    server1:sync_call(Pid, {get, Key}).

get_keys(Pid) ->
    server1:sync_call(Pid, {get_keys}).

clear(Pid) ->
    server1:async_call(Pid, {clear}).

%% Implementacion

init([EstadoInicial]) -> 
    EstadoInicial.

handle_cast({store, Key, Value}, Diccionario) ->
    maps:put(Key, Value, Diccionario);
handle_cast({clear}, _Diccionario) ->
    maps:new().

handle_call({get, Key}, Pid, Diccionario) ->
    case maps:find(Key, Diccionario) of
	{ok, Value} -> server1:reply(Pid, Value);
	_ -> server1:reply(Pid, null)
    end,
    Diccionario;
handle_call({get_keys}, Pid, Diccionario) ->
    server1:reply(Pid, maps:keys(Diccionario)),
    Diccionario.

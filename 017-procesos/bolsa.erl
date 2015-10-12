-module(bolsa).
-export([start/0, poner/2, sacar/2, consultar/1, loop/1]).

%% Cliente

start() ->
    spawn(?MODULE, loop, [[]]).

poner(Pid, Item) ->
    Pid ! {self(), {poner, Item}},
    receive
	{Pid, {ok, Item}} ->
	    Item;
	{Pid, {error, _Item}} ->
	    error
    after 500 ->
	    timeout
    end.

sacar(Pid, Item) ->
    Pid ! {self(), {sacar, Item}},
    receive
	{Pid, {ok, Item}} ->
	    Item;
	{Pid, {error, _Item}} ->
	    error
    after 500 ->
	    timeout
    end.

consultar(Pid) ->
    Pid ! {self(), {consultar}},
    receive
	{Pid, {ok, Lista}} ->
	    Lista;
	{Pid, {error}} ->
	    error
    after 500 ->
	    timeout
    end.

%% Servidor

loop(Bolsa) ->
    receive
	{Desde, {poner, Item}} when is_pid(Desde) ->
	    NuevaBolsa = case lists:member(Item, Bolsa) of
			     false ->
				 Desde ! {self(), {ok, Item}},
				 Bolsa ++ [Item];
			     true ->
				 Desde ! {self(), {error, ya_existe}},
				 Bolsa
			 end,
	    loop(NuevaBolsa);
	{Desde, {sacar, Item}} when is_pid(Desde) ->
	    NuevaBolsa = case lists:member(Item, Bolsa) of
			     true ->
				 Desde ! {self(), {ok, Item}},
				 lists:delete(Item, Bolsa);
			     false ->
				 Desde ! {self(), {error, no_existe}},
				 Bolsa
			 end,
	    loop(NuevaBolsa);
	{Desde, {consultar}} when is_pid(Desde) ->
	    Desde ! {self(), {ok, Bolsa}},
	    loop(Bolsa);
	_ ->
	    loop(Bolsa)
    end.

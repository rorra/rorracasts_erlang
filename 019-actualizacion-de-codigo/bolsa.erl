-module(bolsa).
-export([start/0, poner/2, sacar/2, consultar/1, loop/1, convertir/1]).
-vsn(2.0).

%% Cliente

start() ->
    spawn(?MODULE, loop, [sets:new()]).

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

convertir(Datos) ->
    sets:from_list(Datos).


%% Servidor

loop(Bolsa) ->
    receive
	{Desde, {poner, Item}} when is_pid(Desde) ->
	    NuevaBolsa = case sets:is_element(Item, Bolsa) of
			     false ->
				 Desde ! {self(), {ok, Item}},
				 sets:add_element(Item, Bolsa);
			     true ->
				 Desde ! {self(), {error, ya_existe}},
				 Bolsa
			 end,
	    loop(NuevaBolsa);
	{Desde, {sacar, Item}} when is_pid(Desde) ->
	    NuevaBolsa = case sets:is_element(Item, Bolsa) of
			     true ->
				 Desde ! {self(), {ok, Item}},
				 sets:del_element(Item, Bolsa);
			     false ->
				 Desde ! {self(), {error, no_existe}},
				 Bolsa
			 end,
	    loop(NuevaBolsa);
	{Desde, {consultar}} when is_pid(Desde) ->
	    Desde ! {self(), {ok, Bolsa}},
	    loop(Bolsa);
	{Desde, update} ->
	    Desde ! "ok, upgradeando",
	    NuevaBolsa = bolsa:convertir(Bolsa),
	    bolsa:loop(NuevaBolsa);
	_ ->
	    loop(Bolsa)
    end.

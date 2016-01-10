-module(banco).
-export([start/0, server/1]).

start() ->
    global:register_name(banco, spawn(?MODULE, server, [ets:new(banco_ets, [bag, named_table, public])])),
    ok.

server(ETS) ->
    receive
	{Desde, {depositar, Cliente, Importe}} ->
	    case ets:lookup(ETS, Cliente) of
		[{Cliente, Saldo}] ->
		    ets:delete(ETS, Cliente),
		    NuevoSaldo = Saldo + Importe,
		    ets:insert(ETS, {Cliente, NuevoSaldo}),
		    Desde ! {self(), {ok, NuevoSaldo}};
		_ ->
		    ets:insert(ETS, {Cliente, Importe}),
		    Desde ! {self(), {ok, Importe}}
	    end,
	    server(ETS);
	{Desde, {retirar, Cliente, Importe}} ->
	    case ets:lookup(ETS, Cliente) of
		[{Cliente, Saldo}] when Saldo >= Importe ->
		    ets:delete(ETS, Cliente),
		    NuevoSaldo = Saldo - Importe,
		    ets:insert(ETS, {Cliente, NuevoSaldo}),
		    Desde ! {self(), {ok, NuevoSaldo}};
		[{Cliente, Saldo}] when Saldo < Importe  ->
		    Desde ! {self(), {error, insuficiente}};
		_ ->
		    Desde ! {self(), {error, no_existe}}
	    end,
	    server(ETS);
	{Desde, {consultar, Cliente}} ->
	    case ets:lookup(ETS, Cliente) of
		[{Cliente, Saldo}] ->
		    Desde ! {self(), {cliente, Saldo}};
		_ ->
		    Desde ! {self(), {error, no_existe}}
	    end,
	    server(ETS);
	stop ->
	    ok;
	_ ->
	    server(ETS)
    end.


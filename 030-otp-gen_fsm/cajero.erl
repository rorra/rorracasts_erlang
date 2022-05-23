%%%-------------------------------------------------------------------
%%% @author Rodrigo Dominguez <rorra@rorra.com.ar>
%%% @copyright (C) 2022, Rodrigo Dominguez
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2022 by Rodrigo Dominguez <rorra@rorra.com.ar>
%%%-------------------------------------------------------------------
-module(cajero).

-behaviour(gen_fsm).

%% API
-export([start_link/0, ingresar_tarjeta/1, ingresar_password/1, ingresar_dinero/1, retirar_dinero/1, cancelar/0]).

%% gen_fsm callbacks
-export([init/1, pantalla_principal/2, password/2, menu/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 15000).

-record(state, {tarjeta, saldo, intentos}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

ingresar_tarjeta(Tarjeta) ->
    gen_fsm:send_event(?MODULE, {tarjeta, Tarjeta}).

ingresar_password(Password) ->
    gen_fsm:send_event(?MODULE, {password, Password}).

ingresar_dinero(Dinero) ->
    gen_fsm:send_event(?MODULE, {deposito, Dinero}).

retirar_dinero(Dinero) ->
    gen_fsm:send_event(?MODULE, {retiro, Dinero}).

cancelar() ->
    gen_fsm:sync_send_all_state_event(?MODULE, {cancelar}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, pantalla_principal, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
pantalla_principal({tarjeta, Tarjeta}, Estado) ->
    io:format("Se ingreso la tarjeta ~p~n", [Tarjeta]),
    Saldo = 100 + rand:uniform(1000),
    io:format("Ingrese su password~n"),
    {next_state, password, Estado#state{tarjeta=Tarjeta, saldo=Saldo, intentos=0}, ?TIMEOUT};
pantalla_principal(_, Estado) ->
    io:format("Ingrese una tarjeta~n"),
    {next_state, pantalla_principal, Estado}.

password({password, "123"}, Estado) ->
    io:format("Password OK~n"),
    mostrar_menu(Estado),
    {next_state, menu, Estado};
password({password, _}, #state{intentos=Intentos}=Estado) when Intentos < 2 ->
    io:format("Password invalido~n"),
    io:format("Ingrese su password~n"),
    NuevosIntentos = Intentos + 1,
    {next_state, password, Estado#state{intentos=NuevosIntentos}, ?TIMEOUT};
password({password, _}, _Estado) ->
    io:format("Tarjeta retenida~n"),
    {next_state, pantalla_principal, #state{}};
password(timeout, _Estado) ->
    io:format("Tarjeta retenida~n"),
    {next_state, pantalla_principal, #state{}};
password(Evento, Estado) ->
    io:format("Ingrese su password~n"),
    {next_state, password, Estado, ?TIMEOUT}.

menu({deposito, Dinero}, #state{saldo=Saldo}=Estado) ->
    NuevoSaldo = Saldo + Dinero,
    io:format("Se ingresaron ~p a su cuenta bancaria~n", [Dinero]),
    NuevoEstado = Estado#state{saldo=NuevoSaldo},
    mostrar_menu(NuevoEstado),
    {next_state, menu, NuevoEstado};
menu({retiro, Dinero}, #state{saldo=Saldo}=Estado) when Saldo >= Dinero ->
    NuevoSaldo = Saldo - Dinero,
    io:format("Se retiraron ~p de su cuenta bancaria~n", [Dinero]),
    NuevoEstado = Estado#state{saldo=NuevoSaldo},
    mostrar_menu(NuevoEstado),
    {next_state, menu, NuevoEstado};
menu({retiro, Dinero}, #state{saldo=Saldo}=Estado) ->
    io:format("Saldo insuficiente~n"),
    mostrar_menu(Estado),
    {next_state, menu, Estado};
menu(_Evento, Estado) ->
    io:format("Opcion invalida"),
    mostrar_menu(Estado),
    {next_state, menu, Estado}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event({cancelar}, _From, NombreEstado, _Estado) ->
    io:format("Estado actual: ~p. Saliendo~n", [NombreEstado]),
    {reply, ok, pantalla_principal, #state{}};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
mostrar_menu(Estado) ->
    Saldo = Estado#state.saldo,
    io:format("Saldo: ~p~n", [Saldo]),
    io:format("ingresar_dinero - Ingresa dinero a su cuenta bancaria~n"),
    io:format("retirar_dinero - Retira dinero de su cuenta bancaria~n"),
    io:format("salir - Devuelve su tarjeta y vuelve a la pantalla principal~n").

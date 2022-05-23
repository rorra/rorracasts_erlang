%%%-------------------------------------------------------------------
%%% @author Rodrigo Dominguez <rorra@rorra.com.ar>
%%% @copyright (C) 2022, Rodrigo Dominguez
%%% @doc
%%%
%%% @end
%%% Created : 23 May 2022 by Rodrigo Dominguez <rorra@rorra.com.ar>
%%%-------------------------------------------------------------------
-module(cajero).

-behaviour(gen_statem).

%% API
-export([start_link/0, ingresar_tarjeta/1, ingresar_password/1, ingresar_dinero/1, retirar_dinero/1, cancelar/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([pantalla_principal/3, password/3, menu/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 15000).

-record(data, {tarjeta, saldo, intentos}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
	  {ok, Pid :: pid()} |
	  ignore |
	  {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

ingresar_tarjeta(Tarjeta) ->
    gen_statem:cast(?MODULE, {tarjeta, Tarjeta}).

ingresar_password(Password) ->
    gen_statem:cast(?MODULE, {password, Password}).

ingresar_dinero(Dinero) ->
    gen_statem:cast(?MODULE, {deposito, Dinero}).

retirar_dinero(Dinero) ->
    gen_statem:cast(?MODULE, {retiro, Dinero}).

cancelar() ->
    gen_statem:call(?MODULE, {cancelar}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> [state_functions, state_enter].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
	  gen_statem:init_result(atom()).
init([]) ->
    process_flag(trap_exit, true),
    {ok, pantalla_principal, #data{}}.

pantalla_principal(enter, _, _) ->
    io:format("Ingrese una tarjeta~n"),
    {next_state, pantalla_principal, #data{}};
pantalla_principal(cast, {tarjeta, Tarjeta}, Data) ->
    io:format("Se ingreso la tarjeta ~p~n", [Tarjeta]),
    Saldo = 100 + rand:uniform(1000),
    {next_state, password, Data#data{tarjeta=Tarjeta, saldo=Saldo, intentos=0}, [{timeout, ?TIMEOUT, {}}]};
pantalla_principal({call, Caller}, {cancelar}, _Data) ->
    io:format("Cancelando la operacion~n"),
    {keep_state_and_data, [{reply, Caller, ok}]};
pantalla_principal(_, _, _) ->
    io:format("Ingrese una tarjeta~n"),
    {keep_state_and_data, []}.

password(enter, _, Data) ->
    io:format("Ingrese su password~n"),
    {next_state, password, Data};
password(cast, {password, "123"}, Data) ->
    io:format("Password OK~n"),
    {next_state, menu, Data};
password(cast, {password, _}, #data{intentos=Intentos}=Data) when Intentos < 2 ->
    io:format("Password invalido~n"),
    io:format("Ingrese su password~n"),
    NuevosIntentos = Intentos + 1,
    {next_state, password, Data#data{intentos=NuevosIntentos}, [{timeout, ?TIMEOUT, {}}]};
password(cast, {password, _}, _Data) ->
    io:format("Tarjeta retenida~n"),
    {next_state, pantalla_principal, #data{}};
password(timeout, {}, _Data) ->
    io:format("Tarjeta retenida~n"),
    {next_state, pantalla_principal, #data{}};
password({call, Caller}, {cancelar}, _Data) ->
    io:format("Cancelando la operacion~n"),
    {next_state, pantalla_principal, #data{}, [{reply, Caller, ok}]};
password(_, _Evento, _Data) ->
    io:format("Ingrese su password~n"),
    {keep_state_and_data, [{timeout, ?TIMEOUT, {}}]}.

menu(enter, _, Data) ->
    mostrar_menu(Data),
    {next_state, menu, Data};
menu(cast, {deposito, Dinero}, #data{saldo=Saldo}=Data) ->
    NuevoSaldo = Saldo + Dinero,
    io:format("Se ingresaron ~p a su cuenta bancaria~n", [Dinero]),
    NuevaData = Data#data{saldo=NuevoSaldo},
    mostrar_menu(NuevaData),
    {keep_state, NuevaData};
menu(cast, {retiro, Dinero}, #data{saldo=Saldo}=Data) when Saldo >= Dinero ->
    NuevoSaldo = Saldo - Dinero,
    io:format("Se retiraron ~p de su cuenta bancaria~n", [Dinero]),
    NuevaData = Data#data{saldo=NuevoSaldo},
    mostrar_menu(NuevaData),
    {keep_state, NuevaData};
menu(cast, {retiro, _Dinero}, Data) ->
    io:format("Saldo insuficiente~n"),
    mostrar_menu(Data),
    {keep_state_and_data, []};
menu({call, Caller}, {cancelar}, _Data) ->
    io:format("Cancelando la operacion~n"),
    {next_state, pantalla_principal, #data{}, [{reply, Caller, ok}]};
menu(_, _Evento, Data) ->
    io:format("Opcion invalida"),
    mostrar_menu(Data),
    {keep_state_and_data, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
	  any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
	OldVsn :: term() | {down,term()},
	State :: term(), Data :: term(), Extra :: term()) ->
	  {ok, NewState :: term(), NewData :: term()} |
	  (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
mostrar_menu(Data) ->
    Saldo = Data#data.saldo,
    io:format("Saldo: ~p~n", [Saldo]),
    io:format("ingresar_dinero - Ingresa dinero a su cuenta bancaria~n"),
    io:format("retirar_dinero - Retira dinero de su cuenta bancaria~n"),
    io:format("cancelar - Devuelve su tarjeta y vuelve a la pantalla principal~n").

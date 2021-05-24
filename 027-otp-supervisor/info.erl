-module(info).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1, ok/1, error/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {nombre, tiempo}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Nombre) ->
    io:format("info:start_link [~p]~n", [Nombre]),
    gen_server:start_link({local, Nombre}, ?MODULE, [Nombre], []).

stop(Nombre) ->
    io:format("info:stop [~p]~n", [Nombre]),
    gen_server:call(Nombre, {stop}).

ok(Nombre) ->
    io:format("info:ok [~p]~n", [Nombre]),
    gen_server:call(Nombre, {ok}).

error(Nombre) ->
    io:format("info:error [~p]~n", [Nombre]),
    gen_server:call(Nombre, {error}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Nombre]) ->
    io:format("info:init [~p]~n", [Nombre]),
    process_flag(trap_exit, true),
    Tiempo = rand:uniform(10000),
    {ok, #state{tiempo=Tiempo, nombre=Nombre}, Tiempo}.

handle_call({ok}, _From, State) ->
    io:format("info:handle_call{ok} [~p]~n", [State#state.nombre]),
    {reply, ok, State};
handle_call({error}, _From, State) ->
    io:format("info:handle_call{error} [~p]~n", [State#state.nombre]),
    {reply, 0/0, State};
handle_call({stop}, _From, State) ->
    io:format("info:handle_call{stop} [~p]~n", [State#state.nombre]),
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    io:format("info:handle_call inesperado [~p]~n", [State#state.nombre]),
    {reply, ok, State}.

handle_cast(_Request, State) ->
    io:format("info:handle_cast inesperado [~p]~n", [State#state.nombre]),
    {noreply, State}.

handle_info(timeout, State) ->
    io:format("info:handle_info{timeout} [~p]~n", [State#state.nombre]),
    {noreply, State, State#state.tiempo};
handle_info(_Info, State) ->
    io:format("info:handle_info inesperado [~p]~n", [State#state.nombre]),
    {noreply, State}.

terminate(_Reason, State) ->
    io:format("info:terminate [~p]~n", [State#state.nombre]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

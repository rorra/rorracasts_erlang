-module(diccionario1).

-behaviour(gen_server).

%% API
-export([start_link/0, store/3, get/2, get_keys/1, clear/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

store(Pid, Key, Value) ->
    gen_server:cast(Pid, {store, Key, Value}).

get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}).

get_keys(Pid) ->
    gen_server:call(Pid, {get_keys}).

clear(Pid) ->
    gen_server:cast(Pid, {clear}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, maps:new()}.

handle_call({get, Key}, _From, State) ->
    Value = case maps:find(Key, State) of
		{ok, Found} -> Found;
		_ -> null
	    end,
    {reply, Value, State};
handle_call({get_keys}, _From, State) ->
    {reply, maps:keys(State), State};
handle_call(Request, _From, State) ->
    io:format("handle_call no esperado~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({store, Key, Value}, State) ->
    NewState = maps:put(Key, Value, State),
    {noreply, NewState};
handle_cast({clear}, _State) ->
    {noreply, maps:new()};
handle_cast(Request, State) ->
    io:format("handle_cast no esperado~p~n", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("handle_info no esperado~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Proceso terminado de forma normal~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

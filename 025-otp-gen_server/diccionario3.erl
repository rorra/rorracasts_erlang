%%%-------------------------------------------------------------------
%%% @author Rodrigo Dominguez <rorra@rorra-laptop>
%%% @copyright (C) 2021, Rodrigo Dominguez
%%% @doc
%%% Un diccionario mostrando como funciona el otp
%%% @end
%%% Created :  5 May 2021 by Rodrigo Dominguez <rorra@rorra-laptop>
%%%-------------------------------------------------------------------
-module(diccionario3).

-behaviour(gen_server).

%% API
-export([start_link/0, store/2, get/1, get_keys/0, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

store(Key, Value) ->
    gen_server:cast({global, ?MODULE}, {store, Key, Value}).

get(Key) ->
    gen_server:call({global, ?MODULE}, {get, Key}).

get_keys() ->
    gen_server:call({global, ?MODULE}, {get_keys}).

clear() ->
    gen_server:cast({global, ?MODULE}, {clear}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, maps:new()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({get, Key}, _From, State) ->
    Value = case maps:find(Key, State) of
		 {ok, Found} -> Found;
		 _ -> undefined
	     end,
    {reply, Value, State};
handle_call({get_keys}, _From, State) ->
    {reply, maps:keys(State), State};
handle_call(_Mensaje, _From, State) ->
    io:format("Se recibio un mensaje sincronico no esperado~n"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({store, Key, Value}, State) ->
    NewDiccionario = maps:put(Key, Value, State),
    {noreply, NewDiccionario};
handle_cast({clear}, _State) ->
    {noreply, maps:new()};
handle_cast(_Mensaje, State) ->
    io:format("Se recibio un mensaje asincronico no esperado~n"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    io:format("Se recibio un mensaje en handle_info~n"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

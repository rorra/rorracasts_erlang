%%%-------------------------------------------------------------------
%%% @author Rodrigo Dominguez <rorra@rorra.com.ar>
%%% @copyright (C) 2022, Rodrigo Dominguez
%%% @doc
%%%
%%% @end
%%% Created : 12 May 2022 by Rodrigo Dominguez <rorra@rorra.com.ar>
%%%-------------------------------------------------------------------
-module(disco).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {file}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()} | term()}.
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%% @end
%%--------------------------------------------------------------------
-spec add_handler() -> ok | {'EXIT', Reason :: term()} | term().
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term() | {Args :: term(), Term :: term()}) ->
	  {ok, State :: term()} |
	  {ok, State :: term(), hibernate} |
	  {error, Reason :: term()}.
init([NombreArchivo]) ->
    {ok, File} = file:open(NombreArchivo, write),
    {ok, #state{file=File}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Event :: term(), State :: term()) ->
	  {ok, NewState :: term()} |
	  {ok, NewState :: term(), hibernate} |
	  remove_handler |
	  {swap_handler, Args1 :: term(), NewState :: term(),
	   Handler2 :: atom() | {atom(), term()} , Args2 :: term()}.
handle_event(Evento, State) ->
    io:format(State#state.file, "~p~n", [Evento]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), State :: term()) ->
	  {ok, Reply :: term(), NewState :: term()} |
	  {ok, Reply :: term(), NewState :: term(), hibernate} |
	  {remove_handler, Reply :: term()} |
	  {swap_handler, Reply :: term(), Args1 :: term(), NewState :: term(),
	   Handler2 :: atom() | {atom(), term()}, Args2 :: term()}.
handle_call(_Request, State) ->
    io:format("handle_call llamado en disco~n"),
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: term()) ->
	  {ok, NewState :: term()} |
	  {ok, NewState :: term(), hibernate} |
	  remove_handler |
	  {swap_handler, Args1 :: term(), NewState :: term(),
	   Handler2 :: atom() | {atom(), term()}, Args2 :: term()}.
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Arg :: {stop, Reason :: term()} |
		       stop |
		       remove_handler |
		       {error, {'EXIT', Reason :: term()}} |
		       {error, Term :: term()} |
		       term(),
		State :: term()) -> any().
terminate(_Arg, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_event status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

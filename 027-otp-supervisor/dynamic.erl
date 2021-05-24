-module(dynamic).

-behaviour(supervisor).

%% API
-export([start_link/0, add_child/1, remove_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_child(Nombre) ->
    Child = #{id => Nombre,
	       start => {info, start_link, [Nombre]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [info]},
    supervisor:start_child(?SERVER, Child).

remove_child(Nombre) ->
    supervisor:terminate_child(?SERVER, Nombre),
    supervisor:delete_child(?SERVER, Nombre).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    AChild = #{id => marcos,
	       start => {info, start_link, [marcos]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [info]},
    BChild = #{id => juan,
	       start => {info, start_link, [juan]},
	       restart => temporary,
	       shutdown => 5000,
	       type => worker,
	       modules => [info]},
    CChild = #{id => pedro,
	       start => {info, start_link, [pedro]},
	       restart => transient,
	       shutdown => 5000,
	       type => worker,
	       modules => [info]},

    {ok, {SupFlags, [AChild, BChild, CChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

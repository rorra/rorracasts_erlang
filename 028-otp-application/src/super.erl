-module(super).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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

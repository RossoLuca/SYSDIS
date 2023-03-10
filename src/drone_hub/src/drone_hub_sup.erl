%%%-------------------------------------------------------------------
%% @doc drone_hub top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(drone_hub_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [ #{
                id => drone_supervisor,
                start => {drone_supervisor, start_link, []},
                restart => permanent,
                type => supervisor
        }],
    {ok, {SupFlags, ChildSpecs}}.

%%%-------------------------------------------------------------------
%% @doc drone_hub public API
%% @end
%%%-------------------------------------------------------------------

-module(drone_hub_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    drone_hub_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

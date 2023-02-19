%%%-------------------------------------------------------------------
%% @doc drone public API
%% @end
%%%-------------------------------------------------------------------

-module(drone_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    drone_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

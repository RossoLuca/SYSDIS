%%%-------------------------------------------------------------------
%% @doc repository public API
%% @end
%%%-------------------------------------------------------------------

-module(repository_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    db_supervisor:start().

stop(_State) ->
    ok.

%% internal functions

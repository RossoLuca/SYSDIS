-module(ui_control_service_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ui_control_service_sup:start_link().

stop(_State) ->
	ok.

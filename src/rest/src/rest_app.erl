-module(rest_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    % net_kernel:start(['rest_api@rest_host']),
    erlang:set_cookie(node(),'dis_sys'),
	{ ok, Pid } = rest_sup:start_link(),
    Routes = [{
        '_',
        [
            { "/delivery/getactivedrones", active_drove_handler, [ ] },         %% returns the list of all active drones
            { "/delivery/", delivery_handler, [ ] }   %% updates a delivery
        ]
    }],
    Dispatch = cowboy_router:compile( Routes ),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    { ok, Pid }.

stop(_State) ->
	ok.

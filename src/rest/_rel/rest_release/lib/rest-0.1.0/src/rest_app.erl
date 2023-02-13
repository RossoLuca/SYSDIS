-module(rest_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ ok, Pid } = rest_sup:start_link(),
    Routes = [{
        '_',
        [
            { "/delivery/getactivedrones", active_drove_handler, [ ] },         %% returns the list of all active drones
            { "/delivery/spawnrecoverydrone", spawn_recovery_handler, [ ] },    %% creates new recovery drone
            { "/delivery/", delivery_handler, [ ] },     %% updates a delivery
            { "/delivery/readbyid", single_drone_handler, [ ] }   %% Get delivery by id
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

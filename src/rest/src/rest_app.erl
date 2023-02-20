-module(rest_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    
	{ ok, Pid } = rest_sup:start_link(),
    Routes = [{
        '_',
        [
            { "/delivery/get_active_drones",[],active_drone_handler, [ ] },         %% returns the list of all active drones
            { "/delivery/",delivery_handler, [] },   %% updates a delivery
            { "/delivery/insert",create_delivery_handler, [] },   %% Creates a new delivery
            { "/delivery/kill",drone_kill_handler, []},
            { "/delivery/id",ids_handler, []}
            % ,{ "/delivery/get_adjancesies", adjancesies_handler, []}
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




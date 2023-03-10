-module(rest_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    
	{ ok, Pid } = rest_sup:start_link(),
    Routes = [{
        '_',
        [
            { "/delivery/get_active_drones",[],active_drone_handler, [ ] }, %% returns the list of all active drones
            { "/delivery/",delivery_handler, [] }, %% write or updates a delivery
            { "/delivery/insert",create_delivery_handler, [] }, %% send a request to create a new drone for a delivery to the drone hub
            { "/delivery/kill",drone_kill_handler, []}, %% send a requesto to the drone hub to kill the drone that handles a specific delivery
            { "/delivery/id",ids_handler, []} %% returns the last id that was used to insert a delivery
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




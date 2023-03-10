-module(drone_hub_wrapper).
-include("records.hrl").
-export([notify/2]).

notify(create,{Start_x,Start_y,End_x,End_y}) ->
    Process = get_drone_hub_process(),
    Process ! {create, {node(),self()} ,Start_x,Start_y,End_x,End_y},
    receive
        {ack, Id} ->
            Id
    end;
    
notify(kill,Id) ->
    Process = get_drone_hub_process(),
    Process ! {kill, {node(),self()} ,Id},
    receive
        {success, Id} -> Id;
        {error, Response} -> Response
    end.

get_drone_hub_process() ->
    Process = list_to_atom(os:getenv("DRONE_HUB_PROCESS")),
    Host = list_to_atom(os:getenv("DRONE_HUB_HOST")),
    {Process, Host}.
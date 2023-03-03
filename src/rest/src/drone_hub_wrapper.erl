-module(drone_hub_wrapper).
-include("records.hrl").
-export([notify/2]).



notify(create,{Start_x,Start_y,End_x,End_y}) ->
    {drone_hub, 'drone_hub@drone_hub_host'} ! {create, {node(),self()} ,Start_x,Start_y,End_x,End_y},
    receive
        {ack, Id} ->
            Id
    end;
    
notify(kill,Id) ->
    {drone_hub, 'drone_hub@drone_hub_host'} ! {kill, {node(),self()} ,Id},
    receive
        {success, Id} -> Id;
        {error, Response} -> Response
    end.
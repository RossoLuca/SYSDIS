-module(drone_hub_wrapper).
-include("records.hrl").
-export([notify/2]).



notify(create,{Start_x,Start_y,End_x,End_y}) ->
    %TODO CAMBIARE HOST
    {drone_hub, 'drone_hub@drone_hub_host'} ! {create, {node(),self()} ,Start_x,Start_y,End_x,End_y},
    erlang:display({Start_x,Start_y,End_x,End_y});
    % handle_response().
    
notify(kill,Id) ->
    {drone_hub, 'drone_hub@drone_hub_host'} ! {kill, {node(),self()} ,Id},
    erlang:display(Id).
    % handle_response().

% handle_response() -> SOME KIND OF FUNCTION

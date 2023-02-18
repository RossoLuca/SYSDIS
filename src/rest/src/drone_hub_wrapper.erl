-module(drone_hub_wrapper).
-include("records.hrl").
-export([notify/2]).



notify(create,{Start_x,Start_y,End_x,End_y}) ->
    %TODO CAMBIARE HOST
    {drone_hub, 'drone_hub@drone_hub'} ! {create, {node(),self()} ,Start_x,Start_y,End_x,End_y},
    erlang:display(Data);
    % handle_response().
    
notify(kill,Id) ->
    {drone_hub, 'drone_hub@drone_hub'} ! {kill, {node(),self()} ,Id},
    erlang:display(Id).
    % handle_response().

% handle_response() -> SOME KIND OF FUNCTION

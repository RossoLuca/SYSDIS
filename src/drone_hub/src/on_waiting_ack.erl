-module(on_waiting_ack).

-export([handle_state/7]).


handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified) ->
    io:format("Drone ~p --> Waiting ack messages from drones: ~p~n", [Id, Notified]),
    receive
        Any -> io:format("Drone ~p --> Received: ~p~n", [Id, Any])
    end,
    flying:handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions).
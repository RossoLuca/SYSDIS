-module(flying).

-export([handle_state/6]).


handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions) -> 
    io:format("Drone ~p --> Started to fly~n", [Id]),
    receive
        Any -> io:format("Drone ~p --> Received: ~p~n", [Id, Any])
    end,
    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions).
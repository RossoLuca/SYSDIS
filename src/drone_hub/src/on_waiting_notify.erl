-module(on_waiting_notify).

-export([handle_state/6]).


handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions) ->
    WaitingFrom = maps:get(collisions, maps:get(Id, CollisionTable)),
    Size = length(WaitingFrom),
    if Size == 0 ->
        flying:handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions);
    true ->
        io:format("Drone ~p --> Waiting notify messages from ~p~n", [Id, WaitingFrom]),
        %% TODO: Da mettere il receive dei notify
        receive
            {sync_hello, FromPid, FromId} ->
                io:format("Drone ~p --> Received sync_hello message from drone ~p~n", [Id, FromId]),
            
                Start = maps:get(route_start, Configuration),
                End = maps:get(route_end, Configuration),
                State = maps:get(state, DroneState),
                Ack_count = maps:get(ack_count, DroneState),
                
                FromPid ! {sync_routes, self(), Id, Start, End, State, Ack_count};
            {sync_result, FromPid, FromId, Collision_response, Collision_points} ->
                NewPersonalCollisions = drone_main:update_personal_collisions(Collision_response, FromId, FromPid, Collision_points, PersonalCollisions),
                                    
                if Collision_response == no_collision ->
                    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollision);
                true ->
                    io:format("Drone ~p --> Sending update table in agreement_loop~n", [Id]),
                    send_update_table()
                end;

        end,
        handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions)
    end.
-module(on_waiting_ack).

-export([handle_state/8]).


handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks) ->
    receive
        {sync_hello, FromPid, FromId, FromMainPid, FromRoute} ->
            io:format("Drone ~p --> Received sync_hello message from drone ~p~n to compute collision computation~n", [Id, FromMainPid]),
            
            DroneSize = maps:get(drone_size, Configuration),
            MyStart = drone_main:get_route_start(Configuration),
            MyEnd = maps:get(route_end, Configuration),
            
            {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
            NewPersonalCollisions = drone_main:update_personal_collisions(Collision_response, FromMainPid, FromId, Collision_points, PersonalCollisions),
                
            FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},
            handle_state(Id, Configuration, DroneState, CollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, Notified, ReceivedAcks);

        {update_table, FromPid, FromId, _Action, FromCollidingDrones, FromState, FromAck_count} ->
            Find = maps:get(FromId, CollisionTable, not_exists),

            if Find == not_exists ->
                io:format("Drone ~p --> Received update_table message from the new drone ~p while waiting acks~n", [Id, FromId]),
                Collisions = #{
                    collisions => FromCollidingDrones,
                    state => FromState,
                    ack_count => FromAck_count
                },
                %% Must be added a new entry in for the drone FromId in the CollisionTable
                %% Moreover, must be update the field collision for the personal entry in the CollisionTable
                %% adding the collision with the drone FromId
                NewCollisionTable = maps:put(FromId, Collisions, CollisionTable),
                MyCollisions = maps:get(collisions, maps:get(Id, NewCollisionTable)),
                NewMyCollisions = [FromId | MyCollisions],
                UpdatedCollisionTable = maps:put(Id, maps:put(collisions, NewMyCollisions, maps:get(Id, NewCollisionTable)), NewCollisionTable),

                %% Must be propagated the updated version of the CollisionTable to all the other colliding drones
                %% and also the new drone FromId
                CollidingDrones = maps:keys(PersonalCollisions),
                State = maps:get(state, DroneState),
                Ack_count = maps:get(ack_count, DroneState),
                % FromPid = maps:get(pid, maps:get(FromId, PersonalCollisions)),
                maps:foreach(fun(K, _V) ->
                        if K =/= Id ->
                            DronePid = maps:get(pid, maps:get(K, PersonalCollisions)),
                            DronePid ! {update_table, self(), Id, add, CollidingDrones, State, Ack_count};
                        true ->
                            ok
                        end
                    end, UpdatedCollisionTable),
                %% FromPid ! {update_table, self(), Id, add, CollidingDrones, State, Ack_count},


                Send = check_send_notify(FromId, UpdatedCollisionTable, Notified),
                if Send == true ->
                    NewNotified = [FromId | Notified],
                    FromPid ! {notify, self(), Id},
                    io:format("Drone ~p --> Waiting ack messages from drones: ~p~n", [Id, NewNotified]),
                    handle_state(Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, NewNotified, ReceivedAcks);
                true ->
                    handle_state(Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks)
                end;

            true ->
                Collisions = #{
                    collisions => FromCollidingDrones,
                    state => FromState,
                    ack_count => FromAck_count
                },
                UpdatedCollisionTable = maps:put(FromId, Collisions, CollisionTable), 
                Send = check_send_notify(FromId, UpdatedCollisionTable, Notified),

                if Send == true ->
                    NewNotified = [FromId | Notified],
                    FromPid ! {notify, self(), Id},
                    io:format("Drone ~p --> Waiting ack messages from drones: ~p~n", [Id, NewNotified]),
                    handle_state(Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, NewNotified, ReceivedAcks);
                true ->
                    handle_state(Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks)
                end
            end;
        {ack, _FromPid, FromId} ->
            io:format("Drone ~p -> Received ack message from ~p~n", [Id, FromId]),

            Find = lists:member(FromId, Notified),
            if Find == false ->
                io:format("Drone ~p --> Received an unwaited ack from drone ~p~n", [Id, FromId]),
                handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks);
            true ->
                NewReceivedAcks = [FromId | ReceivedAcks],
                SizeNotified = length(Notified),
                SizeReceivedAcks = length(NewReceivedAcks),

                if SizeNotified == SizeReceivedAcks ->
                    NewCollisionTable = maps:fold(fun(K, V, Map) -> 
                                            FindDrone = lists:member(K, NewReceivedAcks),
                                            if FindDrone == true ->
                                                Map;
                                            true ->
                                                MapOut = maps:put(K, V, Map),
                                                MapOut
                                            end
                                        end, #{}, CollisionTable),
                    MyEntryCollisionTable = #{
                                    ack_count => lists:append(maps:get(ack_count, maps:get(Id, CollisionTable)), NewReceivedAcks),
                                    state => pending,
                                    collisions => lists:subtract(maps:get(collisions, maps:get(Id, CollisionTable)), NewReceivedAcks)        
                    },
                    UpdatedCollisionTable = maps:put(Id, MyEntryCollisionTable, NewCollisionTable),
                    NewPersonalCollisions = maps:fold(fun(K, V, Map) ->
                                                FindDrone = lists:member(K, NewReceivedAcks),
                                                if FindDrone == true ->
                                                    Map;
                                                true ->
                                                    MapOut = maps:put(K, V, Map),
                                                    MapOut
                                                end
                                            end, #{}, PersonalCollisions),
                    NewDroneState = #{
                        state => pending,
                        fallen => maps:get(fallen, DroneState),
                        ack_count => maps:get(ack_count, maps:get(Id, UpdatedCollisionTable))
                    },
                    drone_main:send_update_table(Id, NewPersonalCollisions, NewDroneState),
                    drone_main:agreement_loop(Id, Configuration, NewDroneState, UpdatedCollisionTable, NewDrones, NewPersonalCollisions);
                true ->
                    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, NewReceivedAcks)
                end      
            end                                  
    end.


check_send_notify(FromId, CollisionTable, Notified) ->
    Collisions = maps:get(collisions, maps:get(FromId, CollisionTable)),
    SetNotified = sets:from_list(Notified),
    SetCollisions = sets:from_list(Collisions),
    Dependencies = sets:size(sets:intersection(SetCollisions, SetNotified)),
    if Dependencies == 0 ->
        true;
    true ->
        false
    end.
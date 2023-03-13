-module(on_waiting_ack).

-export([handle_state/9]).


handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, ToNotUpdate) ->
    receive
        {sync_hello, FromPid, FromId, FromMainPid, FromRoute} ->
            logging:log(Id, "Received sync_hello message from drone ~p to compute collision computation", [FromId]),
            
            DroneSize = maps:get(drone_size, Configuration),
            MyStart = utils:get_route_start(Configuration),
            MyEnd = maps:get(route_end, Configuration),
            
            {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
            NewPersonalCollisions = utils:update_personal_collisions(Collision_response, FromMainPid, FromId, Collision_points, PersonalCollisions),

            FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},

            %% Is checked if the drone that sent the sync_hello is already stored in our CollisionTable
            %% If this is the case, this means that sync_hello has been received from a recovery drone
            %% So, it must be added to the ToNotUpdate buffer
            %% ToNotUpdate is used in order to avoid to send an update_table, when all acks have been received
            %% to drones inside it.
            %% This is done because it is possible that an erroneus update_table message is sent to a recovery drone that is
            %% in its sync_phase.
            %% Indeed, whenever is received an update_table message from someone that is include in ToNotUpdate, then it
            %% is removed from this buffer and treated like all the other drones
            AlreadyInCollisionTable = maps:get(FromId, CollisionTable, false),
            NewToNotUpdate = if AlreadyInCollisionTable =/= false ->
                                sets:add_element(FromId, ToNotUpdate);
                            true ->
                                ToNotUpdate
                            end,

            AlreadyNotified = sets:is_element(FromId, Notified),
            DroneAcked = lists:member(FromId, ReceivedAcks),
            NewNotified = if AlreadyNotified == true, DroneAcked == false ->
                            sets:del_element(FromId, Notified);
                        true ->
                            Notified
                        end,

            handle_state(Id, Configuration, DroneState, CollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, NewNotified, ReceivedAcks, NewToNotUpdate);

        {update_table, FromPid, FromId, _Action, FromCollidingDrones, FromState, FromNotify_count} ->

            IsCorrectPid = utils:check_drone_pid(FromPid, FromId, PersonalCollisions),
        
            if IsCorrectPid == true ->
                    Find = maps:get(FromId, CollisionTable, not_exists),
                    if Find == not_exists ->
                        logging:log(Id, "Received update_table message from the new drone ~p while waiting acks", [FromId]),
                        %% Must be added a new entry in for the drone FromId in the CollisionTable
                        %% Moreover, must be update the field collision for the personal entry in the CollisionTable
                        %% adding the collision with the drone FromId
                        NewCollisionTable = utils:update_entry_in_collision_table(FromId, CollisionTable, FromCollidingDrones, FromState, FromNotify_count),
                        
                        MyCollisions = maps:get(collisions, maps:get(Id, NewCollisionTable)),
                        NewMyCollisions = sets:add_element(FromId, MyCollisions),
                        UpdatedCollisionTable = maps:put(Id, maps:put(collisions, NewMyCollisions, maps:get(Id, NewCollisionTable)), NewCollisionTable),

                        %% Must be propagated the updated version of the CollisionTable to all the other colliding drones
                        %% and also the new drone FromId
                        CollidingDrones = maps:keys(PersonalCollisions),
                        State = maps:get(state, DroneState),
                        Notify_count =  maps:get(notify_count, DroneState),
                        
                        maps:foreach(fun(K, _V) ->
                                if K =/= Id ->
                                    DronePid = maps:get(pid, maps:get(K, PersonalCollisions)),
                                    %% We need to check to don't send the update_table message to a drone that is 
                                    %% waiting a notify from us
                                    SentNotify = sets:is_element(K, Notified),
                                    InToNotUpdate = sets:is_element(K, ToNotUpdate),
                                    if SentNotify == false, InToNotUpdate == false ->
                                        DronePid ! {update_table, self(), Id, add, CollidingDrones, State, Notify_count};
                                    true ->
                                        ok
                                    end;
                                true ->
                                    ok
                                end
                            end, UpdatedCollisionTable),
                        
                        %% Must be checked if also a notify can be sent to the drone FromId 
                        %% (but only if the number of already sent notify is less than the Notify_Threshold)
                        Send = check_send_notify(FromId, UpdatedCollisionTable, Notified, Configuration, DroneState),
                        
                        if Send == true ->
                            NewNotified = sets:add_element(FromId, Notified),
                            FromPid ! {notify, self(), Id},
                            logging:log(Id, "Sent notify to drone ~p", [FromId]),
                            NewDroneState = #{
                                notify_count => sets:to_list(sets:from_list(lists:append(maps:get(notify_count, DroneState), [FromId]))),
                                state => pending,
                                fallen => maps:get(fallen, DroneState)
                            },
                            CollisionTableAfterNotify = maps:put(Id,
                                                            maps:put(notify_count, 
                                                            lists:append(maps:get(notify_count, NewDroneState), [FromId]), 
                                                            maps:get(Id, UpdatedCollisionTable)),
                                                        UpdatedCollisionTable),
            
                            handle_state(Id, Configuration, NewDroneState, CollisionTableAfterNotify, NewDrones, PersonalCollisions, NewNotified, ReceivedAcks, ToNotUpdate);
                        true ->
                            handle_state(Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, ToNotUpdate)
                        end;

                    true ->
                        UpdatedCollisionTable = utils:update_entry_in_collision_table(FromId, CollisionTable, FromCollidingDrones, FromState, FromNotify_count),


                        InToNotUpdate = sets:is_element(FromId, ToNotUpdate),
                        NewToNotUpdate = if InToNotUpdate == true ->
                                            CollidingDrones = maps:keys(PersonalCollisions),
                                            State = maps:get(state, DroneState),
                                            Notify_count =  maps:get(notify_count, DroneState),
                                            FromPid ! {update_table, self(), Id, add, CollidingDrones, State, Notify_count},
                                            sets:del_element(FromId, ToNotUpdate);
                                        true ->
                                            ToNotUpdate
                                        end,

                        ReceivedAllAcks = check_received_all_acks(Notified, ReceivedAcks),

                        if ReceivedAllAcks == true ->
                            go_to_agreement(Id, Configuration, DroneState, UpdatedCollisionTable, PersonalCollisions, NewDrones, ReceivedAcks, NewToNotUpdate);
                        true ->
                            
                            %% Must be checked if also a notify can be sent to the drone FromId 
                            %% (but only if the number of already sent notify is less than the Notify_Threshold)
                            Send = check_send_notify(FromId, UpdatedCollisionTable, Notified, Configuration, DroneState),                    
                    
                            if Send == true ->
                                NewNotified = sets:add_element(FromId, Notified),
                                FromPid ! {notify, self(), Id},
                                logging:log(Id, "Sent notify to ~p", [FromId]),
                                
                                NewDroneState = #{
                                    notify_count => sets:to_list(sets:from_list(lists:append(maps:get(notify_count, DroneState), [FromId]))),
                                    state => pending,
                                    fallen => maps:get(fallen, DroneState)
                                },

                                CollisionTableAfterNotify = maps:put(Id,
                                                                    maps:put(notify_count, 
                                                                        lists:append(maps:get(notify_count, NewDroneState), [FromId]), 
                                                                        maps:get(Id, UpdatedCollisionTable)),
                                                                    UpdatedCollisionTable),

                                handle_state(Id, Configuration, NewDroneState, CollisionTableAfterNotify, NewDrones, PersonalCollisions, NewNotified, ReceivedAcks, NewToNotUpdate);
                            true ->
                                handle_state(Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, NewToNotUpdate)
                            end
                        end
                    end;
            true ->
                handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, ToNotUpdate)
            end;
        {ack, FromPid, FromId} ->

            IsCorrectPid = utils:check_drone_pid(FromPid, FromId, PersonalCollisions),

            if IsCorrectPid == true ->
            
                logging:log(Id, "Received ack message from ~p", [FromId]),

                Find = sets:is_element(FromId, Notified),
                if Find == false ->
                    logging:log(Id, "Received an unwaited ack from drone ~p", [FromId]),
                    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, ToNotUpdate);
                true ->
                    NewReceivedAcks = [FromId | ReceivedAcks],
                    ReceivedAll = check_received_all_acks(Notified, NewReceivedAcks),

                    if ReceivedAll == true ->
                        %% When all the awaiting acks have been received, the CollisionTable is updated removing the entry of the drones
                        %% for which we received an ack, and also updating the personal entry in the CollisionTable removing from the collision field
                        %% the drones for which we received the ack 
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
                                        notify_count => maps:get(notify_count, DroneState),
                                        state => pending,
                                        collisions => sets:subtract(maps:get(collisions, maps:get(Id, CollisionTable)), sets:from_list(NewReceivedAcks))        
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
                        send_update_table_with_collision_table(Id, NewPersonalCollisions, UpdatedCollisionTable, DroneState, ToNotUpdate),
                        drone_main:agreement_loop(Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, NewPersonalCollisions, ToNotUpdate);
                    true ->
                        handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, NewReceivedAcks, ToNotUpdate)
                    end
                end;
            true ->
                logging:log(Id, "Received ack message from the failed drone ~p", [FromId]),
                handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, ToNotUpdate)
            end;
        {notify, _FromPid, FromId} ->
            
            logging:log(Id, "Received an unwaited notify from drone ~p while waiting acks", [FromId]),

            handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, ToNotUpdate)   
    end.

check_received_all_acks(Notified, ReceivedAcks) ->
    ReceivedAll = sets:fold(fun(NotifiedDrone, Flag) ->
        Find = lists:member(NotifiedDrone, ReceivedAcks),
        if Flag == true ->
            if Find == false ->
                false;
            true ->
                true
            end;
        true ->
            Flag
        end
    end, true, Notified),
    ReceivedAll.


check_send_notify(FromId, CollisionTable, Notified, Configuration, DroneState) ->
    Collisions = maps:get(collisions, maps:get(FromId, CollisionTable)),
    Dependencies = sets:size(sets:intersection(Collisions, Notified)),
    Notify_Threshold = maps:get(notify_threshold, Configuration),
    Notify_count = length(maps:get(notify_count, DroneState)),
    if Notify_count < Notify_Threshold ->
        if Dependencies == 0 ->
            true;
        true ->
            false
        end;
    true ->
        false
    end.
    


send_update_table_with_collision_table(Id, PersonalCollisions, CollisionTable, DroneState, ToNotUpdate) ->
    maps:foreach(fun(External_Id, _Entry) ->
        InToNotUpdate = sets:is_element(External_Id, ToNotUpdate),
        if External_Id =/= Id, InToNotUpdate == false ->
            External_Pid = maps:get(pid, maps:get(External_Id, PersonalCollisions)),
            utils:send_update_table_add(Id, External_Pid, PersonalCollisions, DroneState),
            logging:log(Id, "Sent update_table message to drone ~p", [External_Id]);
        true -> 
            ok
        end    
    end, CollisionTable).

go_to_agreement(Id, Configuration, DroneState, CollisionTable, PersonalCollisions, NewDrones, ReceivedAcks, ToNotUpdate) ->
        NewCollisionTable = maps:fold(fun(K, V, Map) -> 
                                FindDrone = lists:member(K, ReceivedAcks),
                                if FindDrone == true ->
                                    Map;
                                true ->
                                    MapOut = maps:put(K, V, Map),
                                    MapOut
                                end
                            end, #{}, CollisionTable),
        MyEntryCollisionTable = #{
                        notify_count => sets:to_list(sets:from_list(lists:append(maps:get(notify_count, maps:get(Id, CollisionTable)), ReceivedAcks))),
                        state => pending,
                        collisions => sets:subtract(maps:get(collisions, maps:get(Id, CollisionTable)), sets:from_list(ReceivedAcks))        
        },
        UpdatedCollisionTable = maps:put(Id, MyEntryCollisionTable, NewCollisionTable),
        NewPersonalCollisions = maps:fold(fun(K, V, Map) ->
                                    FindDrone = lists:member(K, ReceivedAcks),
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
            notify_count => maps:get(notify_count, maps:get(Id, UpdatedCollisionTable))
        },
        send_update_table_with_collision_table(Id, NewPersonalCollisions, UpdatedCollisionTable, DroneState, ToNotUpdate),
        drone_main:agreement_loop(Id, Configuration, NewDroneState, UpdatedCollisionTable, NewDrones, NewPersonalCollisions, ToNotUpdate).
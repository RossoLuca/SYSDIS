-module(synchronization).

-export([sync_loop/8, test_table/7]).

sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, NewDrones, PersonalCollisions, ToNotUpdate) ->
    receive
        {collision_response, External_Pid, External_Id, Collision_response, Collision_points} ->
            
            ReceivedResultFlag = maps:get(received_result, maps:get(External_Id, SynchronizationMap)),

            %% This is done because it's possible that during the sync loop I received a sync hello message from a drone for which I have created a drone synchronizer that
            %% has fallen during my sync phase
            {NewPersonalCollisions, NewSynchronizationMap} = if ReceivedResultFlag == true ->
                                                                {PersonalCollisions, SynchronizationMap};
                                                            true ->                     
                                                                %% According to the response from the collision computation, I can update my synchronization map
                                                                TmpMap = maps:get(External_Id, SynchronizationMap),
                                                                SendMyTableFlag = case Collision_response of
                                                                                        collision -> true;
                                                                                        no_collision -> false
                                                                                    end,
                                                                NewMap = #{
                                                                    pid => maps:get(pid, TmpMap),
                                                                    received_result => true,
                                                                    send_my_table => SendMyTableFlag
                                                                },
                                                                New_Synchronization_Map = maps:put(External_Id, NewMap, SynchronizationMap),

                                                                %% According to the response from the collision computation, I can update my personal collisions
                                                                New_Personal_Collisions = utils:update_personal_collisions(Collision_response, External_Pid, External_Id, Collision_points, PersonalCollisions),
                                                                {New_Personal_Collisions, New_Synchronization_Map}
                                                            end,
            
            %% FilteredMap contains all the drones in the NewSynchronizationMap for which I know the result of the collision computation
            %% If the size of this map is equal to 0, this means that I know all my collision and then I can send an update_table message to
            %% each of the drones colliding with me
            FilteredMap = maps:filter(fun(_K, V) ->
                            Flag = maps:get(received_result, V),
                            if Flag == true -> 
                                false;
                            true ->
                                true
                            end                                
                    end, NewSynchronizationMap),
            
            Size = maps:size(FilteredMap),
            if Size == 0 ->
                
                %% Since I have received all the collision computance from all the other drones, I can insert the row 
                %% regarding myself in my collision table
                Map = #{
                    collisions => sets:from_list(maps:keys(NewPersonalCollisions)),
                    state => maps:get(state, DroneState),
                    notify_count => maps:get(notify_count, DroneState)
                },
                NewCollisionTable = maps:put(Id, Map, CollisionTable),
                
                %% So, since I know all my collisions I'm ready to send update_table message to drones involved with me
                %% I'm also sure that in can get in this if only once, since the the condition Size == 0 is evaluated only
                %% when I receive a sync_result from other drones (so once I get them all I never get in this receive clause)
                send_update_table(Id, NewPersonalCollisions, DroneState),

                
                %% The following condition will be evaluated to true only when the current drone hasn't collisions with other drones
                StartAgreement = drone_main:go_to_agreement(NewCollisionTable, NewSynchronizationMap),
                if StartAgreement == true ->
                    test_table(Id, Configuration, DroneState, NewCollisionTable, NewDrones, NewPersonalCollisions, ToNotUpdate);
                true ->
                    sync_loop(Id, Configuration, DroneState, NewCollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions, ToNotUpdate)
                end;
            true ->
                sync_loop(Id, Configuration, DroneState, CollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions, ToNotUpdate)
            end;

        {sync_hello, FromPid, FromId, FromRoute} ->
            MyStart = utils:get_route_start(Configuration),
            MyEnd = maps:get(route_end, Configuration),
            DroneSize = maps:get(drone_size, Configuration),

            {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
            NewPersonalCollisions = utils:update_personal_collisions(Collision_response, FromPid, FromId, Collision_points, PersonalCollisions),
            
            Map = maps:put(received_result, true, maps:get(FromId, SynchronizationMap)),
            NewSynchronizationMap = maps:put(FromId, Map, SynchronizationMap),

            AlreadyInCollisionTable = maps:get(FromId, CollisionTable, false),
            UpdatedCollisionTable = if AlreadyInCollisionTable =/= false ->
                                        maps:remove(FromId, CollisionTable);
                                    true ->
                                        CollisionTable
                                    end,

            FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},
            sync_loop(Id, Configuration, DroneState, UpdatedCollisionTable, NewSynchronizationMap, maps:put(FromId, FromPid, NewDrones), NewPersonalCollisions, ToNotUpdate);
        
        {update_table, FromPid, FromId, Action, FromCollidingDrones, FromState, FromNotify_count} ->

            % logging:log(Id, "Received update_table message from drone ~p with action ~p", [FromId, Action]),
            %% We need to check that we received an update_table message from a drone that is already in our PersonalCollisions
            %% Otherwise this could be an old message
            Drone = maps:get(FromId, PersonalCollisions, not_exists),

            AlreadyReceived = maps:get(FromId, CollisionTable, not_exists),

            %% If we already received an update_table message from a drone with the same ID, this means that this last message comes from a recovery drone
            %% so after the receive of the update_table message we need also to send back an update_table message about our collisions to this drone
            if AlreadyReceived =/= not_exists ->
                CollidingDrones = maps:keys(PersonalCollisions),
                State = maps:get(state, DroneState),
                Notify_count = maps:get(notify_count, DroneState),
                FromPid ! {update_table, self(), Id, add, CollidingDrones, State, Notify_count};
            true ->
                ok
            end,

            NewSynchronizationMap = if Action == remove ->
                                            maps:put(FromId, maps:put(send_my_table, false, maps:get(FromId, SynchronizationMap)), SynchronizationMap);
                                    true ->
                                        SynchronizationMap
                                    end,

            if Drone =/= not_exists ->
                    NewCollisionTable = if Action =/= remove ->
                                            utils:update_entry_in_collision_table(FromId,
                                                                        CollisionTable,
                                                                        FromCollidingDrones,
                                                                        FromState,
                                                                        FromNotify_count);
                                        true ->
                                            MyEntryCollisionTable = #{
                                                collisions => sets:del_element(FromId, maps:get(collisions, maps:get(Id, CollisionTable))),
                                                notify_count => maps:get(notify_count, maps:get(Id, CollisionTable)),
                                                state => pending
                                            },
                                            maps:put(Id, MyEntryCollisionTable, CollisionTable)
                                        end,
                    NewPersonalCollisions = if Action =/= remove ->
                                                PersonalCollisions;
                                            true ->
                                                maps:remove(FromId, PersonalCollisions)
                                            end,

                    StartAgreement = drone_main:go_to_agreement(NewCollisionTable, NewSynchronizationMap),

                    if StartAgreement == true ->
                        test_table(Id, Configuration, DroneState, NewCollisionTable, NewDrones, NewPersonalCollisions, ToNotUpdate);
                    true ->
                        sync_loop(Id, Configuration, DroneState, NewCollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions, ToNotUpdate)
                    end;
            true ->
                sync_loop(Id, Configuration, DroneState, CollisionTable, NewSynchronizationMap, NewDrones, PersonalCollisions, ToNotUpdate)
            end
    end.


test_table(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, ToNotUpdate) ->
    test ! {collision_table, Id, CollisionTable},
    loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, ToNotUpdate).


loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, ToNotUpdate) ->
    receive
        {sync_hello, FromPid, FromId, _FromMainPid, FromRoute} ->
            MyStart = utils:get_route_start(Configuration),
            MyEnd = maps:get(route_end, Configuration),
            DroneSize = maps:get(drone_size, Configuration),

            {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
            NewPersonalCollisions = utils:update_personal_collisions(Collision_response, FromPid, FromId, Collision_points, PersonalCollisions),


            AlreadyInCollisionTable = maps:get(FromId, CollisionTable, false),
            UpdatedCollisionTable = if AlreadyInCollisionTable =/= false ->
                                        maps:remove(FromId, CollisionTable);
                                    true ->
                                        CollisionTable
                                    end,

            FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},

            loop(Id, Configuration, DroneState, UpdatedCollisionTable, maps:put(FromId, FromPid, NewDrones), NewPersonalCollisions, ToNotUpdate);
        {update_table, FromPid, FromId, _Action, FromCollidingDrones, FromState, FromNotify_count} ->

            NewCollisionTable = utils:update_entry_in_collision_table(FromId, CollisionTable, FromCollidingDrones, FromState, FromNotify_count),
            utils:send_update_table_add(Id, FromPid, PersonalCollisions, DroneState),

            test ! {collision_table, Id, NewCollisionTable},

            loop(Id, Configuration, DroneState, NewCollisionTable, NewDrones, PersonalCollisions, ToNotUpdate)
    end.


send_update_table(Id, PersonalCollisions, DroneState) ->
    maps:foreach(fun(_External_Id, Entry) ->
            External_Pid = maps:get(pid, Entry),
            utils:send_update_table_add(Id, External_Pid, PersonalCollisions, DroneState)
        end, PersonalCollisions). 

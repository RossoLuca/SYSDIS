-module(on_waiting_ack).

-export([handle_state/11]).


handle_state(MessageOrder, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, Ordering, ToNotUpdate) ->
    receive
        {sync_hello, FromPid, FromId, FromMainPid, FromRoute} ->
            io:format("Drone ~p [~p] --> Received sync_hello message from drone ~p~n to compute collision computation~n", [Id, MessageOrder, FromMainPid]),
            
            DroneSize = maps:get(drone_size, Configuration),
            MyStart = drone_main:get_route_start(Configuration),
            MyEnd = maps:get(route_end, Configuration),
            
            {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
            NewPersonalCollisions = drone_main:update_personal_collisions(Collision_response, FromMainPid, FromId, Collision_points, PersonalCollisions),

            FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},


            AlreadyInCollisionTable = maps:get(FromId, CollisionTable, false),
            NewToNotUpdate = if AlreadyInCollisionTable =/= false ->
                                sets:add_element(FromId, ToNotUpdate);
                            true ->
                                ToNotUpdate
                            end,

            % UpdatedCollisionTable = if AlreadyInCollisionTable =/= false ->
            %                             remove_from_all(FromId, maps:remove(FromId, CollisionTable));
            %                         true ->
            %                             CollisionTable
            %                         end,

            AlreadyNotified = sets:is_element(FromId, Notified),
            DroneAcked = lists:member(FromId, ReceivedAcks),
            NewNotified = if AlreadyNotified == true, DroneAcked == false ->
                            sets:del_element(FromId, Notified);
                        true ->
                            Notified
                        end,

            handle_state(MessageOrder, Id, Configuration, DroneState, CollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, NewNotified, ReceivedAcks, Ordering, NewToNotUpdate);


            %% When I receive a sync_hello message from drone FromId, if I understand that FromId has fallen and I'm waiting ack from it, first of all I need to remove the drone from Notified
            %% and check if now I have received all the acks that I'm waiting.
            %% If some acks are still missing, I have to check if I can send a notify to some other drone (only if notify_count < NOTIFY_THRESHOLD).
            %% Otherwise, if I received all the acks then I can return to the agreement phase
            % if AlreadyNotified == true, StillNotAcked == false ->
            %     NewNotified = sets:del_element(FromId, Notified),
            %     ReceivedAllAcks = check_received_all_acks(Notified, ReceivedAcks),
            %     if ReceivedAllAcks == true ->
            %         go_to_agreement(MessageOrder, Id, Configuration, DroneState, CollisionTable, PersonalCollisions, NewDrones, ReceivedAcks);
            %     true ->
            %         {NewDroneState, NewCollisionTable, UpdatedNotified} = check_send_new_notify(Id, Configuration, DroneState, NewNotified, ReceivedAcks, UpdatedCollisionTable, NewPersonalCollisions, Ordering),
            %         handle_state(MessageOrder, Id, Configuration, NewDroneState, NewCollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, UpdatedNotified, ReceivedAcks, Ordering)
            %     end;
            % true ->
            %     handle_state(MessageOrder, Id, Configuration, DroneState, UpdatedCollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, Notified, ReceivedAcks, Ordering)
            % end;
            
            %% If for each notified drones there's a received ack, then we must go to the agreement loop otherwise we can loop on the same state
            % check_notified_drones(MessageOrder + 1, Id, Configuration, DroneState, UpdatedCollisionTable, NewPersonalCollisions, NewDrones, NewNotified, NewReceivedAcks, Ordering); 
            % handle_state(MessageOrder, Id, Configuration, DroneState, CollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, NewNotified, ReceivedAcks, Ordering, NewToNotUpdate);

        {update_table, FromPid, FromId, _Action, FromCollidingDrones, FromState, FromNotify_count} ->
            % ExternalPid = maps:get(pid, maps:get(FromId, PersonalCollisions)),
            % if FromPid =/= ExternalPid ->
            %     io:format("Drone ~p --> Update_table message from older drone ~p~n", [Id, FromId]);
            % true ->
            %     ok
            % end,

            FindInPersonalCollisions = maps:get(FromId, PersonalCollisions, false),
            StoredPid = if FindInPersonalCollisions =/= false ->
                            maps:get(pid, FindInPersonalCollisions);
                        true ->
                            false
                        end,
            PidConsistency = (StoredPid =/= false) andalso (StoredPid == FromPid),
            % io:format("Drone ~p [~p] --> While waiting acks, FindInPersonalCollisions: ~p, StoredPid: ~p, FromPid: ~p, PidConsistency: ~p~n", [Id, MessageOrder, FindInPersonalCollisions, StoredPid, FromPid, PidConsistency]),
            if PidConsistency == true ->
                    Find = maps:get(FromId, CollisionTable, not_exists),
                    if Find == not_exists ->
                        io:format("Drone ~p [~p] --> Received update_table message from the new drone ~p while waiting acks~n", [Id, MessageOrder + 1, FromId]),
                        Collisions = #{
                            collisions => sets:from_list(FromCollidingDrones),
                            state => FromState,
                            notify_count => FromNotify_count
                        },
                        %% Must be added a new entry in for the drone FromId in the CollisionTable
                        %% Moreover, must be update the field collision for the personal entry in the CollisionTable
                        %% adding the collision with the drone FromId
                        NewCollisionTable = maps:put(FromId, Collisions, CollisionTable),
                        MyCollisions = maps:get(collisions, maps:get(Id, NewCollisionTable)),
                        NewMyCollisions = sets:add_element(FromId, MyCollisions),
                        UpdatedCollisionTable = maps:put(Id, maps:put(collisions, NewMyCollisions, maps:get(Id, NewCollisionTable)), NewCollisionTable),

                        %% Must be propagated the updated version of the CollisionTable to all the other colliding drones
                        %% and also the new drone FromId
                        CollidingDrones = maps:keys(PersonalCollisions),
                        State = maps:get(state, DroneState),
                        Notify_count =  maps:get(notify_count, DroneState),
                        % io:format("Drone ~p [~p] --> TEST NOTIFY_COUNT: ~p~n", [Id, MessageOrder + 2, Notify_count]),
                        % FromPid = maps:get(pid, maps:get(FromId, PersonalCollisions)),
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
                        %% FromPid ! {update_table, self(), Id, add, CollidingDrones, State, Ack_count},


                        Send = check_send_notify(FromId, UpdatedCollisionTable, Notified, Configuration),
                        % Send = false,
                        if Send == true ->
                            NewNotified = sets:add_element(FromId, Notified),
                            FromPid ! {notify, self(), Id},
                            io:format("Drone ~p [~p] --> Sent notify to drone ~p~n", [Id, MessageOrder + 3, FromId]),
                            NewDroneState = #{
                                notify_count => lists:append(maps:get(notify_count, DroneState), [FromId]),
                                state => pending,
                                fallen => maps:get(fallen, DroneState)
                            },
                            CollisionTableAfterNotify = maps:put(Id,
                                                            maps:put(notify_count, 
                                                            lists:append(maps:get(notify_count, NewDroneState), [FromId]), 
                                                            maps:get(Id, UpdatedCollisionTable)),
                                                        UpdatedCollisionTable),
            

                            io:format("Drone ~p [~p] --> Waiting ack messages from drones: ~p~n", [Id, MessageOrder + 4, NewNotified]),
                            handle_state(MessageOrder + 5, Id, Configuration, NewDroneState, CollisionTableAfterNotify, NewDrones, PersonalCollisions, NewNotified, ReceivedAcks, Ordering, ToNotUpdate);
                        true ->
                            handle_state(MessageOrder + 3, Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, Ordering, ToNotUpdate)
                        end;

                    true ->
                        Collisions = #{
                            collisions => sets:from_list(FromCollidingDrones),
                            state => FromState,
                            notify_count => FromNotify_count
                        },
                        UpdatedCollisionTable = maps:put(FromId, Collisions, CollisionTable), 


                        io:format("Drone ~p --> ToNotUpdate: ~p~n", [Id, ToNotUpdate]),
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
                            go_to_agreement(MessageOrder, Id, Configuration, DroneState, UpdatedCollisionTable, PersonalCollisions, NewDrones, ReceivedAcks, NewToNotUpdate);
                        true ->
                            
                            Send = check_send_notify(FromId, UpdatedCollisionTable, Notified, Configuration),                    
                    
                            if Send == true ->
                                NewNotified = sets:add_element(FromId, Notified),
                                FromPid ! {notify, self(), Id},
                                % io:format("Drone ~p [~p] --> Sent notify to drone ~p~n", [Id, MessageOrder + 1, FromId]),
                                NewDroneState = #{
                                    notify_count => lists:append(maps:get(notify_count, DroneState), [FromId]),
                                    state => pending,
                                    fallen => maps:get(fallen, DroneState)
                                },

                                CollisionTableAfterNotify = maps:put(Id,
                                                                    maps:put(notify_count, 
                                                                        lists:append(maps:get(notify_count, NewDroneState), [FromId]), 
                                                                        maps:get(Id, UpdatedCollisionTable)),
                                                                    UpdatedCollisionTable),

                                % io:format("Drone ~p [~p] --> Waiting ack messages from drones: ~p~n", [Id, MessageOrder + 2, NewNotified]),
                                handle_state(MessageOrder + 3, Id, Configuration, NewDroneState, CollisionTableAfterNotify, NewDrones, PersonalCollisions, NewNotified, ReceivedAcks, Ordering, NewToNotUpdate);
                            true ->
                                handle_state(MessageOrder + 1, Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, Ordering, NewToNotUpdate)
                            end
                        end
                    end;
            true ->
                % io:format("Drone ~p [~p] --> Received update_table message from the failed drone ~p~n", [Id, MessageOrder + 1, FromId]),
                handle_state(MessageOrder + 2, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, Ordering, ToNotUpdate)
            end;
        {ack, FromPid, FromId} ->

            % ExternalPid = maps:get(pid, maps:get(FromId, PersonalCollisions)),
            % if FromPid =/= ExternalPid ->
            %     io:format("Drone ~p --> Update_table message from older drone ~p~n", [Id, FromId]);
            % true ->
            %     ok
            % end,

            FindInPersonalCollisions = maps:get(FromId, PersonalCollisions, false),
            StoredPid = if FindInPersonalCollisions =/= false ->
                            maps:get(pid, FindInPersonalCollisions);
                        true ->
                            false
                        end,
            PidConsistency = (StoredPid =/= false) andalso (StoredPid == FromPid),
            if PidConsistency == true ->

                io:format("Drone ~p [~p] --> Received ack message from ~p~n", [Id, MessageOrder, FromId]),

                Find = sets:is_element(FromId, Notified),
                if Find == false ->
                    io:format("Drone ~p [~p] --> Received an unwaited ack from drone ~p~n", [Id, MessageOrder + 1, FromId]),
                    handle_state(MessageOrder + 2, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, Ordering, ToNotUpdate);
                true ->
                    NewReceivedAcks = [FromId | ReceivedAcks],
                    ReceivedAll = check_received_all_acks(Notified, NewReceivedAcks),

                    io:format("Drone ~p --> NewReceivedAcks: ~p, Notified: ~p --> ReceivedAll: ~p~n", [Id, NewReceivedAcks, sets:to_list(Notified), ReceivedAll]),

                    % SizeNotified = sets:size(Notified),
                    % SizeReceivedAcks = length(NewReceivedAcks),

                    if ReceivedAll == true ->
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
                                        % notify_count => sets:to_list(sets:from_list(lists:append(maps:get(notify_count, maps:get(Id, CollisionTable)), NewReceivedAcks))),
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
                        % drone_main:send_update_table(Id, NewPersonalCollisions, DroneState),
                        drone_main:agreement_loop(MessageOrder + 1, Id, Configuration, DroneState, UpdatedCollisionTable, NewDrones, NewPersonalCollisions, ToNotUpdate);
                    true ->
                        handle_state(MessageOrder + 1, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, NewReceivedAcks, Ordering, ToNotUpdate)
                    end
                end;
            true ->
                io:format("Drone ~p [~p]--> Received ack message from the failed drone ~p~n", [Id, MessageOrder, FromId]),
                handle_state(MessageOrder + 1, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, Ordering, ToNotUpdate)
            end;
        {notify, _FromPid, FromId} ->
            
            io:format("Drone ~p [~p] -> Received an unwaited notify from drone ~p while waiting acks~n", [Id, MessageOrder, FromId]),

            % FromPid ! {cancel_notify, self(), Id},    

            % io:format("Drone ~p [~p] -> Received an unwaited notify from drone ~p while waiting acks --> Sent a cancel to  drone ~p~n", [Id, MessageOrder, FromId, FromId]),

            handle_state(MessageOrder + 1, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, Ordering, ToNotUpdate)
        % {cancel_notify, _FromPid, FromId} ->
        %     FindInNotified = sets:is_element(FromId, Notified),
        %     if FindInNotified == true ->
        %         io:format("Drone ~p --> Received a cancel_notify from drone ~p~n", [Id, FromId]),
        %         NewNotify_count = lists:delete(FromId, maps:get(notify_count, DroneState)),
        %         NewDroneState = maps:put(notify_count, NewNotify_count, DroneState),
        %         NewNotified = sets:del_element(FromId, Notified),
        %         UpdatedCollisionTable = maps:put(
        %                                 Id, 
        %                                 maps:put(notify_count,
        %                                          maps:get(notify_count, NewDroneState),
        %                                          maps:get(Id, CollisionTable)),
        %                                 CollisionTable),
        %         handle_state(MessageOrder, Id, Configuration, NewDroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, NewNotified, ReceivedAcks, Ordering);
        %     true ->
        %         io:format("Drone ~p --> Received a cancel_notify from drone ~p~n that hasn't been yet notified~n", [Id, FromId]),
        %         handle_state(MessageOrder, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, Notified, ReceivedAcks, Ordering)
        %     end   
    end.

% check_notified_drones(MessageOrder, Id, Configuration, DroneState, CollisionTable, PersonalCollisions, NewDrones, Notified, ReceivedAcks, Ordering) ->
%     ReceivedAll = check_received_all_acks(Notified, ReceivedAcks),
%     if ReceivedAll == true ->
%         NewCollisionTable = maps:fold(fun(K, V, Map) -> 
%                                 FindDrone = lists:member(K, ReceivedAcks),
%                                 if FindDrone == true ->
%                                     Map;
%                                 true ->
%                                     MapOut = maps:put(K, V, Map),
%                                     MapOut
%                                 end
%                             end, #{}, CollisionTable),
%         MyEntryCollisionTable = #{
%                         notify_count => sets:to_list(sets:from_list(lists:append(maps:get(notify_count, maps:get(Id, CollisionTable)), ReceivedAcks))),
%                         state => pending,
%                         collisions => sets:subtract(maps:get(collisions, maps:get(Id, CollisionTable)), sets:from_list(ReceivedAcks))        
%         },
%         UpdatedCollisionTable = maps:put(Id, MyEntryCollisionTable, NewCollisionTable),
%         NewPersonalCollisions = maps:fold(fun(K, V, Map) ->
%                                     FindDrone = lists:member(K, ReceivedAcks),
%                                     if FindDrone == true ->
%                                         Map;
%                                     true ->
%                                         MapOut = maps:put(K, V, Map),
%                                         MapOut
%                                     end
%                                 end, #{}, PersonalCollisions),
%         NewDroneState = #{
%             state => pending,
%             fallen => maps:get(fallen, DroneState),
%             notify_count => maps:get(notify_count, maps:get(Id, UpdatedCollisionTable))
%         },
%         send_update_table_with_collision_table(Id, NewPersonalCollisions, UpdatedCollisionTable, DroneState),
%         % drone_main:send_update_table(Id, NewPersonalCollisions, NewDroneState),
%         drone_main:agreement_loop(MessageOrder, Id, Configuration, NewDroneState, UpdatedCollisionTable, NewDrones, NewPersonalCollisions);
%     true ->
%         % If the notified_count is less the NOT_THRESHOLD, by scanning the ordering must be seen if other notify can be sent
%         % The order must be scanned from left to right, but avoiding drones already in notified and 
%         {NewNotify, Added} = lists:foldl(fun(T, {TmpNotified, Added}) ->
%                             AlreadyNotified = sets:is_element(T, Notified),
%                             StillNotAcked = sets:subtract(Notified, sets:from_list(ReceivedAcks)),
%                             SizeNotified = sets:size(Notified),
%                             Notify_Threshold = maps:get(notify_threshold, Configuration),
%                             if AlreadyNotified == false, T =/= Id, SizeNotified < Notify_Threshold, Added == false ->
%                                 Send = check_send_notify_on_still_not_acked(T, CollisionTable, StillNotAcked),
%                                 if Send == true ->
%                                     Out = [T | TmpNotified],
%                                     {Out, true};
%                                 true ->
%                                     {TmpNotified, Added}
%                                 end;
%                             true ->
%                                 {TmpNotified, Added}
%                             end
%             end, {[], false}, Ordering),
%         NewNotified =   if Added == true ->
%                             lists:foreach(fun(T) -> 
%                                     Pid_T = maps:get(pid, maps:get(T, PersonalCollisions)),
%                                     Pid_T ! {notify, self(), Id}
%                                 end, NewNotify),
%                             lists:append(Notified, NewNotify);
%                         true ->
%                             Notified
%                         end,
%         NewDroneState = #{
%                     state => maps:get(state, DroneState),
%                     fallen => maps:get(fallen, DroneState),
%                     notify_count => lists:append(maps:get(notify_count, DroneState), NewNotify)
%         },
%         UpdatedCollisionTable = maps:put(
%                                         Id, 
%                                         maps:put(notify_count,
%                                                  maps:get(notify_count, NewDroneState),
%                                                  maps:get(Id, CollisionTable)),
%                                         CollisionTable),
                        
%         handle_state(MessageOrder, Id, Configuration, NewDroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, NewNotified, ReceivedAcks, Ordering)
%     end.

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



check_send_notify(FromId, CollisionTable, Notified, Configuration) ->
    Collisions = maps:get(collisions, maps:get(FromId, CollisionTable)),
    SizeNotified = sets:size(Notified),
    Dependencies = sets:size(sets:intersection(Collisions, Notified)),
    Notify_Threshold = maps:get(notify_threshold, Configuration),
    if SizeNotified < Notify_Threshold ->
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

            CollidingDrones = maps:keys(PersonalCollisions), 
            State = maps:get(state, DroneState),
            Notify_count = maps:get(notify_count, DroneState),
            External_Pid ! {update_table, self(), Id, add, CollidingDrones, State, Notify_count},

            io:format("Drone ~p --> Sent update_table message to drone ~p~n", [Id, External_Id]);
        true -> 
            ok
        end    
    end, CollisionTable).


% remove_from_all(RemovedId, CollisionTable) ->
%     NewCollisionTable = maps:fold(fun(K, V, Map) ->
%                         Collisions = maps:get(collisions, V),
%                         NewCollisions = sets:del_element(RemovedId, Collisions),
%                         OutMap = maps:put(K, maps:put(collisions, NewCollisions, V), Map),
%                         OutMap
%         end, #{}, CollisionTable),
%     NewCollisionTable.

% check_send_new_notify(Id, Configuration, DroneState, Notified, ReceivedAcks, CollisionTable, PersonalCollisions, Ordering) ->
%     % If the notified_count is less the NOT_THRESHOLD, by scanning the ordering must be seen if other notify can be sent
%     % The order must be scanned from left to right, but avoiding drones already in notified and 
%     {NewNotify, Added} = lists:foldl(fun(T, {TmpNotified, Added}) ->
%                         AlreadyNotified = sets:is_element(T, Notified),
%                         %% I need also to check that T is present in the CollisionTable, since it could also be the fallen drone
%                         FindInCollisionTable = maps:get(T, CollisionTable, not_exists),
%                         StillNotAcked = sets:subtract(Notified, sets:from_list(ReceivedAcks)),
%                         SizeNotified = sets:size(Notified),
%                         Notify_Threshold = maps:get(notify_threshold, Configuration),
%                         if FindInCollisionTable =/= not_exists, AlreadyNotified == false, T =/= Id, SizeNotified < Notify_Threshold, Added == false ->
%                             Send = check_send_notify(T, CollisionTable, Notified, Configuration),
%                             if Send == true ->
%                                 Out = [T | TmpNotified],
%                                 {Out, true};
%                             true ->
%                                 {TmpNotified, Added}
%                             end;
%                         true ->
%                             {TmpNotified, Added}
%                         end
%         end, {[], false}, Ordering),
%     NewNotified =   if Added == true ->
%                         lists:foreach(fun(T) -> 
%                                 Pid_T = maps:get(pid, maps:get(T, PersonalCollisions)),
%                                 Pid_T ! {notify, self(), Id}
%                             end, NewNotify),
%                         sets:union(Notified, sets:from_list(NewNotify));
%                     true ->
%                         Notified
%                     end,
%     NewDroneState = #{
%                 state => maps:get(state, DroneState),
%                 fallen => maps:get(fallen, DroneState),
%                 notify_count => lists:append(maps:get(notify_count, DroneState), NewNotify)
%     },
%     UpdatedCollisionTable = maps:put(
%                                     Id, 
%                                     maps:put(notify_count,
%                                                 maps:get(notify_count, NewDroneState),
%                                                 maps:get(Id, CollisionTable)),
%                                     CollisionTable),
%     {NewDroneState, UpdatedCollisionTable, NewNotified}.

go_to_agreement(MessageOrder, Id, Configuration, DroneState, CollisionTable, PersonalCollisions, NewDrones, ReceivedAcks, ToNotUpdate) ->
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
        % drone_main:send_update_table(Id, NewPersonalCollisions, NewDroneState),
        drone_main:agreement_loop(MessageOrder, Id, Configuration, NewDroneState, UpdatedCollisionTable, NewDrones, NewPersonalCollisions, ToNotUpdate).
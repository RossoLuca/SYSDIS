-module(on_waiting_notify).

-export([handle_state/9, spawnFlightProcess/4]).


handle_state(MessageOrder, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked) ->
    WaitingFrom = maps:get(collisions, maps:get(Id, CollisionTable)),
    SizeWaitingFrom = sets:size(WaitingFrom),
    SizeToBeAcked = sets:size(ToBeAcked),
    if SizeWaitingFrom == SizeToBeAcked ->
        CurrentPosition = drone_main:get_route_start(Configuration),
        NewCollisionTable = applyBufferedUpdate(Id, UpdateTableBuffer, CollisionTable, PersonalCollisions),
        {NewDroneState, UpdatedCollisionTable} = change_state(Id, flying, DroneState, NewCollisionTable),
        FlyingProcessPid = spawnFlightProcess(Id, Configuration, PersonalCollisions, sets:to_list(ToBeAcked)),
        UpdatedToBeAcked = lists:map(fun(T) -> #{id => T, received => pre} end, sets:to_list(ToBeAcked)),
        AlreadyAcked = [],
        RestConnection = http_utils:createConnection(),
        io:format("Drone ~p [~p] --> Started to fly --- UpdatedToBeAcked: ~p~n", [Id, MessageOrder, UpdatedToBeAcked]),
        flying:handle_state(MessageOrder + 1, Id, Configuration, NewDroneState, CurrentPosition, UpdatedCollisionTable, NewDrones, PersonalCollisions, UpdatedToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked);
    true ->
        % io:format("Drone ~p [~p] --> WAITING NOTIFY MESSAGES FROM ~p~n", [Id, MessageOrder, WaitingFrom]),
        receive
            {sync_hello, FromPid, FromId, FromMainPid, FromRoute} ->
                %% When a drone receive a sync_hello message while it's waiting for some notify messages it does the collision computation,
                %% updates its PersonalCollisions (in case of collision), returns the sync_result to other drone, but continue to remain in the
                %% same state

                io:format("Drone ~p [~p] --> Received sync_hello message from drone ~p to compute collision computation~n", [Id, MessageOrder + 1, FromMainPid]),
                MyStart = drone_main:get_route_start(Configuration),
                MyEnd = maps:get(route_end, Configuration),
                DroneSize = maps:get(drone_size, Configuration),

                AwaitingNotify = sets:is_element(FromId, ToBeAcked),
                NewToBeAcked =  if AwaitingNotify == true ->
                                    sets:del_element(FromId, ToBeAcked);
                                true -> 
                                    ToBeAcked
                                end,

                {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
                NewPersonalCollisions = drone_main:update_personal_collisions(Collision_response, FromMainPid, FromId, Collision_points, PersonalCollisions),
                
                % AlreadyInCollisionTable = maps:get(FromId, CollisionTable, false),
                % UpdatedCollisionTable = if AlreadyInCollisionTable =/= false ->
                %                             remove_from_all(FromId, maps:remove(FromId, CollisionTable));
                %                         true ->
                %                             CollisionTable
                %                         end,

                FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},
                handle_state(MessageOrder + 2, Id, Configuration, DroneState, CollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, UpdateTableBuffer, NewToBeAcked);

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
                % io:format("Drone ~p [~p] --> While waiting notify, FindInPersonalCollisions: ~p, StoredPid: ~p, FromPid: ~p, PidConsistency: ~p~n", [Id, MessageOrder + 1, FindInPersonalCollisions, StoredPid, FromPid, PidConsistency]),
                if PidConsistency == true ->
                        %% When a drone receive an update_table message while it's waiting for some notify messages, first of all must be seen
                        %% if this update_table message come from a drone that is alerady present in the collision table or is from 
                        %% a new drone
                        Find = maps:get(FromId, CollisionTable, not_exists),
                        if Find =/= not_exists ->
                            %% In this case the drone has already an entry about FromId in its CollisionTable
                            %% The only possible reason of this message from FromId is because the number of collisions of FromPid
                            %% has increased
                            %% This means that anyway the drone has fewer collisions than FromId and so the drone, after updating its CollisionTable,
                            %% remains in the same state
                            Collisions = #{
                                collisions => sets:from_list(FromCollidingDrones),
                                state => FromState,
                                notify_count => FromNotify_count
                            },
                            NewCollisionTable = maps:put(FromId, Collisions, CollisionTable),

                            %% When I received an update_table message from a drone that is already in my table
                            %% actually means that this drone has fallen before, so it's important to send back 
                            %% the update_table message to him, such a way then it can send a notify to us 
                            CollidingDrones = maps:keys(PersonalCollisions),
                            State = maps:get(state, DroneState),
                            Notify_count = maps:get(notify_count, DroneState),
                            FromPid ! {update_table, self(), Id, add, CollidingDrones, State, Notify_count},

                            % io:format("Drone ~p --> Sent update_table~n", [Id]),
                                        
                            handle_state(MessageOrder + 2, Id, Configuration, DroneState, NewCollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked);
                        true ->
                            %% In this case the drone hasn't an entry about FromId in its CollisionTable
                            %% Since the drone has already taken the priority from the other drones that collide with him,
                            %% it's possible that if FromId compute now the agreement_order, the priority move from the current drone to
                            %% FromId
                            %% So, in order to avoid this, the current drone can buffer this message and apply it to its CollisionTable
                            %% (and also sending the update_table message to FromId) only when it has received all the notify messages and start to fly
                            %% such a way that when the FromId drone compute its agreement_order it will give surely priority to the current drone 
                            %% Obviously, the applying of an update_table message means that not only a new entry is inserted in my CollisionTable,
                            %% but also an update_table message will be sent to the drones that have sent them
                            UpdateTableMessage = {FromId, FromCollidingDrones, FromState, FromNotify_count},
                            NewBuffer = [UpdateTableMessage | UpdateTableBuffer],
                            handle_state(MessageOrder + 2, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, NewBuffer, ToBeAcked)
                        end;
                true ->
                    % io:format("Drone ~p [~p] --> Received update_table message from the failed drone ~p~n", [Id, MessageOrder + 2, FromId]),
                    handle_state(MessageOrder + 3, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
                end;
            {notify, FromPid, FromId} ->

                % ExternalPid = maps:get(pid, maps:get(FromId, PersonalCollisions)),
                % if FromPid =/= ExternalPid ->
                %     io:format("Drone ~p --> Notify message from older drone ~p~n", [Id, FromId]);
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

                        WaitingFrom = maps:get(collisions, maps:get(Id, CollisionTable)),
                        Find = sets:is_element(FromId, WaitingFrom),
                        if Find == true ->
                            AlreadyReceived = sets:is_element(FromId, ToBeAcked),
                            if AlreadyReceived == false ->
                                io:format("Drone ~p [~p] --> Received notify message from drone ~p~n", [Id, MessageOrder + 1, FromId]),
                                NewToBeAcked = sets:add_element(FromId, ToBeAcked),
                                handle_state(MessageOrder + 2, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, NewToBeAcked);
                            true ->
                                io:format("Drone ~p [~p] --> Received a duplicated notify message from drone ~p~n", [Id, MessageOrder + 1, FromId]),
                                handle_state(MessageOrder + 2, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
                            end;
                        true ->
                            io:format("Drone ~p [~p] --> Received notify message from drone ~p that is unecessary~n", [Id, MessageOrder + 1, FromId]),
                            handle_state(MessageOrder + 2, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
                        end;
                true ->
                    % io:format("Drone ~p [~p] --> Received notify message from the failed drone ~p~n", [Id, MessageOrder + 1, FromId]),
                    handle_state(MessageOrder + 2, Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
                end
            end
    end.


applyBufferedUpdate(Id, UpdateTableBuffer, CollisionTable, PersonalCollisions) ->
    NewCollisionTable = lists:foldl(fun({FromId, FromCollidingDrones, FromState, FromNotify_count}, AccIn) ->
            CollidingDrones = maps:keys(PersonalCollisions),
            State = maps:get(state, maps:get(Id, CollisionTable)),
            Notify_count = maps:get(notify_count, maps:get(Id, CollisionTable)),
            FromPid = maps:get(pid, maps:get(FromId, PersonalCollisions)),
            FromPid ! {update_table, self(), Id, add, CollidingDrones, State, Notify_count},

            Collisions = #{
                collisions => sets:from_list(FromCollidingDrones),
                state => FromState,
                notify_count => FromNotify_count
            },
            AccOut = maps:put(FromId, Collisions, AccIn),
            AccOut

        end, CollisionTable, UpdateTableBuffer),
    NewCollisionTable.


spawnFlightProcess(Id, Configuration, PersonalCollisions, ToBeAcked) ->
    Height = maps:get(height, Configuration),
    Start = drone_main:get_route_start(Configuration),
    End = maps:get(route_end, Configuration),
    Velocity = maps:get(velocity, Configuration),
    DroneSize = maps:get(drone_size, Configuration),
    PointsToBeAcked = lists:map(fun(T) -> 
                                        P = maps:get(Id, maps:get(points, maps:get(T, PersonalCollisions))),
                                        P
                        end, ToBeAcked),
    % io:format("Drone ~p --> PointsToBeAcked: ~p~n", [Id, PointsToBeAcked]),
    Pid = spawn_link(flight, init, [Id, Height, Start, End, Velocity, DroneSize, PointsToBeAcked, self()]),
    % io:format("Drone ~p --> Flight process spawned with pid: ~p~n", [Id, Pid]),
    Pid.

change_state(Id, NewState, DroneState, CollisionTable) ->
    NewDroneState = maps:put(state, NewState, DroneState),
    NewCollisionTable = maps:put(Id, maps:put(state, NewState, maps:get(Id, CollisionTable)), CollisionTable),
    {NewDroneState, NewCollisionTable}.
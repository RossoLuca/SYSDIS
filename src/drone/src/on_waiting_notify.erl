-module(on_waiting_notify).

-export([handle_state/8, spawnFlightProcess/4]).


handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked) ->
    WaitingFrom = maps:get(collisions, maps:get(Id, CollisionTable)),
    SizeWaitingFrom = sets:size(WaitingFrom),
    SizeToBeAcked = sets:size(ToBeAcked),
    if SizeWaitingFrom == SizeToBeAcked ->
        CurrentPosition = utils:get_route_start(Configuration),
        NewCollisionTable = applyBufferedUpdate(Id, UpdateTableBuffer, CollisionTable, PersonalCollisions),
        {NewDroneState, UpdatedCollisionTable} = utils:change_state(Id, flying, DroneState, NewCollisionTable),
        FlyingProcessPid = spawnFlightProcess(Id, Configuration, PersonalCollisions, sets:to_list(ToBeAcked)),
        UpdatedToBeAcked = lists:map(fun(T) -> #{id => T, received => pre} end, sets:to_list(ToBeAcked)),
        AlreadyAcked = [],
        RestConnection = http_utils:createConnection(maps:get(rest_endpoint, Configuration)),
        logging:log(Id, "Started to fly", []),
        flying:handle_state(Id, Configuration, NewDroneState, CurrentPosition, UpdatedCollisionTable, NewDrones, PersonalCollisions, UpdatedToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked);
    true ->
        receive
            {sync_hello, FromPid, FromId, FromMainPid, FromRoute} ->
                %% When a drone receive a sync_hello message while it's waiting for some notify messages it does the collision computation,
                %% updates its PersonalCollisions (in case of collision), returns the sync_result to other drone, but continue to remain in the
                %% same state

                logging:log(Id, "Received sync_hello message from drone ~p to compute collision computation", [FromMainPid]),
                MyStart = utils:get_route_start(Configuration),
                MyEnd = maps:get(route_end, Configuration),
                DroneSize = maps:get(drone_size, Configuration),

                AwaitingNotify = sets:is_element(FromId, ToBeAcked),
                NewToBeAcked =  if AwaitingNotify == true ->
                                    sets:del_element(FromId, ToBeAcked);
                                true -> 
                                    ToBeAcked
                                end,

                {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
                NewPersonalCollisions = utils:update_personal_collisions(Collision_response, FromMainPid, FromId, Collision_points, PersonalCollisions),

                FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},
                handle_state(Id, Configuration, DroneState, CollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, UpdateTableBuffer, NewToBeAcked);

            {update_table, FromPid, FromId, _Action, FromCollidingDrones, FromState, FromNotify_count} ->

                IsCorrectPid = utils:check_drone_pid(FromPid, FromId, PersonalCollisions),
                
                if IsCorrectPid == true ->
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
                            NewCollisionTable = utils:update_entry_in_collision_table(FromId, CollisionTable, FromCollidingDrones, FromState, FromNotify_count),

                            %% When I received an update_table message from a drone that is already in my table
                            %% actually means that this drone has fallen before, so it's important to send back 
                            %% the update_table message to him, such a way then it can send a notify to us 
                            utils:send_update_table_add(Id, FromPid, PersonalCollisions, DroneState),
                                        
                            handle_state(Id, Configuration, DroneState, NewCollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked);
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
                            handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, NewBuffer, ToBeAcked)
                        end;
                true ->
                    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
                end;
            {notify, FromPid, FromId} ->

                IsCorrectPid = utils:check_drone_pid(FromPid, FromId, PersonalCollisions),

                if IsCorrectPid == true ->

                        WaitingFrom = maps:get(collisions, maps:get(Id, CollisionTable)),
                        Find = sets:is_element(FromId, WaitingFrom),
                        if Find == true ->
                            AlreadyReceived = sets:is_element(FromId, ToBeAcked),
                            if AlreadyReceived == false ->
                                logging:log(Id, "Received notify message from drone ~p", [FromId]),
                                NewToBeAcked = sets:add_element(FromId, ToBeAcked),
                                handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, NewToBeAcked);
                            true ->
                                logging:log(Id, "Received a duplicated notify message from drone ~p", [FromId]),
                                handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
                            end;
                        true ->
                            logging:log(Id, "Received notify message from drone ~p that is unecessary", [FromId]),
                            handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
                        end;
                true ->
                    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
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

            AccOut = utils:update_entry_in_collision_table(FromId, AccIn, FromCollidingDrones, FromState, FromNotify_count),
            AccOut

        end, CollisionTable, UpdateTableBuffer),
    NewCollisionTable.


spawnFlightProcess(Id, Configuration, PersonalCollisions, ToBeAcked) ->
    Height = maps:get(height, Configuration),
    Start = utils:get_route_start(Configuration),
    End = maps:get(route_end, Configuration),
    Velocity = maps:get(velocity, Configuration),
    DroneSize = maps:get(drone_size, Configuration),
    PointsToBeAcked = lists:map(fun(T) -> 
                                        P = maps:get(Id, maps:get(points, maps:get(T, PersonalCollisions))),
                                        P
                        end, ToBeAcked),
    Pid = spawn_link(flight, init, [Id, Height, Start, End, Velocity, DroneSize, PointsToBeAcked, self()]),
    Pid.
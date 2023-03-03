-module(on_waiting_notify).

-export([handle_state/8, spawnFlightProcess/4]).


handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked) ->
    WaitingFrom = maps:get(collisions, maps:get(Id, CollisionTable)),
    SizeWaitingFrom = length(WaitingFrom),
    SizeToBeAcked = length(ToBeAcked),
    if SizeWaitingFrom == SizeToBeAcked ->
        CurrentPosition = maps:get(route_start, Configuration),
        NewCollisionTable = applyBufferedUpdate(Id, UpdateTableBuffer, CollisionTable, PersonalCollisions),
        {NewDroneState, UpdatedCollisionTable} = change_state(Id, flying, DroneState, NewCollisionTable),
        FlyingProcessPid = spawnFlightProcess(Id, Configuration, PersonalCollisions, ToBeAcked),
        UpdatedToBeAcked = lists:map(fun(T) -> #{id => T, received => pre} end, ToBeAcked),
        RestConnection = http_utils:createConnection(),
        io:format("Drone ~p --> Started to fly --- UpdatedToBeAcked: ~p~n", [Id, UpdatedToBeAcked]),
        flying:handle_state(Id, Configuration, NewDroneState, CurrentPosition, UpdatedCollisionTable, NewDrones, PersonalCollisions, UpdatedToBeAcked, FlyingProcessPid, RestConnection);
    true ->
        io:format("Drone ~p --> WAITING NOTIFY MESSAGES FROM ~p~n", [Id, WaitingFrom]),
        receive
            {sync_hello, FromPid, FromId, FromMainPid, FromRoute} ->
                %% When a drone receive a sync_hello message while it's waiting for some notify messages it does the collision computation,
                %% updates its PersonalCollisions (in case of collision), returns the sync_result to other drone, but continue to remain in the
                %% same state

                io:format("Drone ~p --> Received sync_hello message from drone ~p~n to compute collision computation", [Id, FromMainPid]),
                MyStart = maps:get(route_start, Configuration),
                MyEnd = maps:get(route_end, Configuration),
                DroneSize = maps:get(drone_size, Configuration),

                {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
                NewPersonalCollisions = drone_main:update_personal_collisions(Collision_response, FromMainPid, FromId, Collision_points, PersonalCollisions),
                
                FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},
                handle_state(Id, Configuration, DroneState, CollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, UpdateTableBuffer, ToBeAcked);

            {update_table, _FromPid, FromId, _Action, FromCollidingDrones, FromState, FromAck_count} ->
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
                        collisions => FromCollidingDrones,
                        state => FromState,
                        ack_count => FromAck_count
                    },
                    NewCollisionTable = maps:put(FromId, Collisions, CollisionTable),
                    handle_state(Id, Configuration, DroneState, NewCollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked);
                true ->
                    %% In this case the drone hasn't an entry about FromIt in its CollisionTable
                    %% Since the drone has already taken the priority from the other drones that collide with him,
                    %% it's possible that if FromId compute now the agreement_order, the priority move from the current drone to
                    %% FromId
                    %% So, in order to avoid this, the current drone can buffer this message and apply it to its CollisionTable
                    %% (and also sending the update_table message to FromId) only when it has received all the notify messages and start to fly
                    %% such a way that when the FromId drone compute its agreement_order it will give surely priority to the current drone 
                    %% Obviously, the applying of an update_table message means that not only a new entry is inserted in my CollisionTable,
                    %% but also an update_table message will be sent to the drones that have sent them
                    UpdateTableMessage = {FromId, FromCollidingDrones, FromState, FromAck_count},
                    NewBuffer = [UpdateTableMessage | UpdateTableBuffer],
                    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, NewBuffer, ToBeAcked)
                end;
            {notify, _FromPid, FromId} ->
                WaitingFrom = maps:get(collisions, maps:get(Id, CollisionTable)),
                Find = lists:member(FromId, WaitingFrom),
                if Find == true ->
                    io:format("Drone ~p --> Received notify message from drone ~p~n", [Id, FromId]),
                    NewToBeAcked = [FromId | ToBeAcked],
                    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, NewToBeAcked);
                true ->
                    io:format("Drone ~p --> Received notify message from drone ~p~n that is unecessary~n", [Id, FromId]),
                    handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked)
                end
            end
    end.


applyBufferedUpdate(Id, UpdateTableBuffer, CollisionTable, PersonalCollisions) ->
    NewCollisionTable = lists:foldl(fun({FromId, FromCollidingDrones, FromState, FromAck_count}, AccIn) ->
            CollidingDrones = maps:keys(PersonalCollisions),
            State = maps:get(state, maps:get(Id, PersonalCollisions)),
            Ack_count = maps:get(ack_count, maps:get(Id, PersonalCollisions)),
            FromPid = maps:get(pid, maps:get(FromId, PersonalCollisions)),
            FromPid ! {update_table, self(), Id, add, CollidingDrones, State, Ack_count},

            Collisions = #{
                collisions => FromCollidingDrones,
                state => FromState,
                ack_count => FromAck_count
            },
            AccOut = maps:put(FromId, Collisions, AccIn),
            AccOut

        end, CollisionTable, UpdateTableBuffer),
    NewCollisionTable.


spawnFlightProcess(Id, Configuration, PersonalCollisions, ToBeAcked) ->
    Start = maps:get(route_start, Configuration),
    End = maps:get(route_end, Configuration),
    Velocity = maps:get(velocity, Configuration),
    DroneSize = maps:get(drone_size, Configuration),
    PointsToBeAcked = lists:map(fun(T) -> 
                                        P = maps:get(Id, maps:get(points, maps:get(T, PersonalCollisions))),
                                        P
                        end, ToBeAcked),
    io:format("Drone ~p --> PointsToBeAcked: ~p~n", [Id, PointsToBeAcked]),
    Pid = spawn_link(flight, init, [Id, Start, End, Velocity, DroneSize, PointsToBeAcked, self()]),
    io:format("Drone ~p --> Flight process spawned with pid: ~p~n", [Id, Pid]),
    Pid.

change_state(Id, NewState, DroneState, CollisionTable) ->
    NewDroneState = maps:put(state, NewState, DroneState),
    NewCollisionTable = maps:put(Id, maps:put(state, NewState, maps:get(Id, CollisionTable)), CollisionTable),
    {NewDroneState, NewCollisionTable}.
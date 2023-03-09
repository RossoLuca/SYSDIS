-module(flying).

-export([handle_state/11]).


handle_state(Id, Configuration, DroneState, CurrentPosition, CollisionTable, NewDrones, PersonalCollisions, ToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked) -> 
    receive
        {sync_hello, FromPid, FromId, FromMainPid, FromRoute} ->
                %% When a drone receive a sync_hello message while it's flying it does the collision computation,
                %% updates its PersonalCollisions (in case of collision), returns the sync_result to the other drone, but continue to remain in the
                %% same state
               
                MyStart = utils:get_route_start(Configuration),
                MyEnd = maps:get(route_end, Configuration),
                DroneSize = maps:get(drone_size, Configuration),

                logging:log(Id, "Received sync_hello message from drone ~p to compute collision computation", [FromMainPid]),
                {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),

                NewPersonalCollisions = utils:update_personal_collisions(Collision_response, FromMainPid, FromId, Collision_points, PersonalCollisions),


                FindInToBeAcked = fun(Entry) ->
                                        Drone = maps:get(id, Entry),
                                        if Drone == FromId ->
                                            true;
                                        true ->
                                            false
                                        end
                                end,
                NotYetAcked = lists:search(FindInToBeAcked, ToBeAcked),
                NewToBeAcked =  if NotYetAcked == false ->
                                        ToBeAcked;
                                true ->
                                    lists:dropwhile(FindInToBeAcked, ToBeAcked)
                                end,

                AlreadyInCollisionTable = maps:get(FromId, CollisionTable, false),
                UpdatedCollisionTable = if AlreadyInCollisionTable =/= false ->
                                            maps:remove(FromId, CollisionTable);
                                        true ->
                                            CollisionTable
                                        end,

                FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},
                handle_state(Id, Configuration, DroneState, CurrentPosition, UpdatedCollisionTable, maps:put(FromId, FromMainPid, NewDrones), NewPersonalCollisions, NewToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked);

        {update_table, FromPid, FromId, _Action, FromCollidingDrones, FromState, FromNotify_count} ->
                logging:log(Id, "Received update_table message from drone ~p", [FromId]),

                IsCorrectPid = utils:check_drone_pid(FromPid, FromId, PersonalCollisions),
                
                if IsCorrectPid == true ->

                    Find = maps:get(FromId, CollisionTable, not_exists),
                    if Find =/= not_exists ->
                        %% In this case the drone has already an entry about FromId in its CollisionTable
                        %% Actually, this case could not happen
                        %%
                        %% Actually, this case can happen: For example, drone 0 is flying, then drone 1 join the network and after synchronization send its first update_table message to drone 0
                        %% So, drone 0 insert the row about 1 in its CollisionTable. Then, drone 2 joins the network and after synchronization send update_table message to drone 1
                        %% Since, drone is currently waiting the ack from drone 0, after it applies the update_table message of drone 2 to its table, 
                        %% then it propagates its updated entry also to drone 0. Since drone 0 is still flying, when it receives the update_table message from drone 1, it sees that it already
                        %% has an entry about 1 in its CollisionTable, so the drone ends in this if case
                        %%
                        %%
                        %% (theoretically it should't receive in this case an update_table message from someone that it already knows because the condition for the
                        %% current drone to start flying is to receive a notify message from all the other drones colliding with him
                        %% --> so, all these other drones should send update_table messages only when they have received all the acks; moreover these update_table messages
                        %% shouldn't be sent to drones for which they have received an ack before)
                        %% With Fault tolerance this can happen!
                        logging:log(Id, "Received update_table message from drone ~p while flying", [FromId]),

                        Start = utils:get_route_start(Configuration),
                        End = maps:get(route_end, Configuration),
                        DroneSize = maps:get(drone_size, Configuration),
                        IntersectionPoint = maps:get(Id, maps:get(points, maps:get(FromId, PersonalCollisions))), 
                        Passed = check_intersection_passed(Start, End, CurrentPosition, IntersectionPoint, DroneSize),

                        if Passed == true ->
                            utils:send_update_table_remove(Id, FromPid);
                        true ->
                            utils:send_update_table_add(Id, FromPid, PersonalCollisions, DroneState)
                        end,

                        NewCollisionTable = utils:update_entry_in_collision_table(FromId, CollisionTable, FromCollidingDrones, FromState, FromNotify_count),

                        handle_state(Id, Configuration, DroneState, CurrentPosition, NewCollisionTable, NewDrones, PersonalCollisions, ToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked);
                    true ->
                        %% In this case the drone hasn't an entry about FromId in its CollisionTable
                        %% Since the drone is flying, it must check if the intersection point with FromId
                        %% has already been passed
                        %% Cannot be used compute_collision function to check this because it need also the route (start, end) of other drone which
                        %% we don't have
                        Start = utils:get_route_start(Configuration),
                        End = maps:get(route_end, Configuration),
                        DroneSize = maps:get(drone_size, Configuration),
                        IntersectionPoint = maps:get(Id, maps:get(points, maps:get(FromId, PersonalCollisions))), 
                        Passed = check_intersection_passed(Start, End, CurrentPosition, IntersectionPoint, DroneSize),
                        
                        if Passed == true ->
                            %% If the intersection has already been passed, then the current drone must send an update_table message to the other
                            %% but informing it that there's no more collision
                            logging:log(Id, "Received an update_table message from drone ~p but there's no more collision", [FromId]),
                            utils:send_update_table_remove(Id, FromPid),
                        
                            handle_state(Id, Configuration, DroneState, CurrentPosition, CollisionTable, NewDrones, PersonalCollisions, ToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked);
                        true ->
                            %% If the intersection hasn't been passed, the the current drone must send a normal update_table message to the other
                            utils:send_update_table_add(Id, FromPid, PersonalCollisions, DroneState),

                            %% At the end, the CollisionTable must be updated with the new entry and the other drone
                            %% must be added to my collision list in my entry in the CollisionTable
                            MyCollisions = #{
                                collisions => sets:add_element(FromId, maps:get(collisions, maps:get(Id, CollisionTable))),
                                state => maps:get(state, maps:get(Id, CollisionTable)),
                                notify_count => maps:get(notify_count, maps:get(Id, CollisionTable))
                            },
                            NewCollisionTable = utils:update_entry_in_collision_table(FromId, CollisionTable, FromCollidingDrones, FromState, FromNotify_count),
                            UpdatedCollisionTable = maps:put(Id, MyCollisions, NewCollisionTable),
                            
                            handle_state(Id, Configuration, DroneState, CurrentPosition, UpdatedCollisionTable, NewDrones, PersonalCollisions, ToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked)
                        end
                    end;
                true ->
                    logging:log(Id, "Received update_table message from the failed drone ~p", [FromId]),
                    handle_state(Id, Configuration, DroneState, CurrentPosition, CollisionTable, NewDrones, PersonalCollisions, ToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked)
                end;
        {notify, FromPid, FromId} ->

            IsCorrectPid = utils:check_drone_pid(FromPid, FromId, PersonalCollisions),
            
            if IsCorrectPid == true ->

                %% Must be checked that the notify message comes from someone included in my collisions
                MyCollisions = maps:get(collisions, maps:get(Id, CollisionTable)),
                Find = sets:is_element(FromId, MyCollisions),
                if Find == false ->
                    handle_state(Id, Configuration, DroneState, CurrentPosition, CollisionTable, NewDrones, PersonalCollisions, ToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked);
                true ->
                    Start = utils:get_route_start(Configuration),
                    End = maps:get(route_end, Configuration),
                    DroneSize = maps:get(drone_size, Configuration),
                    IntersectionPoint = maps:get(Id, maps:get(points, maps:get(FromId, PersonalCollisions))), 
                    Passed = check_intersection_passed(Start, End, CurrentPosition, IntersectionPoint, DroneSize),
                    logging:log(Id, "Received notify message from drone ~p while flying", [FromId]),
                    if Passed == true ->
                        FromPid ! {ack, self(), Id},
                        logging:log(Id, "Sent ack message to drone ~p", [FromId]),
                        NewPersonalCollisions = maps:remove(FromId, PersonalCollisions),
                        NewCollisionTable = maps:remove(FromId, CollisionTable),
                        NewMyCollisions = sets:del_element(FromId, MyCollisions),
                        UpdatedCollisionTable = maps:put(Id, NewMyCollisions, NewCollisionTable),
                        handle_state(Id, Configuration, DroneState, CurrentPosition, UpdatedCollisionTable, NewDrones, NewPersonalCollisions, ToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked);
                    true ->
                        NewToBeAcked = [#{id => FromId, received => post} | ToBeAcked],

                        handle_state(Id, Configuration, DroneState, CurrentPosition, CollisionTable, NewDrones, PersonalCollisions, NewToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked)
                    end
                end;
            true ->
                logging:log(Id, "Received notify message from the failed drone ~p", [FromId]),
                handle_state(Id, Configuration, DroneState, CurrentPosition, CollisionTable, NewDrones, PersonalCollisions, ToBeAcked, FlyingProcessPid, RestConnection, AlreadyAcked)
            end;
        {update_position, FlyingProcessPid, Id, New_x, New_y, Real_x, Real_y, Type} ->
            logging:log(Id, "Arrived at point (~p, ~p) of type ~p", [Real_x, Real_y, Type]),
            
            if Type == taking_off ->
                logging:log(Id, "Reached height of fly", []);
            Type == landing ->
                logging:log(Id, "Landed on the ground", []);
            true -> ok
            end,

            {Start_x, Start_y} = maps:get(route_start, Configuration),
            {End_x, End_y} = maps:get(route_end, Configuration),
            CodedPid = maps:get(coded_pid, Configuration),
            Fallen = maps:get(fallen, DroneState),
            
            DeliveryState = if Real_x == End_x, Real_y == End_y -> completed; true -> flying end, 

            Resource = "/delivery/",

            %% The update of the current position must be done here only when the state is flying
            %% Otherwise the update of the state to completed must be done when all the needed messages are
            %% alreby been sent
            if DeliveryState == flying ->
                UpdatedDelivery = #{
                    id => Id,
                    pid => CodedPid,
                    state => DeliveryState,
                    start_x => Start_x,
                    start_y => Start_y,
                    current_x => Real_x,
                    current_y => Real_y,
                    end_x => End_x, 
                    end_y => End_y,
                    fallen => Fallen
                },
                %% TODO: Add management of the response
                _Response = http_utils:doPost(RestConnection, Resource, UpdatedDelivery);
            true ->
                ok
            end,

            NewToBeAcked = lists:foldl(fun(T, Acc) ->
                    WasAlreadyAcked = lists:member(maps:get(id, T), AlreadyAcked),
                    if WasAlreadyAcked == true ->
                        Acc;
                    true ->
                        AccOut = [T | Acc],
                        AccOut
                    end
                end, [], ToBeAcked),

            Pre = lists:filter(fun(Entry) -> 
                                Received = maps:get(received, Entry),
                                if Received == pre ->
                                    true;
                                true ->
                                    false
                                end
                            end, NewToBeAcked),
            Post = lists:filter(fun(Entry) -> 
                                Received = maps:get(received, Entry),
                                if Received == post ->
                                    true;
                                true ->
                                    false
                                end
                            end, NewToBeAcked),
            
            {UpdatedToBeAcked, NewCollisionTable, Acked} =  if Type == normal ->
                                {ToBeRemoved, NewPost} = lists:foldl(fun(Entry, {Acked, NotAcked}) ->
                                                        DroneId = maps:get(id, Entry),
                                                        DronePid = maps:get(pid, maps:get(DroneId, PersonalCollisions)),
                                                        {P_x, P_y} = maps:get(Id, maps:get(points, maps:get(DroneId, PersonalCollisions))),
                                                        Passed = check_intersection_passed({Start_x, Start_y}, {End_x, End_y}, {New_x, New_y}, {P_x, P_y}, maps:get(drone_size, Configuration)),
                                                        if Passed == true ->
                                                            DronePid ! {ack, self(), Id},
                                                            {[DroneId | Acked], NotAcked};
                                                        true ->
                                                            {Acked, [Entry | NotAcked]}
                                                        end
                                        end, {[], []}, Post),
                                {lists:append(Pre, NewPost), maps:without(ToBeRemoved, CollisionTable), ToBeRemoved};
                            true ->
                                {ToBeRemoved, NewPre} = lists:foldl(fun(Entry, {Acked, NotAcked}) ->
                                                    DroneId = maps:get(id, Entry),
                                                    DronePid = maps:get(pid, maps:get(DroneId, PersonalCollisions)),
                                                    {P_x, P_y} = maps:get(Id, maps:get(points, maps:get(DroneId, PersonalCollisions))),
                                                    if Real_x == P_x, Real_y == P_y ->
                                                        DronePid ! {ack, self(), Id},
                                                        {[DroneId | Acked], NotAcked};
                                                    true ->
                                                        {Acked, [Entry | NotAcked]}
                                                    end
                                        end, {[], []}, Pre),
                                {lists:append(Post, NewPre), maps:without(ToBeRemoved, CollisionTable), ToBeRemoved}
                            end,

            {NewDroneState, UpdatedCollisionTable} = utils:change_state(Id, DeliveryState, DroneState, NewCollisionTable),

            NewAlreadyAcked = lists:append(AlreadyAcked, Acked),

            RemovedPersonalCollisions = sets:to_list(sets:subtract(sets:from_list(maps:keys(CollisionTable)), sets:from_list(maps:keys(UpdatedCollisionTable)))),
            NewPersonalCollisions = maps:without(RemovedPersonalCollisions, PersonalCollisions),

            handle_state(Id, Configuration, NewDroneState, {Real_x, Real_y}, UpdatedCollisionTable, NewDrones, NewPersonalCollisions, UpdatedToBeAcked, FlyingProcessPid, RestConnection, NewAlreadyAcked);

        {'EXIT', FlyingProcessPid, Reason} ->
            if Reason =/= normal ->
                Ids_toBeAcked = lists:map(fun(Entry) -> maps:get(id, Entry) end, ToBeAcked),
                UpdatedToBeAcked = lists:map(fun(Entry) -> 
                                        #{id => maps:get(id, Entry), received => pre}
                        end, ToBeAcked),
                NewFlyingProcessPid = on_waiting_notify:spawnFlightProcess(Id, Configuration, PersonalCollisions, Ids_toBeAcked),
                handle_state(Id, Configuration, DroneState, CurrentPosition, CollisionTable, NewDrones, PersonalCollisions, UpdatedToBeAcked, NewFlyingProcessPid, RestConnection, AlreadyAcked);
            true ->
                RemainingAcks = lists:delete(Id, maps:keys(CollisionTable)),
                lists:foreach(fun(DroneId) -> 
                                DronePid = maps:get(DroneId, NewDrones),
                                DronePid ! {ack, self(), Id}
                        end, RemainingAcks),

                %% It's possible that when the flight is completed there are still some update_table messages that are coming
                %% from some new drone that sent a sync_hello with a collision result. In this case, to 
                maps:foreach(fun(_DroneId, Entry) -> 
                            DronePid = maps:get(pid, Entry),
                            DronePid ! {update_table, self(), Id, remove, none, none, none}
                    end,PersonalCollisions),
                
                % When no other messages need to be sent, can be done the update of the delivery changing the state
                % completed
                Resource = "/delivery/",
                {Start_x, Start_y} = maps:get(route_start, Configuration),
                {End_x, End_y} = maps:get(route_end, Configuration),
                CodedPid = maps:get(coded_pid, Configuration),
                Fallen = maps:get(fallen, DroneState),
                UpdatedDelivery = #{
                    id => Id,
                    pid => CodedPid,
                    state => completed,
                    start_x => Start_x,
                    start_y => Start_y,
                    current_x => End_x,
                    current_y => End_y,
                    end_x => End_x, 
                    end_y => End_y,
                    fallen => Fallen
                },
                %% TODO: Add management of the response
                _Response = http_utils:doPost(RestConnection, Resource, UpdatedDelivery),

                logging:log(Id, "Arrived at the final point of the delivery", [])
                
            end
    end.


check_intersection_passed(Start, End, CurrentPosition, IntersectionPoint, DroneSize) ->
    {Intersection_x, Intersection_y} = IntersectionPoint,
    {Start_x, Start_y} = Start,
    {Current_x, Current_y} = CurrentPosition,
    {End_x, End_y} = End,
    if Start_x == End_x ->
        if Start_y < End_y ->
            NewCurrentPoint = {Current_x, Current_y - (DroneSize/2)},
            DistanceStartCurrent = geometry_utils:distancePointPoint(Start, NewCurrentPoint),
            NewIntersectionPoint = {Intersection_x, Intersection_y + (DroneSize/2)},
            DistanceStartIntersection = geometry_utils:distancePointPoint(Start, NewIntersectionPoint),
            if DistanceStartIntersection =< DistanceStartCurrent ->
                true;
            true ->
                false
            end;
        true ->
            NewCurrentPoint = {Current_x, Current_y + (DroneSize/2)},
            DistanceStartCurrent = geometry_utils:distancePointPoint(Start, NewCurrentPoint),
            NewIntersectionPoint = {Intersection_x, Intersection_y - (DroneSize/2)},
            DistanceStartIntersection = geometry_utils:distancePointPoint(Start, NewIntersectionPoint),
            if DistanceStartIntersection =< DistanceStartCurrent ->
                true;
            true ->
                false
            end 
        end;
    true -> 
        RouteAngle = flight:compute_slope(Start, End),
        Step_x = math:cos(RouteAngle) * (DroneSize/2),
        Step_y = math:sin(RouteAngle) * (DroneSize/2),
        if Start_x < End_x ->  
            NewCurrentPoint = {Current_x - Step_x, Current_y - Step_y},
            DistanceStartCurrent = geometry_utils:distancePointPoint(Start, NewCurrentPoint),
            NewIntersectionPoint = {Intersection_x + Step_x, Intersection_y + Step_y},
            DistanceStartIntersection = geometry_utils:distancePointPoint(Start, NewIntersectionPoint),
            if DistanceStartIntersection =< DistanceStartCurrent ->
                true;
            true ->
                false
            end;
        true ->
            NewCurrentPoint = {Current_x + Step_x, Current_y + Step_y},
            DistanceStartCurrent = geometry_utils:distancePointPoint(Start, NewCurrentPoint),
            NewIntersectionPoint = {Intersection_x - Step_x, Intersection_y - Step_y},
            DistanceStartIntersection = geometry_utils:distancePointPoint(Start, NewIntersectionPoint),
            if DistanceStartIntersection =< DistanceStartCurrent ->
                true;
            true ->
                false
            end
        end
    end.
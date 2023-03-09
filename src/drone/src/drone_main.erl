-module(drone_main).
-define(RETRY_LIMIT,2).
-export([start_link/0, init/1, drone_synchronizer/8, send_update_table/3, agreement_loop/7]).

start_link() ->
    Id = list_to_integer(os:getenv("ID")),
    Pid = spawn(drone_main, init, [Id]),
    register(drone, Pid),
    {ok,Pid}.

init(Id) ->
    process_flag(trap_exit, true),

    {drone_hub, 'drone_hub@drone_hub_host'} ! {link, {node(), self()}, Id},
    {Configuration, DroneState} = receive_configuration(),

    logging:log(Id, "Started", []),

    Route = {utils:get_route_start(Configuration), maps:get(route_end, Configuration)},
    case http_utils:createConnection() of
        connection_timed_out ->
            logging:log(Id, "Warning: Rest service not reachable", []);
        Connection ->
            Resource = "/delivery/get_active_drones/?fields=id,pid",
            Response = http_utils:doGet(Connection, Resource),

            SynchronizationMap = lists:foldl(fun(Data, Acc0) ->
                            External_Id = maps:get(<<"id">>, Data),
                            if External_Id =/= Id ->
                                Pid = utils:pid_decoding(maps:get(<<"pid">>, Data)),
                                % Pid = list_to_pid(binary_to_list(maps:get(<<"pid">>, Data))),
                                Map = #{
                                    pid => Pid,
                                    %% At the start this flag will be false for each drone in the synchronization map
                                    received_result => false,
                                    %% At the start this flag will be true for each drone in the synchronization map
                                    %% because we assume that I'm in collision with all the other
                                    %% Then every time the drone receive a sync_result from another drone, this flag
                                    %% will be set in according to the collision_response (collision => true, no_collision => false)
                                    send_my_table => true
                                },
                                Acc1 = maps:put(External_Id, Map, Acc0),
                                Acc1;
                            true ->
                                Acc0
                            end 
                        end, #{}, Response),
            
            PersonalCollisions = #{},
            NewDrones = #{},
            ToNotUpdate = sets:new(),
            
            %% We need to send synchronization request only to drones with a smaller Id.
            %% If a drone is a recovery drone, its synchronization map contains all the drones but not him
            IsRecovery = maps:get(recovery, Configuration),
            FilteredSynchronizationMap = if IsRecovery =/= true ->
                                                maps:filter(fun(K, _V) -> 
                                                        if K < Id -> 
                                                            true;
                                                        true ->
                                                            false
                                                        end
                                            end, SynchronizationMap);
                                        true -> maps:filter(fun(K, _V) -> 
                                                        if K =/= Id -> 
                                                            true;
                                                        true ->
                                                            false
                                                        end
                                            end, SynchronizationMap)
                                        end,
            
            maps:foreach(fun(K, V) -> 
                    logging:log(Id, "Id: ~p --> ~p", [K, V])
                end, FilteredSynchronizationMap),

            Size = maps:size(FilteredSynchronizationMap),
            if Size > 0 ->
                CollisionTable = #{},
                Message = {sync_hello, self(), Id, Route},
                
                maps:foreach(fun(External_Id, Entry) ->
                                    External_Pid = maps:get(pid, Entry),
                                    spawn(drone_main, drone_synchronizer, [Id, self(), maps:get(drone_size, Configuration), Route, External_Id, External_Pid, 0, Message])
                            end, FilteredSynchronizationMap),
                sync_loop(Id, Configuration, DroneState, CollisionTable, FilteredSynchronizationMap, NewDrones, PersonalCollisions, ToNotUpdate);
            true ->
                CollisionTable = #{
                        Id => #{notify_count => [], collisions => sets:new(), state => pending}
                },
                logging:log(Id, "Started agreement phase", []),
                agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, ToNotUpdate)
            end
    end.

receive_configuration() ->
    receive
        {config, CodedPid, Velocity, Drone_size, Notify_Threshold, Policy, Recovery, Height, Start, Current, End, State, Fallen} ->
            Config = #{
                coded_pid => CodedPid, 
                height => Height,
                route_start => Start,
                route_end => End,
                recovery_start => Current,
                velocity => Velocity,
                drone_size => Drone_size,
                policy => Policy,
                notify_threshold => Notify_Threshold,
                recovery => Recovery
            },
            DroneState = #{
                state => State,
                notify_count => [],
                fallen => Fallen
            },
            {Config, DroneState}
    end.

drone_synchronizer(Id, MainPid, DroneSize, Route, External_Id, External_Pid, Retry_count, Message) ->
    External_Pid ! {sync_hello, self(), Id, MainPid, Route},
    logging:log(Id, "Sent sync_hello message to drone ~p with Pid ~p", [External_Id, External_Pid]),
    receive
        {sync_result, External_Pid, External_Id, Collision_response, Collision_points} ->

            MainPid ! {collision_response, External_Pid, External_Id, Collision_response, Collision_points}
        after 3000 ->
            if Retry_count > ?RETRY_LIMIT ->
                MainPid ! {collision_response, External_Pid, External_Id, no_collision, none};
            true ->
                drone_synchronizer(Id, MainPid, DroneSize, Route, External_Id, External_Pid, Retry_count + 1, Message)
            end
    end.

sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, NewDrones, PersonalCollisions, ToNotUpdate) ->
    receive
        {collision_response, External_Pid, External_Id, Collision_response, Collision_points} ->
            logging:log(Id, "Received collision_response with result ~p from drone_synchronizer process about collision with drone ~p on pid ~p", [Collision_response, External_Id, External_Pid]),
            
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
                logging:log(Id, "PersonalCollisions after sync phase: ~p", [NewPersonalCollisions]),
                
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
                StartAgreement = go_to_agreement(NewCollisionTable, NewSynchronizationMap),
                if StartAgreement == true ->
                    agreement_loop(Id, Configuration, DroneState, NewCollisionTable, NewDrones, NewPersonalCollisions, ToNotUpdate);
                true ->
                    sync_loop(Id, Configuration, DroneState, NewCollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions, ToNotUpdate)
                end;
            true ->
                sync_loop(Id, Configuration, DroneState, CollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions, ToNotUpdate)
            end;

        {sync_hello, FromPid, FromId, FromRoute} ->
            logging:log(Id, "Received sync_hello message from drone ~p to compute collision computation", [FromId]),
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

            logging:log(Id, "Received update_table message from drone ~p with action ~p", [FromId, Action]),
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

                    StartAgreement = go_to_agreement(NewCollisionTable, NewSynchronizationMap),

                    if StartAgreement == true ->
                        agreement_loop(Id, Configuration, DroneState, NewCollisionTable, NewDrones, NewPersonalCollisions, ToNotUpdate);
                    true ->
                        sync_loop(Id, Configuration, DroneState, NewCollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions, ToNotUpdate)
                    end;
            true ->
                logging:log(Id, "Received an update_table message from drone ~p that is not in collision with me", [FromId]),
                sync_loop(Id, Configuration, DroneState, CollisionTable, NewSynchronizationMap, NewDrones, PersonalCollisions, ToNotUpdate)
            end
    end.

agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, ToNotUpdate) ->
    %Policy = maps:get(policy, Configuration),
    % Ordering = apply(Policy, [CollisionTable, maps:get(notify_threshold, Configuration)]),
    Ordering = drone_policy:compute_policy(CollisionTable, maps:get(notify_threshold, Configuration)),
    logging:log(Id, "Ordering: ~p", [Ordering]),
    [Head | _Tail] = Ordering,
    if Head == Id ->
        UpdateTableBuffer = [],
        ToBeAcked = sets:new(),
        on_waiting_notify:handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked);
    true ->
        ToBeNotified = create_notify_buffer(Ordering, CollisionTable),

        send_notify(Id, ToBeNotified, PersonalCollisions),
        ReceivedAcks = [],
        NewDroneState = #{
            notify_count => sets:to_list(sets:from_list(lists:append(maps:get(notify_count, DroneState), ToBeNotified))),
            state => pending,
            fallen => maps:get(fallen, DroneState)
        },
        MyCollisions = maps:get(Id, CollisionTable),
        NewMyCollisions = #{
            collisions => maps:get(collisions, MyCollisions),
            notify_count => maps:get(notify_count, NewDroneState),
            state => maps:get(state, NewDroneState)
        },
        UpdatedCollisionTable = maps:put(Id, NewMyCollisions, CollisionTable),

        on_waiting_ack:handle_state(Id, Configuration, NewDroneState, UpdatedCollisionTable, NewDrones, PersonalCollisions, sets:from_list(ToBeNotified), ReceivedAcks, ToNotUpdate)
    end.


send_update_table(Id, PersonalCollisions, DroneState) ->
    maps:foreach(fun(External_Id, Entry) ->
            External_Pid = maps:get(pid, Entry),
            utils:send_update_table_add(Id, External_Pid, PersonalCollisions, DroneState),
            logging:log(Id, "Sent update_table message to drone ~p", [External_Id])    
        end, PersonalCollisions). 

create_notify_buffer(Ordering, CollisionTable) ->
    AccOut = lists:foldl(fun(T, AccIn) ->
                CollisionsT = maps:get(collisions, maps:get(T, CollisionTable)),
                StateT = maps:get(state, maps:get(T, CollisionTable)),
                SetAccIn = sets:from_list(AccIn),
                Size = sets:size(sets:intersection(CollisionsT, SetAccIn)),
                if Size == 0; StateT == flying -> 
                    [T | AccIn];
                true ->
                    AccIn
                end
        end, [], Ordering),
    AccOut.

send_notify(Id, ToBeNotified, PersonalCollisions) ->
    lists:foreach(fun(T) ->
                External_Pid = maps:get(pid, maps:get(T, PersonalCollisions)),
                External_Pid ! {notify, self(), Id},
                logging:log(Id, "Sent notify message to drone ~p", [T])
            end, ToBeNotified).


%% Go_to_agreement simply check if for each drone in the SynchronizationMap with flag send_my_table = true,
%% I have an entry in the CollisionTable
go_to_agreement(CollisionTable, SynchronizationMap) ->
    FilteredMap = maps:filter(fun(_K, V) -> 
                            SendMyTableFlag = maps:get(send_my_table, V),
                            if SendMyTableFlag == true ->
                                true;
                            true ->
                                false
                            end
                end, SynchronizationMap),
    ReceivedAll = maps:fold(fun(K, _V, AccIn) ->
                            Table = maps:get(K, CollisionTable, not_exists),
                            if Table == not_exists ->
                                false;
                            true ->
                                AccIn
                            end
                        end, true, FilteredMap),
    ReceivedAll.
-module(drone_main).
-define(RETRY_LIMIT,2).
-export([start_link/0, init/1, drone_synchronizer/8, update_personal_collisions/5, send_update_table/3, agreement_loop/6, get_route_start/1]).

start_link() ->
    Id = list_to_integer(os:getenv("ID")),
    Pid = spawn(drone_main, init, [Id]),
    register(drone, Pid),
    {ok,Pid}.

init(Id) ->
    process_flag(trap_exit, true),

    {drone_hub, 'drone_hub@drone_hub_host'} ! {link, {node(), self()}, Id},
    {Configuration, DroneState} = receive_configuration(),

    io:format("Drone ~p started~n", [Id]),

    Route = {get_route_start(Configuration), maps:get(route_end, Configuration)},
    case http_utils:createConnection() of
        connection_timed_out ->
            io:format("Warning Rest service not reachable");
        Connection ->
            Resource = "/delivery/get_active_drones/?fields=id,pid",
            Response = http_utils:doGet(Connection, Resource),

            Acc = #{}, 
            SynchronizationMap = lists:foldl(fun(Data, Acc0) ->
                            External_Id = maps:get(<<"id">>, Data),
                            if External_Id =/= Id ->
                                Pid = list_to_pid(binary_to_list(maps:get(<<"pid">>, Data))),
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
                        end, Acc, Response),
            
            PersonalCollisions = #{},
            NewDrones = #{},
            
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

            Size = maps:size(FilteredSynchronizationMap),
            if Size > 0 ->
                CollisionTable = #{},
                Message = {sync_hello, self(), Id, Route},
                
                maps:foreach(fun(External_Id, Entry) ->
                                    External_Pid = maps:get(pid, Entry),
                                    spawn(drone_main, drone_synchronizer, [Id, self(), maps:get(drone_size, Configuration), Route, External_Id, External_Pid, 0, Message])
                            end, FilteredSynchronizationMap),
                sync_loop(Id, Configuration, DroneState, CollisionTable, FilteredSynchronizationMap, NewDrones, PersonalCollisions);
            true ->
                CollisionTable = #{
                        Id => #{ack_count => [], collisions => [], state => pending}
                },
                io:format("Drone ~p --> Started agreement phase~n", [Id]),
                agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions)
            end
    end.

receive_configuration() ->
    receive
        {config, Velocity, Drone_size, Policy, Recovery, Height, Start, Current, End, State, Fallen} ->
            Config = #{
                height => Height,
                route_start => Start,
                route_end => End,
                recovery_start => Current,
                velocity => Velocity,
                drone_size => Drone_size,
                policy => Policy,
                recovery => Recovery
            },
            DroneState = #{
                state => State,
                ack_count => [],
                fallen => Fallen
            },
            {Config, DroneState}
    end.

drone_synchronizer(Id, MainPid, DroneSize, Route, External_Id, External_Pid, Retry_count, Message) ->
    External_Pid ! {sync_hello, self(), Id, MainPid, Route},
    io:format("Drone ~p --> Sent sync_hello message to drone ~p with Pid ~p~n", [Id, External_Id, External_Pid]),
    receive
        {sync_result, External_Pid, External_Id, Collision_response, Collision_points} ->
            io:format("Drone ~p --> Received sync_result from drone ~p with response ~p~n", [Id, External_Id,Collision_response]),

            MainPid ! {collision_response, External_Pid, External_Id, Collision_response, Collision_points}
        after 3000 ->
            if Retry_count > ?RETRY_LIMIT ->
                MainPid ! {collision_response, External_Pid, External_Id, no_collision, none};
            true ->
                drone_synchronizer(Id, MainPid, DroneSize, Route, External_Id, External_Pid, Retry_count + 1, Message)
            end
    end.

sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, NewDrones, PersonalCollisions) ->
    receive
        {collision_response, External_Pid, External_Id, Collision_response, Collision_points} ->
            io:format("Drone ~p --> Received collision_response with result ~p from drone_synchronizer process about collision with drone ~p on pid ~p~n", 
                                        [Id, Collision_response, External_Id, External_Pid]),
            
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
            NewSynchronizationMap = maps:put(External_Id, NewMap, SynchronizationMap),

            %% According to the response from the collision computation, I can update my personal collisions
            NewPersonalCollisions = case Collision_response of
                                collision ->
                                    maps:put(External_Id, #{pid => External_Pid, points => Collision_points}, PersonalCollisions);
                                no_collision ->
                                    PersonalCollisions
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
                io:format("Drone ~p --> PersonalCollisions after sync phase: ~p~n", [Id, NewPersonalCollisions]),
                
                %% Since I have received all the collision computance from all the other drones, I can insert the row 
                %% regarding myself in my collision table
                Map = #{
                    collisions => maps:keys(NewPersonalCollisions),
                    state => maps:get(state, DroneState),
                    ack_count => maps:get(ack_count, DroneState)
                },
                NewCollisionTable = maps:put(Id, Map, CollisionTable),
                
                %% So, since I know all my collisions I'm ready to send update_table message to drones involved with me
                %% I'm also sure that in can get in this if only once, since the the condition Size == 0 is evaluated only
                %% when I receive a sync_result from other drones (so once I get them all I never get in this receive clause)
                io:format("Drone ~p --> Sending update table in sync_loop with this PersonalCollision: ~p~n", [Id, NewPersonalCollisions]),
                send_update_table(Id, NewPersonalCollisions, DroneState),

                
                %% The following condition will be evaluated to true only when the current drone hasn't collisions with other drones
                StartAgreement = go_to_agreement(NewCollisionTable, NewSynchronizationMap),
                if StartAgreement == true ->
                    agreement_loop(Id, Configuration, DroneState, NewCollisionTable, NewDrones, NewPersonalCollisions);
                true ->

                %% Actually, probably the condition StartAgreement == 0 will be never evaluated to true since I can receive update_table messages
                %% only when I have already sent my update_table message to other drones
            
                    sync_loop(Id, Configuration, DroneState, NewCollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions)
                end;
            true ->
                sync_loop(Id, Configuration, DroneState, CollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions)
            end;

        {sync_hello, FromPid, FromId, FromRoute} ->
            io:format("Drone ~p --> Received sync_hello message from drone ~p~n to compute collision computation", [Id, FromId]),
            MyStart = get_route_start(Configuration),
            MyEnd = maps:get(route_end, Configuration),
            DroneSize = maps:get(drone_size, Configuration),

            {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, {MyStart, MyEnd}, FromId, FromRoute),
            NewPersonalCollisions = update_personal_collisions(Collision_response, FromPid, FromId, Collision_points, PersonalCollisions),
            
            FromPid ! {sync_result, self(), Id, Collision_response, Collision_points},
            sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, maps:put(FromId, FromPid, NewDrones), NewPersonalCollisions);
        
        {update_table, _FromPid, FromId, Action, FromCollidingDrones, FromState, FromAck_count} ->
            io:format("Drone ~p --> Received update_table message from drone ~p with action ~p~n", [Id, FromId, Action]),
            %% We need to check that we received an update_table message from a drone that is already in our PersonalCollisions
            %% Otherwise this could be an old message
            Drone = maps:get(FromId, PersonalCollisions, not_exists),

            NewSynchronizationMap = if Action == remove ->
                                            maps:put(FromId, maps:put(send_my_table, false, maps:get(FromId, SynchronizationMap)), SynchronizationMap);
                                    true ->
                                        SynchronizationMap
                                    end,

            if Drone =/= not_exists ->
                    NewCollisionTable = if Action =/= remove ->
                                            Collisions = #{
                                                collisions => FromCollidingDrones,
                                                state => FromState,
                                                ack_count => FromAck_count
                                            },
                                            maps:put(FromId, Collisions, CollisionTable);
                                        true ->
                                            MyEntryCollisionTable = #{
                                                collisions => lists:delete(FromId, maps:get(collisions, maps:get(Id, CollisionTable))),
                                                ack_count => maps:get(collisions, maps:get(Id, CollisionTable)),
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
                        agreement_loop(Id, Configuration, DroneState, NewCollisionTable, NewDrones, NewPersonalCollisions);
                    true ->
                        sync_loop(Id, Configuration, DroneState, NewCollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions)
                    end;
            true ->
                io:format("Drone ~p --> Received an update_table message from drone ~p that is not in collision with me", [Id, FromId]),
                sync_loop(Id, Configuration, DroneState, CollisionTable, NewSynchronizationMap, NewDrones, PersonalCollisions)
            end
    end.

agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions) ->
    io:format("Drone ~p --> Making agreement with CollisionTable: ~p; PersonalCollisions: ~p~n", [Id, CollisionTable, PersonalCollisions]),
    
    Policy = maps:get(policy, Configuration),
    Ordering = apply(Policy, [CollisionTable]),
    io:format("Drone ~p --> Ordering: ~p~n", [Id, Ordering]),
    [Head | _Tail] = Ordering,
    if Head == Id ->
        UpdateTableBuffer = [],
        ToBeAcked = [],
        on_waiting_notify:handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, UpdateTableBuffer, ToBeAcked);
    true ->
        ToBeNotified = create_notify_buffer(Ordering, CollisionTable),
        io:format("Drone ~p --> ToBeNotified: ~p~n", [Id, ToBeNotified]),
        send_notify(Id, ToBeNotified, PersonalCollisions),
        ReceivedAcks = [],
        io:format("Drone ~p --> Waiting ack messages from drones: ~p~n", [Id, ToBeNotified]),
        on_waiting_ack:handle_state(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, ToBeNotified, ReceivedAcks)
    end.

update_personal_collisions(Collision_response, FromPid, FromId, Collision_points, PersonalCollisions) ->
    NewPersonalCollisions = case Collision_response of
                        collision ->
                            maps:put(FromId, #{pid => FromPid, points => Collision_points}, PersonalCollisions);
                        no_collision ->
                            PersonalCollisions
                    end,
    NewPersonalCollisions.


send_update_table(Id, PersonalCollisions, DroneState) ->
    maps:foreach(fun(External_Id, Entry) ->
            External_Pid = maps:get(pid, Entry),

            CollidingDrones = maps:keys(PersonalCollisions),
            State = maps:get(state, DroneState),
            Ack_count = maps:get(ack_count, DroneState),
            External_Pid ! {update_table, self(), Id, add, CollidingDrones, State, Ack_count},

            io:format("Drone ~p --> Sent update_table message to drone ~p~n", [Id, External_Id])        
        end, PersonalCollisions). 

create_notify_buffer(Ordering, CollisionTable) ->
    AccOut = lists:foldl(fun(T, AccIn) ->
                CollisionsT = sets:from_list(maps:get(collisions, maps:get(T, CollisionTable))),
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
                io:format("Drone ~p --> Sent notify message to drone ~p~n", [Id, T])
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

get_route_start(Configuration) ->
    Recovery = maps:get(recovery, Configuration),
    if Recovery == true ->
        maps:get(recovery_start, Configuration);
    true ->
        maps:get(route_start, Configuration)
    end.
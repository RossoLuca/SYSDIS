-module(drone_main).
-define(RETRY_LIMIT,2).
-export([start_link/0, init/1, drone_synchronizer/8]).

start_link() ->
    Id = list_to_integer(os:getenv("ID")),
    Pid = spawn(drone_main, init, [Id]),
    register(drone, Pid),
    {ok,Pid}.

init(Id) ->
    {drone_hub, 'drone_hub@drone_hub_host'} ! {link, {node(), self()}, Id},
    {Configuration, DroneState} = receive_configuration(),

    io:format("Drone ~p started~n", [Id]),

    Route = {maps:get(route_start, Configuration), maps:get(route_end, Configuration)},
    case http_utils:createConnection() of
        connection_timed_out ->
            io:format("Warning Rest service not reachable");
        Connection ->
            Resource = "/delivery/get_active_drones",
            Response = http_utils:doGet(Connection, Resource),

            Acc = #{}, 
            SynchronizationMap = lists:foldl(fun(Data, Acc0) ->
                            External_Id = maps:get(<<"id">>, Data),
                            if External_Id =/= Id ->
                                Pid = list_to_pid(binary_to_list(maps:get(<<"pid">>, Data))),
                                Acc1 = maps:put(External_Id, Pid, Acc0),
                                Acc1;
                            true ->
                                Acc0
                            end 
                        end, Acc, Response),
            
            NewDrones = #{},
            PersonalCollisions = #{},
            CollisionTable = #{},

            io:format("Drone ~p ---> ~p~n", [Id, SynchronizationMap]),

            Size = maps:size(SynchronizationMap),
            if Size > 0 ->
                Message = {sync_hello, self(), Id},

                maps:foreach(fun(External_Id, External_Pid) ->
                                    spawn(drone_main, drone_synchronizer, [Id, self(), maps:get(drone_size, Configuration), Route, External_Id, External_Pid, 0, Message])
                            end, SynchronizationMap),
                sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, NewDrones, PersonalCollisions);
            true ->
                io:format("Drone ~p --> Started agreement phase~n", [Id]),
                agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions)
            end
    end.

receive_configuration() ->
    receive
        {config, Velocity, Drone_size, Policy, Recovery, Start, End, State, Fallen} ->
            Config = #{
                route_start => Start,
                route_end => End,
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

drone_synchronizer(Id, Pid, DroneSize, Route, External_Id, External_Pid, Retry_count, Message) ->
    External_Pid ! {sync_hello, self(), Id},
    io:format("Drone ~p --> Sent sync_hello message to drone ~p~n", [Id, External_Id]),
    receive
        {sync_routes, External_Id, External_Pid, Start, End, _State, _Ack_count} ->
            io:format("Drone ~p --> Received sync_routes message from drone ~p~n", [Id, External_Id]),
            {Collision_response, Collision_points} = collision_detection:compute_collision(DroneSize, Id, Route, External_Id, {Start, End}),
            io:format("Drone ~p --> Computed collision with drone ~p: ~p; ~p~n", [Id, External_Id, Collision_response, Collision_points]),
            Pid ! {collision_response, External_Id, External_Pid, Collision_response, Collision_points}
        after 3000 ->
            if Retry_count > ?RETRY_LIMIT ->
                case http_utils:createConnection() of
                    connection_timed_out ->
                        io:format("Warning Rest service not reachable");
                    Connection ->
                        Resource = "/delivery/?id=",
                        Query = Resource ++ integer_to_list(External_Id),
                        Response = http_utils:doGet(Connection, Query),
                        %% TODO: verificare che lo stato della delivery non sia completed
                        %% se è completed il sync con il drone corrispondennte si può considerare concluso
                        %% TODO: modificare modo in cui si accede al nuovo pid (questo da un errore)
                        New_pid = maps:get(<<"pid">>, lists:nth(1,Response)),
                        drone_synchronizer(Id, Pid, DroneSize, Route, External_Id, New_pid, 0, Message)
                end;
            true ->
                drone_synchronizer(Id, Pid, DroneSize, Route, External_Id, External_Pid, Retry_count + 1, Message)
            end
    end.


% %% ALL THE BUSINESS LOGIC
sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, NewDrones, PersonalCollisions) ->
    receive
        {collision_response, External_Id, External_Pid, Collision_response, Collision_points} ->
            External_Pid ! {sync_result, self(), Id, Collision_response, Collision_points},
            
            NewSynchronizationMap = maps:remove(External_Id, SynchronizationMap),
            Size = maps:size(NewSynchronizationMap),

            NewPersonalCollisions = case Collision_response of
                                collision ->
                                    maps:put(External_Id, #{pid => External_Pid, points => Collision_points}, PersonalCollisions);
                                no_collision ->
                                    PersonalCollisions
                            end,
            if Size == 0 ->
                io:format("Drone ~p --> PersonalCollisions after sync phase: ~p~n", [Id, NewPersonalCollisions]),
                
                Map = #{
                    collisions => maps:keys(NewPersonalCollisions),
                    state => maps:get(state, DroneState),
                    ack_count => maps:get(ack_count, DroneState)
                },
                NewCollisionTable = maps:put(Id, Map, CollisionTable),
                
                %% In the sync phase the update_table message is sent only when all the drone synchronizer process
                %% have terminated
                send_update_table(Id, PersonalCollisions, DroneState),
                
                io:format("Drone ~p --> Started agreement phase~n", [Id]),
                agreement_loop(Id, Configuration, DroneState, NewCollisionTable, NewDrones, NewPersonalCollisions);
            true ->
                sync_loop(Id, Configuration, DroneState, CollisionTable, NewSynchronizationMap, NewDrones, NewPersonalCollisions)
            end;

        {sync_hello, FromPid, FromId} ->
            io:format("Drone ~p --> Received sync_hello message from drone ~p~n", [Id, FromId]),
            Start = maps:get(route_start, Configuration),
            End = maps:get(route_end, Configuration),
            State = maps:get(state, DroneState),
            Ack_count = maps:get(ack_count, DroneState),
            FromPid ! {sync_routes, Id, self(), Start, End, State, Ack_count},
            sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, maps:put(FromId, FromPid, NewDrones), PersonalCollisions);

        {sync_result, FromPid, FromId, Collision_response, Collision_points} ->
            NewPersonalCollisions = update_personal_collisions(Collision_response, FromPid, FromId, Collision_points, PersonalCollisions),

            % In this case we don't send an update_table message to the other since in this phase this will be done only
            % when all the drone synchronizer process have terminated
            sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, NewDrones, NewPersonalCollisions);
        
        {update_table, FromPid, FromId, FromCollidingDrones, FromState, FromAck_count} ->
            Collisions = #{
                collisions => FromCollidingDrones,
                state => FromState,
                ack_count => FromAck_count
            },
            NewCollisionTable = maps:put(FromId, Collisions, CollisionTable),
            
            sync_loop(Id, Configuration, DroneState, NewCollisionTable, SynchronizationMap, NewDrones, PersonalCollisions)

    end.


agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions) ->
    io:format("Drone ~p --> CollisionTable: ~p~n", [Id, CollisionTable]),
    %% TODO: Aggiungere qui tutta la logica della agreement

    receive
        {sync_hello, FromPid, FromId} ->
            io:format("Drone ~p --> Received sync_hello message from drone ~p~n", [Id, FromId]),
        
            Start = maps:get(route_start, Configuration),
            End = maps:get(route_end, Configuration),
            State = maps:get(state, DroneState),
            Ack_count = maps:get(ack_count, DroneState),
            
            FromPid ! {sync_routes, Id, self(), Start, End, State, Ack_count},
            agreement_loop(Id, Configuration, DroneState, CollisionTable, maps:put(FromId, FromPid, NewDrones), PersonalCollisions);
        
        {sync_result, FromPid, FromId, Collision_response, Collision_points} ->
            NewPersonalCollisions = update_personal_collisions(Collision_response, FromId, FromPid, Collision_points, PersonalCollisions),
                                 
            if Collision_response == collision ->
                send_update_table(Id, NewPersonalCollisions, DroneState),
                agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, NewPersonalCollisions);
            true -> 
                agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, NewPersonalCollisions)
            end;
        {update_table, FromPid, FromId, FromCollidingDrones, FromState, FromAck_count} ->
            % TODO: da gestire i casi speciali dell'algoritmo di agreement
            agreement_loop(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions)
    end.

update_personal_collisions(Collision_response, FromId, FromPid, Collision_points, PersonalCollisions) ->
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
            External_Pid ! {update_table, self(), Id, CollidingDrones, State, Ack_count},

            io:format("Drone ~p --> Sent update_table message to drone ~p~n", [Id, External_Id])        
        end, PersonalCollisions).
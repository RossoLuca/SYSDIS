-module(drone_supervisor).

-export([start_link/0, init/0, loop/4, spawnDrone/1, spawnLocalDrone/1, agreementPolicy/1]).

% Velocity is expressed in m/s
-define(VELOCITY, 5.0).

% Defines the size of the bounding box surrounding each drone
-define(DRONE_SIZE, 1.0).

-define(DRONE_HEIGHT, 50.0).

-define(NOTIFY_THRESHOLD, 4).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(drone_hub, Pid),
    {ok, Pid}.

init() ->
    %% This timeout is necessary to be sure that 
    %% Gun library is ready to be used
    receive
        after 2000 -> ok
    end,
    {Conn, IdMax} = getLastId(),
    SpawnedDrones = #{},
    MonitoredDrones = #{},
    Id_to_Pid = #{},
    SpawnedAfterRestore = restoreEnvironment(Conn, SpawnedDrones, MonitoredDrones),
    loop(IdMax, SpawnedAfterRestore, MonitoredDrones, Id_to_Pid).

loop(IdMax, Spawned, MonitoredDrones, Id_to_Pid) ->
    receive
        {create, {_FromNode, FromPid}, StartX, StartY, EndX, EndY} ->
            logging:log("Received a request of spawning a new drone from the Rest API"),
            FromPid ! {ack, IdMax},
            DEV_MODE = list_to_atom(os:getenv("DEV_MODE", "false")),
            if DEV_MODE == true ->
                spawnLocalDrone(IdMax);
            true ->
                spawn(?MODULE, spawnDrone, [IdMax])
            end,
            Delivery = #{
                id => IdMax,
                state => pending,
                start_x => StartX,
                start_y => StartY,
                current_x => StartX,
                current_y => StartY,
                end_x => EndX,
                end_y => EndY,
                fallen => " ",
                recovery => false    
            },
            NewIdMax = IdMax + 1,
            NewSpawned = maps:put(IdMax, Delivery, Spawned),
            loop(NewIdMax, NewSpawned, MonitoredDrones, Id_to_Pid);
        {link, {_FromNode, FromPid}, Id} ->
            logging:log("Received link from drone ~p with Pid ~p", [Id, FromPid]),
            Delivery = maps:get(Id, Spawned),
            DeliveryWithPid = maps:put(pid, pid_to_list(FromPid), Delivery),
            sendNewDelivery(maps:remove(recovery, DeliveryWithPid)),
            NewSpawned = maps:remove(Id, Spawned),

            erlang:monitor(process, FromPid),

            StartX = maps:get(start_x, Delivery),
            StartY = maps:get(start_y, Delivery),
            CurrentX = maps:get(current_x, Delivery),
            CurrentY = maps:get(current_y, Delivery),
            EndX = maps:get(end_x, Delivery),
            EndY = maps:get(end_y, Delivery),
            State = pending,
            Fallen = maps:get(fallen, Delivery),
            RecoveryFlag = maps:get(recovery, Delivery),
            Policy = fun(CollisionTable) -> agreementPolicy(CollisionTable) end,
            
            
            FromPid ! {config, ?VELOCITY, ?DRONE_SIZE, ?NOTIFY_THRESHOLD, Policy, RecoveryFlag, ?DRONE_HEIGHT, {StartX, StartY}, {CurrentX, CurrentY}, {EndX, EndY}, State, Fallen},

            NewMonitoredDrones = maps:put(FromPid, Id, MonitoredDrones),
            NewId_to_Pid = maps:put(Id, FromPid, Id_to_Pid),
            loop(IdMax, NewSpawned, NewMonitoredDrones, NewId_to_Pid); 
        {'DOWN', _Ref, process, FromPid, Reason} ->
            if Reason =/= normal ->
                Id = maps:get(FromPid, MonitoredDrones),
                logging:log("Drone ~p crashed.~n A new drone will be spawned to complete its delivery", [Id]),
                Delivery = get_last_drone_update(Id),
                DEV_MODE = list_to_atom(os:getenv("DEV_MODE", "false")),
                if DEV_MODE == true ->
                    spawnLocalDrone(Id);
                true ->
                    spawn(?MODULE, spawnDrone, [Id])
                end,
                NewSpawned = maps:put(Id, Delivery, Spawned),
                
                loop(IdMax, NewSpawned, MonitoredDrones, Id_to_Pid);
            true ->
                DroneId = maps:get(FromPid, MonitoredDrones),
                logging:log("Drone ~p has completed its task", [DroneId]),
                NewMonitoredDrones = maps:remove(DroneId, MonitoredDrones),
                loop(IdMax, Spawned, NewMonitoredDrones, Id_to_Pid)
            end;
        {kill, {_FromNode, FromPid}, Id} ->
            logging:log("Received a request of kill the drone with ID ~p, from the Rest API", [Id]),
            Drone = maps:get(Id, Id_to_Pid, not_found),
            if Drone =/= not_found ->
                kill_drone(Drone),
                FromPid ! {success, Id};
            true ->
                logging:log("Drone ~p not found!", [Id]),
                FromPid ! {error, drone_not_exists}
            end,
            loop(IdMax, Spawned, MonitoredDrones, Id_to_Pid);
        _ ->
            loop(IdMax, Spawned, MonitoredDrones, Id_to_Pid)
    end.

sendNewDelivery(Delivery) ->
    Conn = http_utils:createConnection(),
    Resource = "/delivery/",
    Response = http_utils:doPost(Conn, Resource, Delivery),
    Response.

% spawnDrone can be used to spawn each drone on a different containter
spawnDrone(Id) ->
    Network = "dis_sys",
    ContainerName = "drone_" ++ integer_to_list(Id),
    HostName = integer_to_list(Id) ++ "_host",
    EnvVariable = "ID=" ++ integer_to_list(Id),
    Image = "drone_image",
    Command = "docker -H unix:///var/run/docker.sock run --rm -d --name " ++ ContainerName ++ 
                    " -h " ++ HostName ++ " --env " ++ EnvVariable ++
                    " --net " ++ Network ++ " " ++ Image,
    _StdOut = os:cmd(Command).


% spawnLocalDrone can be used for testing to spawn drones in local
spawnLocalDrone(Id) ->
    spawn(drone_main, init, [Id]).


getLastId() ->
    Conn = http_utils:createConnection(),
    Resource = "/delivery/id",
    Response = http_utils:doGet(Conn, Resource),
    Info = binary_to_atom(maps:get(<<"info">>, Response)),
    case Info of
        success ->
            IdMax = maps:get(<<"result">>, Response),
            {Conn, IdMax};
        _ ->
            logging:log("Error during attempt to get setup info from the Rest"),
            logging:log("Drone hub will be restarted for another attemp"),
            exit(self(), rest_connection_error)
    end.

get_last_drone_update(DroneId) ->
    Connection = http_utils:createConnection(),
    Resource = "/delivery/?id=" ++ integer_to_list(DroneId),
    Response = http_utils:doGet(Connection, Resource),
    Object = lists:nth(1, Response),
    OldFallen = maps:get(<<"fallen">>, Object),
    CurrentTime = os:system_time(1000000),
    NewFallen = if OldFallen == " " ->
                    integer_to_list(CurrentTime);
                true ->
                    OldFallen ++ ";" ++ integer_to_list(CurrentTime)
                end,

    Delivery = #{
        id => maps:get(<<"id">>, Object),
        state => pending,
        start_x => maps:get(<<"start_x">>, Object),
        start_y => maps:get(<<"start_y">>, Object),
        current_x => maps:get(<<"current_x">>, Object),
        current_y => maps:get(<<"current_y">>, Object),
        end_x => maps:get(<<"end_x">>, Object),
        end_y => maps:get(<<"end_y">>, Object),
        fallen => NewFallen,
        recovery => true
    },
    Delivery.

restoreEnvironment(Conn, Spawned, _Monitored) ->
    Resource = "/delivery/get_active_drones",
    Response = http_utils:doGet(Conn, Resource),
    
    NewSpawned = lists:foldl(fun(Drone, AccIn) ->
            AccOut = restoreDrone(Drone, AccIn),
            AccOut
        end, Spawned, Response),
    NewSpawned.

restoreDrone(Drone, Spawned) ->
    Id = maps:get(<<"id">>, Drone),
    DEV_MODE = list_to_atom(os:getenv("DEV_MODE", "false")),
    if DEV_MODE == true ->
        spawnLocalDrone(Id);
    true ->
        spawn(?MODULE, spawnDrone, [Id])
    end,
    Delivery = #{
        id => Id,
        state => binary_to_atom(maps:get(<<"state">>, Drone)),
        start_x => maps:get(<<"current_x">>, Drone),
        start_y => maps:get(<<"current_y">>, Drone),
        current_x => maps:get(<<"current_x">>, Drone),
        current_y => maps:get(<<"current_y">>, Drone),
        end_x => maps:get(<<"end_x">>, Drone),
        end_y => maps:get(<<"end_y">>, Drone),
        fallen => maps:get(<<"fallen">>, Drone),
        recovery => true    
    },
    NewSpawned = maps:put(Id, Delivery, Spawned),
    NewSpawned.

kill_drone(Pid) ->
    exit(Pid, kill).

agreementPolicy(CollisionTable) ->
    FirstRule = maps:fold(fun(K, V, AccIn) ->
                State = maps:get(state, V),
                if State == flying ->
                    AccOut = [K | AccIn],
                    AccOut;
                true ->
                    AccIn
                end
    end, [], CollisionTable),
    FirstRuleOrdered = lists:sort(FirstRule),
    TableAfterFirstRule = lists:foldl(fun(K, Acc) -> 
                            Out = maps:remove(K, Acc),
                            Out
                    end, CollisionTable, FirstRuleOrdered),
    SecondRule = maps:fold(fun(K, V, AccIn) ->
                        Count = length(maps:get(notify_count, V)),
                        if Count >= ?NOTIFY_THRESHOLD ->
                            AccOut = [K | AccIn],
                            AccOut;
                        true ->
                            AccIn
                        end
                    end, [], TableAfterFirstRule),
    SecondRuleOrdered = lists:sort(SecondRule),
    TableAfterSecondRule = lists:foldl(fun(K, Acc) -> 
                            Out = maps:remove(K, Acc),
                            Out
                    end, TableAfterFirstRule, SecondRuleOrdered),
    ThirdRuleOrdered = lists:sort(fun({A, MapA}, {B, MapB}) ->
                    CollisionA = sets:size(maps:get(collisions, MapA)),
                    CollisionB = sets:size(maps:get(collisions, MapB)),
                    if CollisionA < CollisionB ->
                        true;
                    CollisionA == CollisionB ->
                        if A < B ->
                            true;
                        true ->
                            false
                        end;
                    true ->
                        false
                    end
        end, maps:to_list(TableAfterSecondRule)),
    ThirdRuleIds = lists:map(fun({K, _V}) -> K end, ThirdRuleOrdered),
    TotalOrdering = lists:append(FirstRuleOrdered, lists:append(SecondRuleOrdered, ThirdRuleIds)),
    TotalOrdering.
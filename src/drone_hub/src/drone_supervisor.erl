-module(drone_supervisor).

-export([start_link/0, init/0, loop/3, spawnDrone/1, spawnLocalDrone/1, agreementPolicy/0]).

% Velocity is expressed in m/s
-define(VELOCITY, 1.0).

% Defines the size of the bounding box surrounding each drone
-define(DRONE_SIZE, 1.0).

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
    IdMax = getLastId(),
    SpawnedDrones = #{},
    MonitoredDrones = #{},
    loop(IdMax, SpawnedDrones, MonitoredDrones).

loop(IdMax, Spawned, MonitoredDrones) ->
    receive
        {create, {_FromNode, FromPid}, StartX, StartY, EndX, EndY} ->
            io:format("Received a request of spawning a new drone from the Rest API~n"),
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
                fallen => " "    
            },
            NewIdMax = IdMax + 1,
            NewSpawned = maps:put(IdMax, Delivery, Spawned),
            loop(NewIdMax, NewSpawned, MonitoredDrones);
        {link, {_FromNode, FromPid}, Id} ->
            io:format("Received link from drone ~p with Pid ~p ~n", [Id, FromPid]),
            Delivery = maps:get(Id, Spawned),
            DeliveryWithPid = maps:put(pid, pid_to_list(FromPid), Delivery),
            sendNewDelivery(DeliveryWithPid),
            NewSpawned = maps:remove(Id, Spawned),

            erlang:monitor(process, FromPid),

            StartX = maps:get(start_x, Delivery),
            StartY = maps:get(start_y, Delivery),
            EndX = maps:get(end_x, Delivery),
            EndY = maps:get(end_y, Delivery),
            State = maps:get(state, Delivery),
            Fallen = maps:get(fallen, Delivery),
            RecoveryFlag = false,
            Policy = fun() -> agreementPolicy() end,
            
            FromPid ! {config, ?VELOCITY, ?DRONE_SIZE, Policy, RecoveryFlag, {StartX, StartY}, {EndX, EndY}, State, Fallen},

            NewMonitoredDrones = maps:put(FromPid, Id, MonitoredDrones),
            io:format("~w ~n", [NewMonitoredDrones]),
            loop(IdMax, NewSpawned, NewMonitoredDrones); 
        {'DOWN', _Ref, process, FromPid, _Reason} ->
            %% TO BE IMPLEMENTED: 
            %% Fault of a drones -> spawn of the new drone
            Id = maps:get(FromPid, MonitoredDrones),
            io:format("Drone ~p crashed.~n A new drone will be spawned to complete its delivery.~n", [Id]),
            loop(IdMax, Spawned, MonitoredDrones);
        {kill, {_FromNode, _FromPid}, Id} ->
            %% TO BE IMPLEMENTED
            io:format("Received a request of kill drone with ID ~p, from the Rest API~n", [Id]),
            loop(IdMax, Spawned, MonitoredDrones);
        _ ->
            loop(IdMax, Spawned, MonitoredDrones)
    end.

sendNewDelivery(Delivery) ->
    Conn = http_utils:createConnection(),
    Resource = "/delivery/",
    Response = http_utils:doPost(Conn, Resource, Delivery),
    io:format("Response: ~p~n", [Response]),
    ok.


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



% spawnLocalDrone si può usare per spawnare i droni in locale per testing
% spawnDrone si può usare per spawanare ogni drone su container diversi

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
            IdMax;
        _ ->
            io:format("Error during attempt to get info from the Rest.~n"),
            io:format("Drone hub will be restarted for another attempt.~n"),
            exit(self(), rest_connection_error)
    end.

%% TODO
agreementPolicy() ->
    ok.

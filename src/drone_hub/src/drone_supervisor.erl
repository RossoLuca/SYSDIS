-module(drone_supervisor).

-export([start_link/0, loop/2, spawnDrone/1]).

start_link() ->
    IdMax = 0,
    SpawnedDrones = #{},
    Pid = spawn_link(?MODULE, loop, [IdMax, SpawnedDrones]),
    register(drone_hub, Pid),
    {ok, Pid}.


loop(IdMax, Spawned) ->
    receive
        {create, {_FromNode, FromPid}, StartX, StartY, EndX, EndY} ->
            io:format("Received a request of spawning a new drone from the Rest API~n"),
            FromPid ! {ack, IdMax},
            spawn(?MODULE, spawnDrone, [IdMax]),
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
            io:format("~w ~n", [NewSpawned]),
            io:format("~w ~n", [NewIdMax]),
            loop(NewIdMax, NewSpawned);
        {link, {_FromNode, FromPid}, Id} ->
            io:format("Received link from drone ~p with Pid ~p ~n", [Id, FromPid]),
            Delivery = maps:get(Id, Spawned),
            DeliveryWithPid = maps:put(pid, pid_to_list(FromPid), Delivery),
            sendNewDelivery(DeliveryWithPid),
            NewSpawned = maps:remove(Id, Spawned),
            loop(IdMax, NewSpawned); 
        {kill, {_FromNode, _FromPid}, Id} ->
            io:format("Received a request of kill drone with ID ~p, from the Rest API~n", [Id]),
            loop(IdMax, Spawned);
        _ ->
            loop(IdMax, Spawned)
    end.

sendNewDelivery(Delivery) ->
    Conn = http_utils:createConnection(),
    Resource = "/delivery/",
    _Response = http_utils:doPost(Conn, Resource, Delivery),
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
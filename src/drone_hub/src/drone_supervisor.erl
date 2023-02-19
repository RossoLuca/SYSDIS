-module(drone_supervisor).

-export([start_link/0, loop/1]).

start_link() ->
    IdMax = 0,
    Pid = spawn_link(?MODULE, loop, [IdMax]),
    register(drone_hub, Pid),
    {ok, Pid}.



loop(IdMax) ->
    receive
        {create, {_FromNode, FromPid}, StartX, StartY, EndX, EndY} ->
            io:format("Received a request of spawning a new drone from the Rest API~n"),
            Id = IdMax,
            FromPid ! {ack, Id},
            State = pending,
            Pid = getPid(Id),
            io:format("~p ~n", [Pid]),
            Delivery = #{
                id => Id,
                state => State,
                pid => Pid,
                start_x => StartX,
                start_y => StartY,
                current_x => StartX,
                current_y => StartY,
                end_x => EndX,
                end_y => EndY,
                fallen => " "    
            },
            sendNewDelivery(Delivery), 
            NewIdMax = IdMax + 1,
            loop(NewIdMax);
        {kill, {_FromNode, _FromPid}, Id} ->
            io:format("Received a request of kill drone with ID ~p, from the Rest API~n", [Id]),
            loop(IdMax);
        _ ->
            loop(IdMax)
    end.


getPid(Id) ->
    Network = "dis_sys",
    ContainerName = "drone_" ++ integer_to_list(Id),
    HostName = integer_to_list(Id) ++ "_host",
    Image = "drone_image",
    Command = "docker -H unix:///var/run/docker.sock run --rm --name " ++ ContainerName ++ 
                    " -h " ++ HostName ++ " --net " ++ Network ++ " " ++ Image,
    io:format("~p ~n", [Command]),
    os:cmd(Command),
    receive
        {link, {_FromNode, FromPid}} ->
            io:format("Received link from drone with Pid ~p ~n", [FromPid]),
            FromPid
    end.

sendNewDelivery(Delivery) ->
    Conn = http_utils:createConnection(),
    Resource = "/delivery/",
    _Response = http_utils:doPost(Conn, Resource, Delivery),
    ok.
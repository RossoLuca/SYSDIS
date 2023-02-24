-module(drone_main).

-export([start_link/0, init/1]).

start_link() ->
    Id = list_to_integer(os:getenv("ID")),
    Pid = spawn(drone_main, init, [Id]),
    register(drone, Pid),
    {ok, Pid}.

init(Id) ->
    {drone_hub, 'drone_hub@drone_hub_host'} ! {link, {node(), self()}, Id},
    receive
        {Velocity,Drone_size,Fun_policy,Recovery} ->
            erlang:display(Velocity),
            erlang:display(Drone_size),
            erlang:display(Recovery),
            case http_utils:createConnection() of
                connection_timed_out -> 
                    io:format("Warning Rest service not reachable");
                Connection -> 
                    Resource = "/delivery/getactivedrones",
                    Response = http_utils:doPost(Connection, Resource, Delivery),
                    Id = utils:printNewDelivery(Response),
                    Id
            end;
            loop(Velocity,Drone_size,Fun_policy,Recovery,#{},#{}).
    

%% ALL THE BUSINESS LOGIC
loop() ->
    
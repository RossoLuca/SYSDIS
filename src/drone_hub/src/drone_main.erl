-module(drone_main).

-export([start_link/0, init/1]).

start_link() ->
    Id = list_to_integer(os:getenv("ID")),
    Pid = spawn(drone_main, init, [Id]),
    %register(drone, Pid),
    {ok, Pid}.

init(Id) ->
    io:format("Drone ~p: sending link message to drone hub~n",[Id]),
    {drone_hub, 'drone_hub@drone_hub_host'} ! {link, {node(), self()}, Id},
    loop().

%% ALL THE BUSINESS LOGIC
loop() ->
    loop().
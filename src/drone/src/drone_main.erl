-module(drone_main).

-export([start_link/0, init/0]).

start_link() ->
    Pid = spawn(drone_main, init, []),
    register(drone, Pid),
    {ok, Pid}.

init() ->
    {drone_hub, 'drone_hub@drone_hub_host'} ! {link, {node(), self()}},
    loop().

%% ALL THE BUSINESS LOGIC
loop() ->
    loop().
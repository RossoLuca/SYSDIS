-module(drone_main).
-define(RETRY_LIMIT,2).
-export([start_link/0, init/1, drone_synchronizer/7]).

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
            Id_to_pid = lists:foldl(fun(Data, Acc0) ->
                            External_Id = maps:get(<<"id">>, Data),
                            if External_Id =/= Id ->
                                Pid = list_to_pid(binary_to_list(maps:get(<<"pid">>, Data))),
                                Acc1 = maps:put(External_Id, Pid, Acc0),
                                Acc1;
                            true ->
                                Acc0
                            end 
                        end, Acc, Response),
            io:format("Drone ~p ---> ~p~n", [Id, Id_to_pid]),

            Size = maps:size(Id_to_pid),
            if Size > 0 ->
                Message = {sync_hello, self(), Id},

                maps:foreach(fun(External_Id, External_Pid) ->
                                    spawn(drone_main, drone_synchronizer, [Id, self(), Route, External_Id, External_Pid, 0, Message])
                            end, Id_to_pid),
                loop(Id, Configuration, DroneState, Id_to_pid);
            true ->
                loop(Id, Configuration, DroneState, Id_to_pid)
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

drone_synchronizer(Id, Pid, Route, External_Id, External_Pid, Retry_count, Message) ->
    External_Pid ! {sync_hello, self(), Id},
    io:format("Drone ~p --> Sent sync_hello message to drone ~p~n", [Id, External_Id]),
    receive
        {sync_routes, External_Id, External_Pid, Start, End, _State, _Ack_count} ->
            io:format("Drone ~p --> Received sync_routes message from drone ~p~n", [Id, External_Id]),
            {Collision_response, Collision_points} = collision_detection(Id, Route, External_Id, {Start, End}),
            Pid ! {Id, Pid, External_Id, External_Pid, Collision_response, Collision_points}
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
                        drone_synchronizer(Id, Pid, Route, External_Id, New_pid, 0, Message)
                end;
            true ->
                drone_synchronizer(Id, Pid, Route, External_Id, External_Pid, Retry_count + 1, Message)
            end
    end.


% %% ALL THE BUSINESS LOGIC
loop(Id, Configuration, DroneState, Id_to_pid) ->
    receive
        {Id, Pid, External_id, External_pid, collision_response, Collision_points} ->
            External_pid ! {Id, Pid, collision_response, Collision_points},
            loop(Id, Configuration, DroneState, maps:remove(External_id, Id_to_pid));

        {sync_hello, From, FromId} ->
            io:format("Drone ~p --> Received sync_hello message from drone ~p~n", [Id, FromId]),
            Start = maps:get(route_start, Configuration),
            End = maps:get(route_end, Configuration),
            State = maps:get(state, DroneState),
            Ack_count = maps:get(ack_count, DroneState),
            From ! {sync_routes, Id, self(), Start, End, State, Ack_count},
            loop(Id, Configuration, DroneState, maps:put(FromId, From, Id_to_pid))
    end.


%% All collision detectionLogic
collision_detection(_Id, _Route, _External_id, _External_route) ->
    ok.

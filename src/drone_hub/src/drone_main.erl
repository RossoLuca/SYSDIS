-module(drone_main).
-define(RETRY_LIMIT,2).
-export([start_link/0, init/1]).

start_link() ->
    Id = list_to_integer(os:getenv("ID")),
    Pid = spawn(drone_main, init, [Id]),
    register(drone, Pid),
    {ok,Pid}.

init(Id) ->
    {drone_hub, 'drone_hub@drone_hub_host'} ! {link, {node(), self()}, Id},
    % receive
    %     {{Route},Velocity,Drone_size,Fun_policy,Recovery} ->
    %         erlang:display(Velocity),
    %         erlang:display(Drone_size),
    %         erlang:display(Recovery),  A REGIME Questi parametri mi arrivano dal drone hub
    Fn = fun() -> 
        io:fwrite("Anonymous Function") 
    end, 
    Route = {{1.0,1.0},{2.0,2.0}},
    case http_utils:createConnection() of
        connection_timed_out -> 
            io:format("Warning Rest service not reachable");
        Connection -> 
            Resource = "/delivery/get_active_drones",
            Response = http_utils:doGet(Connection, Resource),
            erlang:display(Response),
            Message = {synchello,Id,self()},
            List = maps:to_list(Response),
            lists:foreach(fun({External_id,Pid}) -> spawn(drone_main,drone_syncronizer,[Id,self(),Route,External_id,Pid,0],Pid ! Message) end,List),
            loop(Id,Route,1,1,Fn,false,Response)
    end.

drone_syncronizer(Id,Pid,Route,External_id,External_Pid,Retry_count) ->
    receive
        {syncroutes,Id,Pid,{Start_x,Start_y,End_x,End_y},State,Ack_count} ->
            {Collisione_response,Collision_points} = collision_detection(Id,Route,External_id,{{Start_x,Start_y},{End_x,End_y}}),
            Pid ! {Id,Pid,External_id,External_Pid,Collisione_response,Collision_points}
        after 3000 ->
            if Retry_count > ?RETRY_LIMIT ->
                case http_utils:createConnection() of
                    connection_timed_out -> 
                        io:format("Warning Rest service not reachable");
                    Connection -> 
                        Resource = "/delivery/?id=",
                        Query = Resource ++ integer_to_list(External_id),
                        Response = http_utils:doGet(Connection, Query),
                        New_pid = maps:get(<<"pid">>, lists:nth(1,Response)),
                        drone_syncronizer(Id,Pid,External_id,New_pid,0)
                end;
            true ->
                drone_syncronizer(Id,Pid,External_id,External_Pid,Retry_count + 1)
            end
    end.
    

% %% ALL THE BUSINESS LOGIC
loop(Id,Velocity,Drone_size,Fun_policy,Recovery,Id_to_pid) ->
    receive
        {Id,Pid,External_id,External_pid,collision_response,Collision_points } ->
            External_pid ! {Id,Pid,collision_response,Collision_points},
            loop(Id,Velocity,Drone_size,Fun_policy,Recovery,maps:remove(External_id,Id_to_pid),maps:remove(External_pid,Pid_to_id))
    end.


%% All collision detectionLogic
collision_detection(Id,Route,External_id,External_route) ->
    ok.
    
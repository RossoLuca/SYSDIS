-module(drone_sync_fixture).

-export([start_drone/3, drone_synchronizer/8]).


init_drone(Id, SynchronizationMap, Configuration, DroneState) ->
    PersonalCollisions = #{},
    NewDrones = #{},
    ToNotUpdate = sets:new(),
    Route = {utils:get_route_start(Configuration), maps:get(route_end, Configuration)},
    Size = maps:size(SynchronizationMap),
    if Size > 0 ->
        CollisionTable = #{},
        Message = {sync_hello, self(), Id, Route},
        
        maps:foreach(fun(External_Id, Entry) ->
                            External_Pid = maps:get(pid, Entry),
                            spawn(?MODULE, drone_synchronizer, [Id, self(), maps:get(drone_size, Configuration), Route, External_Id, External_Pid, 0, Message])
                    end, SynchronizationMap),
        synchronization:sync_loop(Id, Configuration, DroneState, CollisionTable, SynchronizationMap, NewDrones, PersonalCollisions, ToNotUpdate);
    true ->
        CollisionTable = #{
                Id => #{notify_count => [], collisions => sets:new(), state => pending}
        },
        synchronization:test_table(Id, Configuration, DroneState, CollisionTable, NewDrones, PersonalCollisions, ToNotUpdate)
    end.

drone_synchronizer(Id, MainPid, DroneSize, Route, External_Id, External_Pid, Retry_count, Message) ->
    External_Pid ! {sync_hello, self(), Id, MainPid, Route},
    receive
        {sync_result, External_Pid, External_Id, Collision_response, Collision_points} ->
            MainPid ! {collision_response, External_Pid, External_Id, Collision_response, Collision_points}
        after 3000 ->
            RetryLimit = list_to_integer(os:getenv("RETRY_LIMIT", "2")),
            if Retry_count > RetryLimit ->
                MainPid ! {collision_response, External_Pid, External_Id, no_collision, none};
            true ->
                drone_synchronizer(Id, MainPid, DroneSize, Route, External_Id, External_Pid, Retry_count + 1, Message)
            end
    end.


start_drone(Id, Configuration, PreviousDrones) ->
    SynchronizationMap = lists:foldl(fun(Entry, Acc) ->
                                        {External_Id, Pid} = Entry, 
                                        Map = #{
                                            pid => Pid,
                                            received_result => false,
                                            send_my_table => true
                                        },
                                        maps:put(External_Id, Map, Acc)
                        end, #{}, PreviousDrones),
    DroneState = #{
        state => pending,
        notify_count => [],
        fallen => ""
    },
    init_drone(Id, SynchronizationMap, Configuration, DroneState).
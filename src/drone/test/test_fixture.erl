-module(test_fixture).

-export([register/0, unregister/0, spawn_drones/2]).

register() ->
    register(test, self()).

unregister() -> 
    unregister(test).

spawn_drones(Drones, Fixture) ->
    All = lists:foldl(fun(Map, Spawned) ->
                NewSpawned = maps:fold(fun(Id, Configuration, Acc) ->
                        Pid = spawn(Fixture, start_drone, [Id, Configuration, Spawned]),
                        AccOut = [{Id, Pid} | Acc],
                        AccOut
                    end, [], Map),
                lists:append(Spawned, NewSpawned)
        end, [], Drones),
    All.
    
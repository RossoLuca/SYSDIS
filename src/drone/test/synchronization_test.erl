-module(synchronization_test).
-include_lib("eunit/include/eunit.hrl").


isolated_drones_test() ->
    test_fixture:register(),
    Drones = [
            #{0 => #{
                    route_start => {100.0, 100.0}, 
                    route_end => {100.0, 150.0},
                    drone_size => 1.0,
                    recovery => false
                    }
                }, 
            #{1 => #{
                    route_start => {200.0, 200.0}, 
                    route_end => {150.0, 50.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }, 
            #{2 => #{
                    route_start => {20.0, 60.0}, 
                    route_end => {60.0, 40.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }, 
            #{3 => #{
                    route_start => {100.0, 20.0}, 
                    route_end => {140.0, 20.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            },
            #{4 => #{
                    route_start => {70.0, 50.0}, 
                    route_end => {120.0, 70.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }],
    SpawnedDrones = test_fixture:spawn_drones(Drones, drone_sync_fixture),
    CollisionTables = receive_from_all(SpawnedDrones, #{}),
    CheckCollisionsDrone0 = are_the_same(maps:keys(maps:get(0, CollisionTables)), [0]),
    CheckCollisionsDrone1 = are_the_same(maps:keys(maps:get(1, CollisionTables)), [1]),
    CheckCollisionsDrone2 = are_the_same(maps:keys(maps:get(2, CollisionTables)), [2]),
    CheckCollisionsDrone3 = are_the_same(maps:keys(maps:get(3, CollisionTables)), [3]),
    CheckCollisionsDrone4 = are_the_same(maps:keys(maps:get(4, CollisionTables)), [4]),
    test_fixture:unregister(),
    {timeout, 5, ?_assert(CheckCollisionsDrone0 and CheckCollisionsDrone1 and CheckCollisionsDrone2 and CheckCollisionsDrone3 and CheckCollisionsDrone4)}.

all_at_least_one_collision_test() -> 
    test_fixture:register(),
    Drones = [
            #{0 => #{
                    route_start => {10.0, 10.0}, 
                    route_end => {30.0, 15.0},
                    drone_size => 1.0,
                    recovery => false
                    }
                }, 
            #{1 => #{
                    route_start => {12.0, 30.0}, 
                    route_end => {22.0, 5.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }, 
            #{2 => #{
                    route_start => {40.0, 35.0}, 
                    route_end => {70.0, 10.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }, 
            #{3 => #{
                    route_start => {10.0, 2.0}, 
                    route_end => {60.0, 50.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }],
    SpawnedDrones = test_fixture:spawn_drones(Drones, drone_sync_fixture),
    CollisionTables = receive_from_all(SpawnedDrones, #{}),
    CheckCollisionsDrone0 = are_the_same(maps:keys(maps:get(0, CollisionTables)), [0, 1, 3]),
    CheckCollisionsDrone1 = are_the_same(maps:keys(maps:get(1, CollisionTables)), [0, 1, 3]),
    CheckCollisionsDrone2 = are_the_same(maps:keys(maps:get(2, CollisionTables)), [2, 3]),
    CheckCollisionsDrone3 = are_the_same(maps:keys(maps:get(3, CollisionTables)), [0, 1, 2, 3]),
    test_fixture:unregister(),
    {timeout, 5, ?_assert(CheckCollisionsDrone0 and CheckCollisionsDrone1 and CheckCollisionsDrone2 and CheckCollisionsDrone3)}.


five_drones_test() ->
    test_fixture:register(),
    Drones = [
            #{0 => #{
                    route_start => {10.0, 10.0}, 
                    route_end => {60.0, 60.0},
                    drone_size => 1.0,
                    recovery => false
                    }
                }, 
            #{1 => #{
                    route_start => {15.0, 20.0}, 
                    route_end => {30.0, 5.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }, 
            #{2 => #{
                    route_start => {30.0, 40.0}, 
                    route_end => {70.0, 35.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }, 
            #{3 => #{
                    route_start => {60.0, 20.0}, 
                    route_end => {42.0, 55.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            },
            #{4 => #{
                    route_start => {100.0, 100.0}, 
                    route_end => {120.0, 75.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }],
    SpawnedDrones = test_fixture:spawn_drones(Drones, drone_sync_fixture),
    CollisionTables = receive_from_all(SpawnedDrones, #{}),
    CheckCollisionsDrone0 = are_the_same(maps:keys(maps:get(0, CollisionTables)), [0, 1, 2, 3]),
    CheckCollisionsDrone1 = are_the_same(maps:keys(maps:get(1, CollisionTables)), [0, 1]),
    CheckCollisionsDrone2 = are_the_same(maps:keys(maps:get(2, CollisionTables)), [0, 2, 3]),
    CheckCollisionsDrone3 = are_the_same(maps:keys(maps:get(3, CollisionTables)), [0, 2, 3]),
    CheckCollisionsDrone4 = are_the_same(maps:keys(maps:get(4, CollisionTables)), [4]),
    test_fixture:unregister(),
    {timeout, 5, ?_assert(CheckCollisionsDrone0 and CheckCollisionsDrone1 and CheckCollisionsDrone2 and CheckCollisionsDrone3 and CheckCollisionsDrone4)}.

four_drones_in_deadlock_test() ->
    test_fixture:register(),
    Drones = [
            #{0 => #{
                    route_start => {20.0, 20.0}, 
                    route_end => {40.0, 40.0},
                    drone_size => 1.0,
                    recovery => false
                    }
                }, 
            #{1 => #{
                    route_start => {35.0, 40.0}, 
                    route_end => {60.0, 15.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }, 
            #{2 => #{
                    route_start => {65.0, 20.0}, 
                    route_end => {36.0, 4.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }, 
            #{3 => #{
                    route_start => {50.0, 5.0}, 
                    route_end => {18.0, 28.0},
                    drone_size => 1.0,
                    recovery => false
                    }
            }],
    SpawnedDrones = test_fixture:spawn_drones(Drones, drone_sync_fixture),
    CollisionTables = receive_from_all(SpawnedDrones, #{}),
    CheckCollisionsDrone0 = are_the_same(maps:keys(maps:get(0, CollisionTables)), [0, 1, 3]),
    CheckCollisionsDrone1 = are_the_same(maps:keys(maps:get(1, CollisionTables)), [0, 1, 2]),
    CheckCollisionsDrone2 = are_the_same(maps:keys(maps:get(2, CollisionTables)), [1, 2, 3]),
    CheckCollisionsDrone3 = are_the_same(maps:keys(maps:get(3, CollisionTables)), [0, 2, 3]),
    test_fixture:unregister(),
    {timeout, 15, ?_assert(CheckCollisionsDrone0 and CheckCollisionsDrone1 and CheckCollisionsDrone2 and CheckCollisionsDrone3)}.


receive_from_all(Drones, Received) ->
    receive
        {collision_table, FromId, FromCollisionTable} ->
            NewReceived = maps:put(FromId, FromCollisionTable, Received),
            Size = maps:size(NewReceived),
            if Size == length(Drones) ->
                NewReceived;
            true ->
                receive_from_all(Drones, NewReceived)
            end
    end.

are_the_same(List1, List2) ->
    lists:sort(List1) =:= lists:sort(List2).
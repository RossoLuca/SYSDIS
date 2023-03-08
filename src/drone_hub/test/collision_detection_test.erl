-module(collision_detection_test).
-include_lib("eunit/include/eunit.hrl").


parallel_routes_not_in_collision_test() ->
    DroneSize = 1.0,
    Route0 = {{10.0, 10.0}, {50.0, 50.0}},
    Route1 = {{55.0, 35.0}, {65.0, 45.0}},
    {CollisionResult, Points} = collision_detection:compute_collision(DroneSize, 0, Route0, 1, Route1),
    ?assertEqual(CollisionResult, no_collision),
    ?assertEqual(Points, none).

parallel_routes_not_in_collision2_test() ->
    DroneSize = 1.0,
    Route0 = {{20.0, 20.0}, {60.0, 60.0}},
    Route1 = {{70.0, 70.0}, {80.0, 80.0}},
    {CollisionResult, Points} = collision_detection:compute_collision(DroneSize, 0, Route0, 1, Route1),
    ?assertEqual(CollisionResult, no_collision),
    ?assertEqual(Points, none).

parallel_routes_in_collision_test() ->
    DroneSize = 1.0,
    Route0 = {{10.0, 10.0}, {50.0, 50.0}},
    Route1 = {{29.5, 30.5}, {69.5, 70.5}},
    {CollisionResult, Points} = collision_detection:compute_collision(DroneSize, 0, Route0, 1, Route1),
    IntersectionPointFor0 = maps:get(0, Points),
    IntersectionPointFor1 = maps:get(1, Points),
    ?assertEqual(CollisionResult, collision),
    ?assertEqual(IntersectionPointFor0, {30.0, 30.0}),
    ?assertEqual(IntersectionPointFor1, {49.5, 50.5}).

no_collision_routes_test() ->
    DroneSize = 1.0,
    Route0 = {{100.6, 52.3}, {201.0, 102.5}},
    Route1 = {{200.6, 20.6}, {250.0, 25.0}},
    {CollisionResult, Points} = collision_detection:compute_collision(DroneSize, 0, Route0, 1, Route1),
    ?assertEqual(CollisionResult, no_collision),
    ?assertEqual(Points, none).


intersected_routes_test() ->
    DroneSize = 1.0,
    Route0 = {{200.0, 20.0}, {250.0, 25.0}},
    Route1 = {{200.0, 25.0}, {220.0, 15.0}},
    {CollisionResult, Points} = collision_detection:compute_collision(DroneSize, 0, Route0, 1, Route1),
    {PointFor0_x, PointFor0_y} = maps:get(0, Points),
    {PointFor1_x, PointFor1_y} = maps:get(1, Points),
    IntersectionPointFor0 = {list_to_float(io_lib:format("~.3f", [PointFor0_x])), list_to_float(io_lib:format("~.3f", [PointFor0_y]))},
    IntersectionPointFor1 = {list_to_float(io_lib:format("~.3f", [PointFor1_x])), list_to_float(io_lib:format("~.3f", [PointFor1_y]))},
    InterserctionPoint = {208.333, 20.833},
    ?assertEqual(CollisionResult, collision),
    ?assertEqual(IntersectionPointFor0, InterserctionPoint),
    ?assertEqual(IntersectionPointFor1, InterserctionPoint).



proximity_colliding_routes_test() ->
    DroneSize = 1.0,
    Route0 = {{20.0, 20.0}, {60.0, 60.0}},
    Route1 = {{30.0, 22.0}, {55.5, 54.5}},
    {CollisionResult, Points} = collision_detection:compute_collision(DroneSize, 0, Route0, 1, Route1),
    IntersectionPointFor0 = maps:get(0, Points),
    IntersectionPointFor1 = maps:get(1, Points),
    ?assertEqual(CollisionResult, collision),
    ?assertEqual(IntersectionPointFor0, {55.0, 55.0}),
    ?assertEqual(IntersectionPointFor1, {55.5, 54.5}).

proximity_colliding_endpoints_test() ->
    DroneSize = 1.0,
    Route0 = {{20.0, 20.0}, {60.0, 60.0}},
    Route1 = {{60.2, 60.2}, {100.0, 20.0}},
    {CollisionResult, Points} = collision_detection:compute_collision(DroneSize, 0, Route0, 1, Route1),
    IntersectionPointFor0 = maps:get(0, Points),
    IntersectionPointFor1 = maps:get(1, Points),
    ?assertEqual(CollisionResult, collision),
    ?assertEqual(IntersectionPointFor0, {60.0, 60.0}),
    ?assertEqual(IntersectionPointFor1, {60.2, 60.2}).
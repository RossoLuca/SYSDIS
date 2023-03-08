-module(policy_test).
-include_lib("eunit/include/eunit.hrl").


order_on_number_of_collisions_test() ->
    CollisionTable = collision_table_fixture:with_different_collisions(),
    Order = drone_supervisor:agreementPolicy(CollisionTable),
    ?assertEqual(Order, [4, 1, 2, 3, 0]).

order_on_notify_count_test() ->
    CollisionTable = collision_table_fixture:with_starvation(),
    Order = drone_supervisor:agreementPolicy(CollisionTable),
    ?assertEqual(Order, [0, 6, 7, 8, 9]).


order_on_ids_test() ->
    CollisionTable = collision_table_fixture:with_equal_number_of_collisions(),
    Order = drone_supervisor:agreementPolicy(CollisionTable),
    ?assertEqual(Order, [0, 1, 2, 3, 4]).


order_on_drone_state_test() ->
    CollisionTable = collision_table_fixture:with_a_flying_drone(),
    Order = drone_supervisor:agreementPolicy(CollisionTable),
    ?assertEqual(Order, [0, 4, 1, 2, 3]).
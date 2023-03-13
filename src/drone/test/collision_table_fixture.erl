-module(collision_table_fixture).
-export([with_different_collisions/0, with_starvation/0, with_equal_number_of_collisions/0, with_a_flying_drone/0]).


with_different_collisions() ->
    CollisionTable = #{
        0 => #{
            collisions => sets:from_list([1, 2, 3]),
            notify_count => [],
            state => pending 
        },
        1 => #{
            collisions => sets:from_list([0]),
            notify_count => [],
            state => pending
        },
        2 => #{
            collisions => sets:from_list([0, 3]),
            notify_count => [],
            state => pending
        },
        3 => #{
            collisions => sets:from_list([0, 2]),
            notify_count => [],
            state => pending
        },
        4 => #{
            collisions => sets:from_list([]),
            notify_count => [],
            state => pending
        }
    },
    CollisionTable.


with_starvation() ->
    CollisionTable = #{
        0 => #{
            collisions => sets:from_list([6, 8, 9, 7]),
            notify_count => [1, 2, 3, 4],
            state => pending 
        },
        6 => #{
            collisions => sets:from_list([0]),
            notify_count => [],
            state => pending
        },
        7 => #{
            collisions => sets:from_list([0]),
            notify_count => [],
            state => pending
        },
        8 => #{
            collisions => sets:from_list([0]),
            notify_count => [],
            state => pending
        },
        9 => #{
            collisions => sets:from_list([0]),
            notify_count => [],
            state => pending
        }
    },
    CollisionTable.

with_equal_number_of_collisions() ->
    CollisionTable = #{
        0 => #{
            collisions => sets:from_list([1, 2, 3, 4]),
            notify_count => [],
            state => pending 
        },
        1 => #{
            collisions => sets:from_list([0, 2, 3, 4]),
            notify_count => [],
            state => pending
        },
        2 => #{
            collisions => sets:from_list([1, 0, 3, 4]),
            notify_count => [],
            state => pending
        },
        3 => #{
            collisions => sets:from_list([1, 2, 0, 4]),
            notify_count => [],
            state => pending
        },
        4 => #{
            collisions => sets:from_list([1, 2, 3, 0]),
            notify_count => [],
            state => pending
        }
    },
    CollisionTable.


with_a_flying_drone() ->
    CollisionTable = #{
        0 => #{
            collisions => sets:from_list([1, 2, 3]),
            notify_count => [],
            state => flying 
        },
        1 => #{
            collisions => sets:from_list([0]),
            notify_count => [],
            state => pending
        },
        2 => #{
            collisions => sets:from_list([0, 3]),
            notify_count => [],
            state => pending
        },
        3 => #{
            collisions => sets:from_list([0, 2]),
            notify_count => [],
            state => pending
        },
        4 => #{
            collisions => sets:from_list([]),
            notify_count => [],
            state => pending
        }
    },
    CollisionTable.
-module(utils).

-export([printDelivery/1, printNewDelivery/1, toMap/1]).

printDelivery(Delivery) -> 
    io:format("Delivery: ~p~n", [maps:get(<<"id">>, Delivery)]),    
    io:format("\tState: ~p~n", [binary_to_atom(maps:get(<<"state">>, Delivery))]),
    io:format("\tStart position on x axes: ~p~n", [maps:get(<<"start_x">>, Delivery)]),
    io:format("\tStart position on y axes: ~p~n", [maps:get(<<"start_y">>, Delivery)]),
    io:format("\tCurrent position on x axes: ~p~n", [maps:get(<<"current_x">>, Delivery)]),
    io:format("\tCurrent position on y axes: ~p~n", [maps:get(<<"current_y">>, Delivery)]),
    io:format("\tFinal delivery position on x axes: ~p~n", [maps:get(<<"end_x">>, Delivery)]),
    io:format("\tFinal delivery position on y axes: ~p~n", [maps:get(<<"end_y">>, Delivery)]),
    Fallen = maps:get(<<"fallen">>, Delivery),
    if Fallen =/= " " ->
        printFallen(Fallen);
    true ->
        io:format("\tFallen: ~p~n", [maps:get(<<"fallen">>, Delivery)])
    end.

printFallen(Fallen) ->
    io:format("\tFallen: "),
    TimestampList = string:tokens(Fallen, ";"),
    lists:foreach(fun(Timestap) -> 
            {{_, _, _}, {H, M, S}} = calendar:system_time_to_local_time(list_to_integer(Timestap), 1000000),
            io:format("~p:~p:~p\t", [H, M, S])
        end,TimestampList),
    io:format("~n").

printNewDelivery(Delivery) ->
    Id = maps:get(<<"id">>, Delivery),
    io:format("A new delivery with ID ~p has been inserted in the system.~n", [Id]),
    Id.

toMap(Delivery) ->
    Del = #{
        id => maps:get(<<"id">>, Delivery),
        state => binary_to_atom(maps:get(<<"state">>, Delivery)),
        start_x => maps:get(<<"start_x">>, Delivery),
        start_y => maps:get(<<"start_y">>, Delivery),
        current_x => maps:get(<<"current_x">>, Delivery),
        current_y => maps:get(<<"current_y">>, Delivery),
        end_x => maps:get(<<"end_x">>, Delivery),
        end_y => maps:get(<<"end_y">>, Delivery),
        fallen => maps:get(<<"fallen">>, Delivery)
    },
    Del.
-module(utils).

-export([printDelivery/1, toMap/1]).

printDelivery({Delivery}) -> 
    PrintableDel = #{},
    Del = lists:foldl(fun({K, V}, Acc) ->  
                    Key = binary_to_atom(K),
                    Value = case is_integer(V) or is_float(V) of
                                true -> V;
                                false -> binary_to_atom(V)
                            end,
                    %io:format("~p --> ~p ~n", [Key, Value])
                    NewAcc = Acc#{Key => Value},
                    NewAcc
        end, PrintableDel, Delivery),
    
    io:format("Delivery: ~p~n", [maps:get(id, Del)]),
    io:format("\tState: ~p~n", [maps:get(stato, Del)]),
    io:format("\tStart position on x axes: ~p~n", [maps:get(start_x, Del)]),
    io:format("\tStart position on y axes: ~p~n", [maps:get(start_y, Del)]),
    io:format("\tCurrent position on x axes: ~p~n", [maps:get(current_x, Del)]),
    io:format("\tCurrent position on y axes: ~p~n", [maps:get(current_y, Del)]),
    io:format("\tFinal delivery position on x axes: ~p~n", [maps:get(end_x, Del)]),
    io:format("\tFinal delivery position on y axes: ~p~n", [maps:get(end_y, Del)]).


toMap(Delivery) ->
    MapAcc = #{},
    Map = lists:foldl(fun({K, V}, Acc) ->  
                    Key = binary_to_atom(K),
                    Value = case is_integer(V) or is_float(V) of
                                true -> V;
                                false -> binary_to_atom(V)
                            end,
                    NewAcc = Acc#{Key => Value},
                    NewAcc
        end, MapAcc, Delivery),
    Map.
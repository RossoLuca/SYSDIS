-module(terminal).

-export([insert_delivery/4, insert_and_monitor_delivery/4, get_delivery/1, watch_delivery/2, watch_delivery/1 , kill_drone/1]).

insert_delivery(StartX, StartY, EndX, EndY) when is_float(StartX), is_float(StartY), is_float(EndX), is_float(EndY) -> 

    Delivery = #{
        start_x => StartX,
        start_y => StartY,
        end_x => EndX,
        end_y => EndY
    },

    case http_utils:createConnection() of
        connection_timed_out -> 
            io:format("At the moment the delivery cannot be inserted to the system.~nTry again later!~n");
        Connection -> 
            Resource = "/delivery/insert",
            case http_utils:doPost(Connection, Resource, Delivery) of
                {error, timeout} ->
                    io:format("At the moment the delivery cannot be inserted to the system.~nTry again later!~n");
                Response ->
                    Id = utils:printNewDelivery(Response),
                    Id
            end
    end;

insert_delivery(_StartX, _StartY, _EndX, _EndY) ->
    io:format("All the coordinates must be real numbers!").


insert_and_monitor_delivery(StartX, StartY, EndX, EndY) when is_float(StartX), is_float(StartY), is_float(EndX), is_float(EndY) ->
    Delivery = #{
        start_x => StartX,
        start_y => StartY,
        end_x => EndX,
        end_y => EndY
    },

    case http_utils:createConnection() of
        connection_timed_out -> 
            io:format("At the moment the delivery cannot be inserted to the system.~nTry again later!~n");
        Connection -> 
            Resource = "/delivery/insert",
            case http_utils:doPost(Connection, Resource, Delivery) of
                {error, timeout} ->
                    io:format("At the moment the delivery cannot be inserted to the system.~nTry again later!~n");
                Response ->
                    Id = utils:printNewDelivery(Response),
                    watch_delivery(Id)
            end
    end;

insert_and_monitor_delivery(_StartX, _StartY, _EndX, _EndY) ->
    io:format("All the coordinates must be real numbers!").

get_delivery(Id) when is_integer(Id) -> 
    case http_utils:createConnection() of
        connection_timed_out ->
            io:format("At the moment the research cannot be completed.~nTry again later!~n");
        Connection ->
            Resource = "/delivery/?id=",
            Query = Resource ++ integer_to_list(Id),
            case http_utils:doGet(Connection, Query) of 
                {error, timeout} ->
                    io:format("At the moment the research cannot be completed.~nTry again later!~n");
                Response ->
                    case length(Response) of
                        0 -> 
                            io:format("Delivery with ID ~p not found! ~n", [Id]);
                        _ ->
                            utils:printDelivery(lists:nth(1, Response))
                    end
                end
    end;

get_delivery(_Id) -> 
    io:format("Id must be integer!~n", []).



watch_delivery(Id, Timeout) when is_integer(Timeout), is_integer(Id) ->
    case http_utils:createConnection() of
        connection_timed_out ->
            io:format("At the moment the task cannot be started.~nTry again later!~n");
        Connection ->
            spawn(watcher, start, [Id, Timeout, Connection])
    end;

watch_delivery(_Id, _Timeout) ->
    io:format("Id and Timeout must both be integer!~n").


watch_delivery(Id) ->
    WatchTimeout = os:getenv("WATCH_DEFAULT_TIMEOUT", undefined),
    if WatchTimeout == undefined ->
        io:format("Watch default timeout is not defined. Please define it inside the Dockerfile, or indicate a timeout as a second parameter.~n");
    true ->
        watch_delivery(Id, list_to_integer(WatchTimeout))
    end.


kill_drone(Id) when is_integer(Id) -> 
    case http_utils:createConnection() of
        connection_timed_out ->
            io:format("At the moment the drone cannot be killed.~nTry again later!~n");
        Connection ->
            Resource = "/delivery/kill/?id=",
            Query = Resource ++ integer_to_list(Id),
            case http_utils:doGet(Connection, Query) of 
                {error, timeout} -> 
                    io:format("At the moment the drone cannot be killed.~nTry again later!~n");
                Response ->                            
                    Result = maps:get(<<"result">>, Response, error),
                    if Result =/= error ->
                        io:format("Drone dealing with the delivery ~p has been killed.~n", [Id]);
                    true ->
                        io:format("Delivery ~p was not found.~n", [Id])
                    end
            end
    end;

kill_drone(_) ->
    io:format("Id must be integer!~n").
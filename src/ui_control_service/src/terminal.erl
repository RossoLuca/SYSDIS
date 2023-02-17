-module(terminal).

-export([insert_delivery/4, get_delivery/1, watch_delivery/2, kill_drone/1]).

%% TODO: To be completed
insert_delivery(StartX, StartY, EndX, EndY) when is_float(StartX), is_float(StartY), is_float(EndX), is_float(EndY) -> 
    %% Possono essere definite dalla Rest API
    %% _State = pending,    
    % CurrentX = StartX,
    % CurrentY = StartY,

    Delivery = #{
        start_x => StartX,
        start_y => StartY,
        end_x => EndX,
        end_y => EndY},


    case http_utils:createConnection() of
        connection_timed_out -> 
            io:format("At the moment the delivery cannot be inserted to the system.~nTry again later!~n");
        Connection -> 
            Resource = "/delivery/",
            http_utils:doPost(Connection, Resource, Delivery),
            %% Va definita sul rest route per l'inserimento di una nuova delivery
            %% che riceve solo StartX, StartY, EndX, EndY
            ok
    end;

insert_delivery(_StartX, _StartY, _EndX, _EndY) ->
    io:format("All the coordinates must be real numbers!").


get_delivery(Id) when is_integer(Id) -> 
    case http_utils:createConnection() of
        connection_timed_out ->
            io:format("At the moment the research cannot be completed.~nTry again later!~n");
        Connection ->
            Resource = "/delivery/?id=",
            Query = Resource ++ integer_to_list(Id),
            Response = http_utils:doGet(Connection, Query),

            case length(Response) of
                0 -> 
                    io:format("Delivery with ID ~p not found! ~n", [Id]);
                _ ->
                    utils:printDelivery(lists:nth(1, Response))
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


%% TODO: To be implemented
kill_drone(_Id) -> 
    %% Invio di richiesta /kill/?id=Id
    ok.
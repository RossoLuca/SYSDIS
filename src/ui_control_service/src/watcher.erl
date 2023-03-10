-module(watcher).

-export([start/3]).

start(Id, Timeout, Connection) -> 
    io:format("Started monitoring of the delivery ~p ~n", [Id]),
    State = #{state => unknown,
        current_x => unknown,
        current_y => unknown,
        end_x => unknown,
        end_y => unknown,
        fallen => unknown},
    loop(Id, Timeout, Connection, State).

loop(Id, Timeout, Conn, OldState) -> 
    Resource = "/delivery/?id=",
    Query = Resource ++ integer_to_list(Id),
    case http_utils:doGet(Conn, Query) of
        {error, timeout} ->
            io:format("Rest API isn't reachable at the moment. Watcher process for the delivery ~p will now be closed.~nTry again later to watch the delivery", [Id]);
        Response ->
            if length(Response) > 0 ->
                Delivery = lists:nth(1, Response),
                Del = utils:toMap(Delivery),

                NewState = updateState(Id, OldState, Del),
                State = maps:get(state, NewState),

                if State == completed ->
                    io:format("The drone assigned to the delivery ~p has arrived at the final point (~p, ~p) ~n", [Id, maps:get(end_x, NewState), maps:get(end_y, NewState)]);
                true ->
                    receive
                        after Timeout -> ok
                    end,
                    loop(Id, Timeout, Conn, NewState)
                end;
            true ->
                receive
                        after Timeout -> ok
                end,
                loop(Id, Timeout, Conn, OldState)
            end
    end.

updateState(Id, OldState, CurrentState) ->
    St = maps:get(state, OldState),
    if St == unknown -> 
        %% This case happens only at the first call of this function by the watcher
        %% (i.e. when the state is unknown)
        NewState = #{state => maps:get(state, CurrentState),
            current_x => maps:get(current_x, CurrentState),
            current_y => maps:get(current_y, CurrentState),
            end_x => maps:get(end_x, CurrentState),
            end_y => maps:get(end_y, CurrentState),
            fallen => maps:get(fallen, CurrentState)},
        NewState;
    true -> 
        %% This case compare the old state with the current received from the Rest API
        NewState = compare(Id, OldState, CurrentState),
        NewState
    end.

compare(Id, Old, Current) -> 
    Old_state = maps:get(state, Old),
    Current_state = maps:get(state, Current),

    {New_current_x, New_current_y} = update_current_position({maps:get(current_x, Old), maps:get(current_y, Old)},
             {maps:get(current_x, Current), maps:get(current_y, Current)}),
    NewState = #{
        state => update_stato(Id, Old_state, Current_state),
        current_x => New_current_x,
        current_y => New_current_y,
        end_x => maps:get(end_x, Old),
        end_y => maps:get(end_y, Old),
        fallen => check_drone_fall(Id, maps:get(fallen, Old), maps:get(fallen, Current))
    },
    NewState.

update_stato(Id, Old, Current) ->
    if Old =/= Current ->
        io:format("The drone that is dealing with the delivery ~p is now in state ~p~n", [Id, Current]),
        Current;
    true ->
        Old
    end.

update_current_position({OldX, OldY}, {CurrX, CurrY}) ->
    if OldX =/= CurrX; OldY =/= CurrY ->
        {CurrX, CurrY};
    true ->
        {OldX, OldY}
    end.

check_drone_fall(Id, Old, Current) ->
    if Old =/= Current ->
        TimestampList = string:tokens(Current, ";"),
        LastTime = list_to_integer(lists:last(TimestampList)),
        {{_Year, _Month, _Day}, {Hours, Minutes, Seconds}} = calendar:system_time_to_local_time(LastTime, 1000000),
        io:format("## WARNING! ##~nThe drone assigned to the the delivery ~p has fall at time ~p:~p:~p.~nThe delivery will now be completed from a new drone.~n", [Id, Hours, Minutes, Seconds]),
        Current;
    true ->
        Old
    end.
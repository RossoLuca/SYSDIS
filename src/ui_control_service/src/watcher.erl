-module(watcher).

-export([start/3]).

start(Id, Timeout, Connection) -> 
    io:format("Started monitoring of the delivery ~p ~n", [Id]),
    State = #{stato => unknown,
        current_x => unknown,
        current_y => unknown,
        end_x => unknown,
        end_y => unknown},
    loop(Id, Timeout, Connection, State).

loop(Id, Timeout, Conn, OldState) -> 
    Resource = "/delivery/?id=",
    Query = Resource ++ integer_to_list(Id),
    {Delivery} = lists:nth(1, http_utils:doGet(Conn, Query)),
    Del = utils:toMap(Delivery),

    NewState = updateState(Id, OldState, Del),
    State = maps:get(stato, NewState),

    if State == completed ->
        io:format("The delivery ~p has arrived at the final point (~p, ~p) ~n", [Id, maps:get(end_x, NewState), maps:get(end_y, NewState)]);
    true ->
        receive
            after Timeout -> ok
        end,
        loop(Id, Timeout, Conn, NewState)
    end.


updateState(Id, OldState, CurrentState) ->
    St = maps:get(stato, OldState),
    if St == unknown -> 
        %% This case happens only at the first call of this function by the watcher
        %% (i.e. when the state is unknown)
        NewState = #{stato => maps:get(stato, CurrentState),
            current_x => maps:get(current_x, CurrentState),
            current_y => maps:get(current_y, CurrentState),
            end_x => maps:get(end_x, CurrentState),
            end_y => maps:get(end_y, CurrentState)},
        NewState;
    true -> 
        %% This case compare the old state with the current received from the Rest API
        NewState = compare(Id, OldState, CurrentState),
        NewState
    end.

compare(Id, Old, Current) -> 
    Old_stato = maps:get(stato, Old),
    Current_stato = maps:get(stato, Current),

    {New_current_x, New_current_y} = update_current_position({maps:get(current_x, Old), maps:get(current_y, Old)},
             {maps:get(current_x, Current), maps:get(current_y, Current)}),
    NewState = #{
        stato => update_stato(Id, Old_stato, Current_stato),
        current_x => New_current_x,
        current_y => New_current_y,
        end_x => maps:get(end_x, Old),
        end_y => maps:get(end_y, Old)
    },
    NewState.

update_stato(Id, Old, Current) ->
    if Old =/= Current ->
        io:format("The delivery ~p is now in state ~p~n", [Id, Current]),
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
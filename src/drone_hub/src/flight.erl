-module(flight).

-export([init/8, compute_slope/2]).


init(Id, Height, Start, End, Velocity, DroneSize, ToBeAcked, MainPid) ->
    ToBeAdded = DroneSize/2,

    {Start_x, Start_y} = Start,
    {End_x, End_y} = End,

    if Start_x == End_x ->
        %% This means that the route is vertical
        %% So, the slope is of pi/2
        %% ==> cos(pi/2) = 0, sin(pi/2) = 1
        Step_x = 0,
        Step_y = Velocity,

        Sign = case {Start_y, End_y} of
            {S, E} when S < E -> 1;
            {_, _} -> -1
        end,
        Points = lists:reverse(compute_points_with_y(Start, End, Step_x, Step_y, [], Sign)),
        PointsWithEnd = lists:append(Points, [{End_x, End_y, End_x, End_y, normal}]),
        AckPoints = lists:map(fun({X, Y}) -> 
                                {NextPoint_x, NextPoint_y} = compute_ack_point_y(X, Y, (math:pi()/2), Sign, ToBeAdded), 
                                {NextPoint_x, NextPoint_y, X, Y, ack} 
                            end, ToBeAcked),
        TotalPoints = lists:append(PointsWithEnd, AckPoints),
        SortedPoints = lists:sort(fun({RealX_a, RealY_a, _X_a, _Y_a, _Type_a}, {RealX_b, RealY_b, _X_b, _Y_b, _Type_b}) ->
            DistanceA_fromStart = geometry_utils:distancePointPoint({Start_x, Start_y}, {RealX_a, RealY_a}),
            DistanceB_fromStart = geometry_utils:distancePointPoint({Start_x, Start_y}, {RealX_b, RealY_b}),
            if DistanceA_fromStart < DistanceB_fromStart ->
                true;
            true ->
                false
            end
        end, TotalPoints),
        {Times, _LastPoint} = lists:mapfoldl(fun({NextX, NextY, RealX, RealY, Type}, {P_x, P_y}) -> 
            DeltaS = geometry_utils:distancePointPoint({P_x, P_y}, {NextX, NextY}),
            DeltaT = DeltaS / Velocity,
            {{NextX, NextY, RealX, RealY, Type, DeltaT}, {NextX, NextY}} 
        end, Start, SortedPoints),
        start_deliver(Id, (Height/Velocity), Times, MainPid);
    true ->
        %% In this case the slope of the route must be computed from the angular coefficent
        %% of the equation relative to the route
        Angle = compute_slope(Start, End),
        
        Step_x = math:cos(Angle) * (Velocity),
        Step_y = math:sin(Angle) * (Velocity),
        Sign = case {Start_x, End_x} of
                    {S, E} when S < E -> 1;
                    {_, _} -> -1
                end,
        Points = lists:reverse(compute_points_with_x(Start, End, Step_x, Step_y, [], Sign)),
        PointsWithEnd = lists:append(Points, [{End_x, End_y, End_x, End_y, normal}]),
        AckPoints = lists:map(fun({X, Y}) -> 
                                {NextPoint_x, NextPoint_y} = compute_ack_point(X, Y, Angle, Sign, ToBeAdded), 
                                {NextPoint_x, NextPoint_y, X, Y, ack} 
                            end, ToBeAcked),
        TotalPoints = lists:append(PointsWithEnd, AckPoints),
        SortedPoints = lists:sort(fun({RealX_a, RealY_a, _X_a, _Y_a, _Type_a}, {RealX_b, RealY_b, _X_b, _Y_b, _Type_b}) ->
            DistanceA_fromStart = geometry_utils:distancePointPoint({Start_x, Start_y}, {RealX_a, RealY_a}),
            DistanceB_fromStart = geometry_utils:distancePointPoint({Start_x, Start_y}, {RealX_b, RealY_b}),
            if DistanceA_fromStart < DistanceB_fromStart ->
                true;
            true ->
                false
            end
        end, TotalPoints),
        {Times, _LastPoint} = lists:mapfoldl(fun({NextX, NextY, RealX, RealY, Type}, {P_x, P_y}) -> 
            DeltaS = geometry_utils:distancePointPoint({P_x, P_y}, {NextX, NextY}),
            DeltaT = DeltaS / Velocity,
            {{NextX, NextY, RealX, RealY, Type, DeltaT}, {NextX, NextY}} 
        end, Start, SortedPoints),
        start_deliver(Id, (Height/Velocity), Times, MainPid)
    end.

start_deliver(Id, HeightTime, Times, MainPid) ->
    {StartX, StartY, RealStartX, RealStartY, _, _} = lists:nth(1, Times),
    {EndX, EndY, RealEndX, RealEndY, _, _} = lists:nth(length(Times), Times),

    TakingOff = {StartX, StartY, RealStartX, RealStartY, taking_off, HeightTime},
    Landing = {EndX, EndY, RealEndX, RealEndY, landing, HeightTime},

    NewTimes = lists:append([TakingOff | Times], [Landing]),

    Started = get_timestamp(),
    io:format("Drone ~p --> Started to fly at time: ~p~n", [Id, Started]),
    update_position(Id, NewTimes, MainPid),
    Arrived = get_timestamp(),
    io:format("Drone ~p --> Arrived at final position at time: ~p~n", [Id, Arrived]),
    io:format("Drone ~p --> Total travel time: ~p~n", [Id, ((Arrived-Started)/1000)]).

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).


update_position(Id, Times, MainPid) ->
    lists:foreach(fun({NextX, NextY, RealX, RealY, Type, T}) -> 
                receive 
                    after round(T * 1000) ->
                        MainPid ! {update_position, self(), Id, NextX, NextY, RealX, RealY, Type}
                end
            end, Times).


%% Each element is of the form NextPoint_x, NextPoint_y, RealNexPoint_x, RealNextPoint_y
%% This allow to manage the bounding box size when dealing with ack points
%% In the case of normal points, NextPoint and RealNextPoint is equal, while in the case
%% of ack points NextPoint is shifted by half the size of the bounding box on the route
compute_points_with_x({Start_x, Start_y}, {End_x, End_y}, Step_x, Step_y, Points, Sign) ->
    NextPoint_x = Start_x + (Sign * Step_x),
    NextPoint_y = Start_y + (Sign * Step_y),
    if Sign == 1 ->
        if NextPoint_x < End_x ->
            NewPoints = [{NextPoint_x, NextPoint_y, NextPoint_x, NextPoint_y, normal} | Points],
            compute_points_with_x({NextPoint_x, NextPoint_y}, {End_x, End_y}, Step_x, Step_y, NewPoints, Sign);
        true ->
            Points
        end;
    true ->
        if NextPoint_x > End_x ->
            NewPoints = [{NextPoint_x, NextPoint_y, NextPoint_x, NextPoint_y, normal} | Points],
            compute_points_with_x({NextPoint_x, NextPoint_y}, {End_x, End_y}, Step_x, Step_y, NewPoints, Sign);
        true ->
            Points
        end
    end.

%% Each element is of the form NextPoint_x, NextPoint_y, RealNexPoint_x, RealNextPoint_y
%% This allow to manage the bounding box size when dealing with ack points
%% In the case of normal points, NextPoint and RealNextPoint is equal, while in the case
%% of ack points NextPoint is shifted by half the size of the bounding box on the route
compute_points_with_y({Start_x, Start_y}, {End_x, End_y}, Step_x, Step_y, Points, Sign) ->
    NextPoint_x = Start_x + (Sign * Step_x),
    NextPoint_y = Start_y + (Sign * Step_y),
    if Sign == 1 ->
        if NextPoint_y < End_y ->
            NewPoints = [{NextPoint_x, NextPoint_y, NextPoint_x, NextPoint_y, normal} | Points],
            compute_points_with_y({NextPoint_x, NextPoint_y}, {End_x, End_y}, Step_x, Step_y, NewPoints, Sign);
        true ->
            Points
        end;
    true ->
        if NextPoint_y > End_y ->
            NewPoints = [{NextPoint_x, NextPoint_y, NextPoint_x, NextPoint_y, normal} | Points],
            compute_points_with_y({NextPoint_x, NextPoint_y}, {End_x, End_y}, Step_x, Step_y, NewPoints, Sign);
        true ->
            Points
        end
    end.


compute_slope(Start, End) ->
    {A, B, _C} = geometry_utils:equation_coeffs(Start, End),
    AngularCoefficent = (-1) * (A/B),
    Angle = math:atan(AngularCoefficent),
    Angle.

compute_ack_point_y(X, Y, Angle, Sign, ToBeAdded) ->
    {X, Y + (math:sin(Angle) * (Sign * ToBeAdded))}.


compute_ack_point(X, Y, Angle, Sign, ToBeAdded) ->
        {X + (math:cos(Angle) * (Sign * ToBeAdded)), Y + (math:sin(Angle) * (Sign * ToBeAdded))}.
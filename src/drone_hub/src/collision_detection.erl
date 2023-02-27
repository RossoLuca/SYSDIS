-module(collision_detection).

-export([compute_collision/5]).

compute_collision(DroneSize, Id, {S_a, E_a}, External_Id, {S_b, E_b}) ->
    % Computation of the line's equation passing by the start
    % and end point for each drone
    EquationA = geometry_utils:equation_coeffs(S_a, E_a),
    EquationB = geometry_utils:equation_coeffs(S_b, E_b),
    {A_a, A_b, A_c} = EquationA,
    {B_a, B_b, B_c} = EquationB,

    % Check if the segment S_a,E_a and the segment S_b,E_b are in
    % intersection
    Intersect = geometry_utils:check_intersection({S_a, E_a}, {S_b, E_b}),

    case Intersect of
        intersect ->

            % Compute the intersection point of the two routes by computing
            % the intersection point of their respective lines.
            P = geometry_utils:get_intersection_point(EquationA, EquationB),

            %% Può non essere corretto valutare il punto di intersezione come unico punto di collisione
            %% Infatti anche se vi è un punto di intersezione unico tra le rette, è possibile
            %% che durante il volo i droni possano comunque scontrarsi per via del loro corpo

            PointA = check_body_intersection(DroneSize, P, S_a, E_a, S_b, EquationA),
            PointB = check_body_intersection(DroneSize, P, S_b, E_b, S_a, EquationB),

            {collision, #{Id => PointA, External_Id => PointB}};

        _Other ->

            % Compute if the respective lines of the two routes are parallel
            Parallel = geometry_utils:check_parallel(EquationA, EquationB),

            % Check if there's an overlap between the projections both for the
            % x axes and for the y axes
            ProjectionsOverlap = check_projections_overlapping(DroneSize, {S_a, E_a}, {S_b, E_b}),

            %% Minimum distance that must exists between two drones that are flying
            MinimumDistance = DroneSize * math:sqrt(2),

            case {Parallel, ProjectionsOverlap} of
                {true, overlap} ->
                    % In this case two routes are in collision IFF
                    % their distance is less than diagonal of bounding box
                    % of the drone

                    % To compute the distance between the two routes, we can simply
                    % compute the distance between the two respective lines.
                    % Distance line-line between two parallel lines is nothing else
                    % than the distance between any point on one line and the other line
                    Distance = geometry_utils:distancePointLine(S_a, EquationB),

                    if Distance =< MinimumDistance ->

                        PointA = parallel_intersection_point(DroneSize, S_a, E_a, S_b, E_b, EquationA),
                        PointB = parallel_intersection_point(DroneSize, S_b, E_b, S_a, E_a, EquationB),

                        {collision, #{Id => PointA, External_Id => PointB}};
                    true ->
                        {no_collision, none}
                    end;
                {true, not_overlap} ->
                    %% The respective lines of the two routes are parallel but
                    %% the projections both on the x axes and of the y axes
                    %% of the endpoints of the two routes are not overlapped
                    %% In this case, there's no collision between them

                    {no_collision, none};

                {false, not_overlap} ->
                    %% The two routes are neither parallels and in intersection
                    %% and also their projections on the x axes and on the y axes are
                    %% not overlapped
                    %% In this case there's no collision between them

                    {no_collision, none};

                {false, overlap} ->
                    %% The two routes are neither parallels and in intersection
                    %% but both thei projection on the x and on the y axes overlap
                    %% In this case there can be a collision between one of the endpoints
                    %% of a route and the other route

                    DistanceS_a_To_B = distancePointRoute(DroneSize, S_a, EquationB, S_b, E_b),
                    DistanceE_a_To_B = distancePointRoute(DroneSize, E_a, EquationB, S_b, E_b),
                    DistanceS_b_To_A = distancePointRoute(DroneSize, S_b, EquationA, S_a, E_a),
                    DistanceE_b_To_A = distancePointRoute(DroneSize, E_b, EquationA, S_a, E_a),

                    if DistanceS_a_To_B =< MinimumDistance; DistanceE_a_To_B =< MinimumDistance;
                        DistanceS_b_To_A =< MinimumDistance; DistanceE_b_To_A =< MinimumDistance ->

                        CollisionA = compute_collision_point(MinimumDistance, EquationA, S_a, E_a,
                                            S_b, E_b, DistanceS_a_To_B, DistanceE_a_To_B,
                                            DistanceS_b_To_A, DistanceE_b_To_A),
                        CollisionB = compute_collision_point(MinimumDistance, EquationB, S_b, E_b,
                                            S_a, E_a, DistanceS_b_To_A, DistanceE_b_To_A,
                                            DistanceS_a_To_B, DistanceE_a_To_B),

                        Map = #{Id => CollisionA, External_Id => CollisionB},
                        {collision, Map};
                    true ->
                        {no_collision, none}
                    end
            end
    end.

distancePointRoute(DroneSize, {P_x, P_y}, Equation,{S_x, S_y}, {E_x, E_y}) ->
    ToBeAdded = DroneSize/2,
    if S_x =/= E_x ->
        {Left, Right} = order(S_x, E_x),
        if P_x >= (Left - ToBeAdded), P_x =< (Right + ToBeAdded) ->
            Distance = geometry_utils:distancePointLine({P_x, P_y}, Equation),
            Distance;
        true ->
            infinity
        end;
    true ->
        {Left, Right} = order(S_y, E_y),
        if P_y >= (Left - ToBeAdded), P_y =< (Right + ToBeAdded) ->
            Distance = geometry_utils:distancePointLine({P_x, P_y}, Equation),
            Distance;
        true ->
            infinity
        end
    end.

check_body_intersection(DroneSize, {C_x, C_y}, {S_a_x, _S_a_y}, {E_a_x, E_a_y}, {S_b_x, S_b_y}, Equation) ->
    if S_a_x =/= E_a_x ->
        ToBeAdded = DroneSize/2,
        {Left, Right} = order(C_x, E_a_x),

        if S_b_x >= (Left - ToBeAdded), S_b_x =< (Right + ToBeAdded) ->
            P = geometry_utils:compute_perpendicular_intersection({S_b_x, S_b_y}, Equation),
            DistanceA = geometry_utils:distancePointPoint({C_x, C_y}, P),
            DistanceB = geometry_utils:distancePointPoint({C_x, C_y}, {S_b_x, S_b_y}),

            if round(DistanceA) == round(DistanceB) ->
                P;
            true ->
                {C_x, C_y}
            end;
        true ->
            {C_x, C_y}
        end;
    true ->
        ToBeAdded = DroneSize/2,
        {Left, Right} = order(C_y, E_a_y),

        if S_b_y >= (Left - ToBeAdded), S_b_y =< (Right + ToBeAdded) ->
            P = geometry_utils:compute_perpendicular_intersection({S_b_x, S_b_y}, Equation),
            DistanceA = geometry_utils:distancePointPoint({C_x, C_y}, P),
            DistanceB = geometry_utils:distancePointPoint({C_x, C_y}, {S_b_x, S_b_y}),
            
            if DistanceA == DistanceB ->
                P;
            true ->
                {C_x, C_y}
            end;
        true ->
            {C_x, C_y}
        end
    end.

check_projections_overlapping(DroneSize, {{A_sx, A_sy}, {A_ex, A_ey}}, {{B_sx, B_sy}, {B_ex, B_ey}}) ->
    OverlapX = axes_overlap(DroneSize, A_sx, A_ex, B_sx, B_ex) + axes_overlap(DroneSize, B_sx, B_ex, A_sx, A_ex),
    OverlapY = axes_overlap(DroneSize, A_sy, A_ey, B_sy, B_ey) + axes_overlap(DroneSize,B_sy, B_ey, A_sy, A_ey),

    if OverlapX > 0, OverlapY > 0 ->
        overlap;
    true ->
        not_overlap
    end.

order(A, B) ->
    if A =< B ->
        {A, B};
    true ->
        {B, A}
    end.

axes_overlap(DroneSize, A_s, A_e, B_s, B_e) ->
    ToBeAdded = DroneSize / 2,
    {LeftA, RightA} = order(A_s, A_e),
    {LeftB, RightB} = order(B_s, B_e),
    if (LeftB - ToBeAdded) >= (LeftA - ToBeAdded), (RightB - ToBeAdded) =< (RightA + ToBeAdded) ->
        1;
    (RightB + ToBeAdded) >= (LeftA - ToBeAdded), (LeftB - ToBeAdded) =< (RightA + ToBeAdded) ->
        1;
    true ->
        0
    end.

compute_collision_point(Minimum, Equation, Start, End, StartOther, EndOther,
                StartDistanceToOther, EndDistanceToOther, DistanceStartOtherToMe, DistanceEndOtherToMe) ->

    if StartDistanceToOther =< Minimum; EndDistanceToOther =< Minimum ->
        
        if EndDistanceToOther > Minimum ->
            CollisionPoint = Start,
            CollisionPoint;
        true ->
            CollisionPoint = End,
            CollisionPoint
        end;
    DistanceStartOtherToMe =< Minimum; DistanceEndOtherToMe =< Minimum ->
        
        if DistanceStartOtherToMe =< Minimum, DistanceEndOtherToMe =< Minimum ->
            %% When both the other endpoints are in collision with my route,
            %% to compute my collision point I must choose the other endpoint that
            %% is the nearest to my end
            DistanceStartOtherToMyEnd = geometry_utils:distancePointPoint(StartOther, End),
            DistanceEndOtherToMyEnd = geometry_utils:distancePointPoint(EndOther, End),
            if DistanceStartOtherToMyEnd < DistanceEndOtherToMyEnd ->
                CollisionPoint = geometry_utils:compute_perpendicular_intersection(StartOther, Equation),
                CollisionPoint;
            true ->
                CollisionPoint = geometry_utils:compute_perpendicular_intersection(EndOther, Equation),
                CollisionPoint
            end;
        DistanceStartOtherToMe =< Minimum ->
            CollisionPoint = geometry_utils:compute_perpendicular_intersection(StartOther, Equation),
            CollisionPoint;
        true ->
            CollisionPoint = geometry_utils:compute_perpendicular_intersection(EndOther, Equation),
            CollisionPoint
        end
    end.

parallel_intersection_point(DroneSize, {S_a_x, S_a_y}, {E_a_x, E_a_y}, {S_b_x, S_b_y}, {E_b_x, E_b_y}, Equation) ->
    C = DroneSize/2,
    if S_a_x =/= E_a_x ->
        %% Case in which the collision point is decided watching the x component of the points
        if S_a_x =< E_a_x ->
                            if S_b_x > (S_a_x - C) ->
                                if S_b_x =< (E_a_x + C) ->
                                    geometry_utils:compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
                                true ->
                                    geometry_utils:compute_perpendicular_intersection({E_a_x, E_a_y}, Equation)
                                end;
                            true ->
                                if E_b_x =< (E_a_x + C) ->
                                    geometry_utils:compute_perpendicular_intersection({E_b_x, E_b_y}, Equation);
                                true ->
                                    geometry_utils:compute_perpendicular_intersection({E_a_x, E_a_y}, Equation)
                                end
                            end;
                        true ->
                            if S_b_x > (E_a_x - C) ->
                                if S_b_x =< (S_a_x + C) ->
                                    geometry_utils:compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
                                true ->
                                    geometry_utils:compute_perpendicular_intersection({S_a_x, S_a_y}, Equation)
                                end;
                            true ->
                                if E_b_x =< (S_a_x + C) ->
                                    geometry_utils:compute_perpendicular_intersection({E_b_x, E_b_y}, Equation);
                                true ->
                                    geometry_utils:compute_perpendicular_intersection({S_a_x, S_a_y}, Equation)
                                end
                            end
                        end;
    true ->
        %% Case in which the collision point is decided watching the y component of the points
                if S_a_y =< E_a_y ->
                            if S_b_y > (S_a_y - C) ->
                                if S_b_y =< (E_a_y + C) ->
                                    geometry_utils:compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
                                true ->
                                    geometry_utils:compute_perpendicular_intersection({E_a_x, E_a_y}, Equation)
                                end;
                            true ->
                                if E_b_y =< (E_a_y + C) ->
                                    geometry_utils:compute_perpendicular_intersection({E_b_x, E_b_y}, Equation);
                                true ->
                                    geometry_utils:compute_perpendicular_intersection({E_a_x, E_a_y}, Equation)
                                end
                            end;
                        true ->
                            if S_b_y > (E_a_y - C) ->
                                if S_b_y =< (S_a_y + C) ->
                                    geometry_utils:compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
                                true ->
                                    geometry_utils:compute_perpendicular_intersection({S_a_x, S_a_y}, Equation)
                                end;
                            true ->
                                if E_b_y =< (S_a_y + C) ->
                                    geometry_utils:compute_perpendicular_intersection({E_b_x, E_b_y}, Equation);
                                true ->
                                    geometry_utils:compute_perpendicular_intersection({S_a_x, S_a_y}, Equation)
                                end
                            end
                        end
    end.
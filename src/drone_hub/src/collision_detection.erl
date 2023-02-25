-module(collision_detection).

-export([compute_collision/5, get_perpendicular_line/2]).

compute_collision(DroneSize, Id, {S_a, E_a}, External_Id, {S_b, E_b}) ->
    % Computation of the line's equation passing by the start
    % and end point for each drone
    EquationA = equation_coeffs(S_a, E_a),
    EquationB = equation_coeffs(S_b, E_b),
    {A_a, A_b, A_c} = EquationA,
    {B_a, B_b, B_c} = EquationB,

    % Check if the segment S_a,E_a and the segment S_b,E_b are in
    % intersection
    Intersect = check_intersection({S_a, E_a}, {S_b, E_b}),

    case Intersect of
        intersect ->

            % Compute the intersection point of the two routes by computing
            % the intersection point of their respective lines.
            P = get_intersection_point(EquationA, EquationB),
            % % io:format("Collision found!~n"),
            % % io:format("Intersection on the point ~p.~n", [P]),

            %% Può non essere corretto valutare il punto di intersezione come unico punto di collisione
            %% Infatti anche se vi è un punto di intersezione unico tra le rette, è possibile
            %% che durante il volo i droni possano comunque scontrarsi per via del loro corpo

            PointA = check_body_intersection(DroneSize, P, S_a, E_a, S_b, EquationA),
            PointB = check_body_intersection(DroneSize, P, S_b, E_b, S_a, EquationB),
            % % io:format("PointA: ~p~n", [PointA]),
            % % io:format("PointB: ~p~n", [PointB]),
            {collision, #{Id => PointA, External_Id => PointB}};

        _Other ->

            % Compute if the respective lines of the two routes are parallel
            Parallel = check_parallel(EquationA, EquationB),

            % Check if there's an overlap between the projections both for the
            % x axes and for the y axes
            ProjectionsOverlap = check_projections_overlapping(DroneSize, {S_a, E_a}, {S_b, E_b}),

            %% Minimum distance that must exists between two drones that are flying
            MinimumDistance = DroneSize * math:sqrt(2),

            % io:format("Parallel: ~p~n", [Parallel]),
            case {Parallel, ProjectionsOverlap} of
                {true, overlap} ->
                    % In this case two routes are in collision IFF
                    % their distance is less than diagonal of bounding box
                    % of the drone

                    % To compute the distance between the two routes, we can simply
                    % compute the distance between the two respective lines.
                    % Distance line-line between two parallel lines is nothing else
                    % than the distance between any point on one line and the other line
                    Distance = distancePointLine(S_a, EquationB),

                    if Distance =< MinimumDistance ->

                        % io:format("Collision found!~n"),
                        % io:format("Routes are parallel and near: possible collision between the drone body.~n"),

                        PointA = parallel_intersection_point(DroneSize, S_a, E_a, S_b, E_b, EquationA),
                        PointB = parallel_intersection_point(DroneSize, S_b, E_b, S_a, E_a, EquationB),
                        % io:format("PointA: ~p~n", [PointA]),
                        % io:format("PointB: ~p~n", [PointB]),
                        {collision, #{Id => PointA, External_Id => PointB}};
                    true ->
                        % io:format("No collision found!~n"),
                        {no_collision, none}
                    end;
                {true, not_overlap} ->
                    %% The respective lines of the two routes are parallel but
                    %% the projections both on the x axes and of the y axes
                    %% of the endpoints of the two routes are not overlapped
                    %% In this case, there's no collision between them

                    % io:format("No collision found!~n"),
                    {no_collision, none};

                {false, not_overlap} ->
                    %% The two routes are neither parallels and in intersection
                    %% and also their projections on the x axes and on the y axes are
                    %% not overlapped
                    %% In this case there's no collision between them

                    % io:format("No collision found!~n"),
                    {no_collision, none};

                {false, overlap} ->
                    %% The two routes are neither parallels and in intersection
                    %% but both thei projection on the x and on the y axes overlap
                    %% In this case there can be a collision between one of the endpoints
                    %% of a route and the other route

                    % DistanceS_a_To_B = distancePointLine(S_a, EquationB),
                    % DistanceE_a_To_B = distancePointLine(E_a, EquationB),
                    % DistanceS_b_To_A = distancePointLine(S_b, EquationA),
                    % DistanceE_b_To_A = distancePointLine(E_b, EquationA),
                    DistanceS_a_To_B = distancePointRoute(DroneSize, S_a, EquationB, S_b, E_b),
                    DistanceE_a_To_B = distancePointRoute(DroneSize, E_a, EquationB, S_b, E_b),
                    DistanceS_b_To_A = distancePointRoute(DroneSize, S_b, EquationA, S_a, E_a),
                    DistanceE_b_To_A = distancePointRoute(DroneSize, E_b, EquationA, S_a, E_a),


                    % if ((DistanceE_a_To_B =< MinimumDistance) or (DistanceE_a_To_B =< MinimumDistance)) and
                    %     ((DistanceS_b_To_A =< MinimumDistance) or (DistanceE_a_To_B =< MinimumDistance)) ->

                    if DistanceS_a_To_B =< MinimumDistance; DistanceE_a_To_B =< MinimumDistance;
                        DistanceS_b_To_A =< MinimumDistance; DistanceE_b_To_A =< MinimumDistance ->
                        % io:format("Collision found!~n"),
                        % io:format("One of the endpoints of a route collide with the other route!~n"),
                        % io:format("Minimal distance allowed > ~p~n",[MinimumDistance]),
                        % io:format("Distance S_a to B: ~p~n", [DistanceS_a_To_B]),
                        % io:format("Distance E_a to B: ~p~n", [DistanceE_a_To_B]),
                        % io:format("Distance S_b to A: ~p~n", [DistanceS_b_To_A]),
                        % io:format("Distance E_b to A: ~p~n", [DistanceE_b_To_A]),

                        CollisionA = compute_collision_point(MinimumDistance, EquationA, S_a, E_a,
                                            S_b, E_b, DistanceS_a_To_B, DistanceE_a_To_B,
                                            DistanceS_b_To_A, DistanceE_b_To_A),
                        CollisionB = compute_collision_point(MinimumDistance, EquationB, S_b, E_b,
                                            S_a, E_a, DistanceS_b_To_A, DistanceE_b_To_A,
                                            DistanceS_a_To_B, DistanceE_a_To_B),

                        Map = #{Id => CollisionA, External_Id => CollisionB},
                        {collision, Map};
                    true ->
                        % io:format("No collision found!~n"),
                        {no_collision, none}
                    end
            end
    end.

distancePointRoute(DroneSize, {P_x, P_y}, Equation,{S_x, S_y}, {E_x, E_y}) ->
    ToBeAdded = DroneSize/2,
    if S_x =/= E_x ->
        {Left, Right} = order(S_x, E_x),
        if P_x >= (Left - ToBeAdded), P_x =< (Right + ToBeAdded) ->
            Distance = distancePointLine({P_x, P_y}, Equation),
            Distance;
        true ->
            infinity
        end;
    true ->
        {Left, Right} = order(S_y, E_y),
        if P_y >= (Left - ToBeAdded), P_y =< (Right + ToBeAdded) ->
            Distance = distancePointLine({P_x, P_y}, Equation),
            Distance;
        true ->
            infinity
        end
    end.



% check_body_intersection({C_x, C_y}, {S_a_x, _S_a_y}, {E_a_x, E_a_y}, {S_b_x, S_b_y}, Equation) ->
%     % Distance = distancePointLine({S_b_x, S_b_y}, Equation),
%     Distance = distancePointRoute({S_b_x, S_b_y}, Equation, {C_x, C_y}, {E_a_x, E_a_y}),
%     if Distance > ?DRONE_SIZE ->
%         {C_x, C_y};
%     true ->
%         if S_a_x =/= E_a_x ->
%             ToBeAdded = ?DRONE_SIZE/2,
%             {Left, Right} = order(C_x, E_a_x),
%             io:format("Left ~p Right ~p~n", [Left, Right]),
%             if S_b_x >= (Left - ToBeAdded), S_b_x =< (Right + ToBeAdded) ->
%                 compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
%             true ->
%                 {E_a_x, E_a_y}
%             end;
%         true ->
%             ToBeAdded = ?DRONE_SIZE/2,
%             {Left, Right} = order(C_y, E_a_y),
%             if S_b_y >= (Left - ToBeAdded), S_b_y =< (Right + ToBeAdded) ->
%                 compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
%             true ->
%                 {E_a_x, E_a_y}
%             end
%         end
%     end.

check_body_intersection(DroneSize, {C_x, C_y}, {S_a_x, _S_a_y}, {E_a_x, E_a_y}, {S_b_x, S_b_y}, Equation) ->
    if S_a_x =/= E_a_x ->
        ToBeAdded = DroneSize/2,
        {Left, Right} = order(C_x, E_a_x),
        % io:format("Left ~p Right ~p~n", [Left, Right]),
        if S_b_x >= (Left - ToBeAdded), S_b_x =< (Right + ToBeAdded) ->
            P = compute_perpendicular_intersection({S_b_x, S_b_y}, Equation),
            DistanceA = distancePointPoint({C_x, C_y}, P),
            DistanceB = distancePointPoint({C_x, C_y}, {S_b_x, S_b_y}),
            % io:format("DistanceA: ~p\t DistanceB: ~p~n", [DistanceA, DistanceB]),
            % io:format("Rounded --> DistanceA: ~p\t DistanceB: ~p~n", [round(DistanceA), round(DistanceB)]),
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
        % io:format("Left ~p Right ~p~n", [Left, Right]),
        if S_b_y >= (Left - ToBeAdded), S_b_y =< (Right + ToBeAdded) ->
            P = compute_perpendicular_intersection({S_b_x, S_b_y}, Equation),
            DistanceA = distancePointPoint({C_x, C_y}, P),
            DistanceB = distancePointPoint({C_x, C_y}, {S_b_x, S_b_y}),
            % io:format("DistanceA: ~p\t DistanceB: ~p~n", [DistanceA, DistanceB]),
            if DistanceA == DistanceB ->
                P;
            true ->
                {C_x, C_y}
            end;
        true ->
            {C_x, C_y}
        end
    end.

check_parallel({A_a, A_b, _A_c}, {B_a, B_b, _B_c}) ->
    Product1 = A_a * B_b,
    Product2 = A_b * B_a,
    case Product1 == Product2 of
        true -> true;
        false -> false
    end.


check_intersection({A, B}, {C, D}) ->
    ACD = ccw(A, C, D),
    BCD = ccw(B, C, D),
    ABC = ccw(A, B, C),
    ABD = ccw(A, B, D),
    if ACD =/= BCD, ABC =/= ABD ->
        intersect;
    true ->
        not_intersect_or_collinear
    end.

ccw({Ax, Ay}, {Bx, By}, {Cx, Cy}) ->
    Op1 = (Cy - Ay) * (Bx - Ax),
    Op2 = (By - Ay) * (Cx - Ax),
    if Op1 > Op2 ->
        true;
    true ->
        false
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
    if (LeftB - ToBeAdded) >= (LeftA - ToBeAdded), (LeftA - ToBeAdded) =< (RightA + ToBeAdded) ->
        1;
    (RightB + ToBeAdded) >= (LeftA - ToBeAdded), (LeftB - ToBeAdded) =< (RightA + ToBeAdded) ->
        1;
    true ->
        0
    end.

get_intersection_point({A_a, A_b, A_c}, {B_a, B_b, B_c}) ->
    Y_num = (A_c * B_a) - (A_a * B_c),
    Y_den = (A_a * B_b) - (B_a * A_b),
    Y = Y_num / Y_den,

    if A_a =/= 0.0 ->
        X_num = ((-1.0) * A_b * Y) - A_c,
        X = X_num / A_a,
        {X, Y};
    true ->
        X = ((-1) * (B_c))/ B_a,
        {X, Y}
    end.


distancePointLine({P_x, P_y}, {A, B, C}) ->
    Num = abs((A * P_x) + (B * P_y) + C),
    Den = math:sqrt((A * A) + (B * B)),
    Distance = Num / Den,
    Distance.

equation_coeffs({S_x, S_y}, {E_x, E_y}) ->
    ESy = E_y - S_y,
    ESx = E_x - S_x,
    A = ESy,
    B = ESx * (-1.0),
    C = ((-1.0) * S_x * ESy) + (S_y * ESx),
    {A, B, C}.

compute_perpendicular_intersection(Point, Equation) ->
    PerpendicularEquation = get_perpendicular_line(Point, Equation),
    Intersection = get_intersection_point(PerpendicularEquation, Equation),
    Intersection.

get_perpendicular_line({P_x, P_y}, {A, B, _C}) ->
    if A == 0.0 ->
        %% the equation is something like y = c
        {1.0, 0.0, -P_x};
    B == 0.0 ->
        %% the equation is something like x = c
        {0.0, 1.0, -P_y};
    true ->
        AngularCoeff = -(A/B),
        PerpendicularAngularCoeff = -(1.0 / AngularCoeff),
        B_perpendicular = -1.0,
        C_perpendicular = ((-1.0) * PerpendicularAngularCoeff * P_x) + P_y,
        {PerpendicularAngularCoeff, B_perpendicular, C_perpendicular}
    end.


compute_collision_point(Minimum, Equation, Start, End, StartOther, EndOther,
                StartDistanceToOther, EndDistanceToOther, DistanceStartOtherToMe, DistanceEndOtherToMe) ->

    if StartDistanceToOther =< Minimum; EndDistanceToOther =< Minimum ->
        % io:format("CASO A~n"),
        if EndDistanceToOther > Minimum ->
            CollisionPoint = Start,
            CollisionPoint;
        true ->
            CollisionPoint = End,
            CollisionPoint
        end;
    DistanceStartOtherToMe =< Minimum; DistanceEndOtherToMe =< Minimum ->
        % io:format("CASO B~n"),
        if DistanceStartOtherToMe =< Minimum, DistanceEndOtherToMe =< Minimum ->
            %% When both the other endpoints are in collision with my route,
            %% to compute my collision point I must choose the other endpoint that
            %% is the nearest to my end
            DistanceStartOtherToMyEnd = distancePointPoint(StartOther, End),
            DistanceEndOtherToMyEnd = distancePointPoint(EndOther, End),
            if DistanceStartOtherToMyEnd < DistanceEndOtherToMyEnd ->
                CollisionPoint = compute_perpendicular_intersection(StartOther, Equation),
                % io:format("Collision point on: ~p~n", [CollisionPoint]),
                CollisionPoint;
            true ->
                CollisionPoint = compute_perpendicular_intersection(EndOther, Equation),
                % io:format("Collision point on: ~p~n", [CollisionPoint]),
                CollisionPoint
            end;
        DistanceStartOtherToMe =< Minimum ->
            CollisionPoint = compute_perpendicular_intersection(StartOther, Equation),
            % io:format("Collision point on: ~p~n", [CollisionPoint]),
            CollisionPoint;
        true ->
            CollisionPoint = compute_perpendicular_intersection(EndOther, Equation),
            % io:format("Collision point on: ~p~n", [CollisionPoint]),
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
                                    % io:format("~p~n", [{S_b_x, S_b_y}]),
                                    compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
                                true ->
                                    % io:format("~p~n", [{E_a_x, E_a_y}]),
                                    compute_perpendicular_intersection({E_a_x, E_a_y}, Equation)
                                end;
                            true ->
                                if E_b_x =< (E_a_x + C) ->
                                    % io:format("~p~n", [{E_b_x, E_b_y}]),
                                    compute_perpendicular_intersection({E_b_x, E_b_y}, Equation);
                                true ->
                                    % io:format("~p~n", [{E_a_x, E_a_y}]),
                                    compute_perpendicular_intersection({E_a_x, E_a_y}, Equation)
                                end
                            end;
                        true ->
                            if S_b_x > (E_a_x - C) ->
                                if S_b_x =< (S_a_x + C) ->
                                    % io:format("~p~n", [{S_b_x, S_b_y}]),
                                    compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
                                true ->
                                    % io:format("~p~n", [{S_a_x, S_a_y}]),
                                    compute_perpendicular_intersection({S_a_x, S_a_y}, Equation)
                                end;
                            true ->
                                if E_b_x =< (S_a_x + C) ->
                                    % io:format("~p~n", [{E_b_x, E_b_y}]),
                                    compute_perpendicular_intersection({E_b_x, E_b_y}, Equation);
                                true ->
                                    % io:format("~p~n", [{S_a_x, S_a_y}]),
                                    compute_perpendicular_intersection({S_a_x, S_a_y}, Equation)
                                end
                            end
                        end;
    true ->
        %% Case in which the collision point is decided watching the y component of the points
                if S_a_y =< E_a_y ->
                            if S_b_y > (S_a_y - C) ->
                                if S_b_y =< (E_a_y + C) ->
                                    % io:format("a~n"),
                                    % io:format("~p~n", [{S_b_x, S_b_y}]),
                                    % io:format("~p~n", [Equation]),
                                    % io:format("~p~n", [{S_b_x, S_b_y}]),
                                    compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
                                true ->
                                    % io:format("b~n"),
                                    % io:format("~p~n", [{E_a_x, E_a_y}]),
                                    compute_perpendicular_intersection({E_a_x, E_a_y}, Equation)
                                end;
                            true ->
                                if E_b_y =< (E_a_y + C) ->
                                    % io:format("c~n"),
                                    % io:format("~p~n", [{E_b_x, E_b_y}]),
                                    compute_perpendicular_intersection({E_b_x, E_b_y}, Equation);
                                true ->
                                    % io:format("d~n"),
                                    % io:format("~p~n", [{E_a_x, E_a_y}]),
                                    compute_perpendicular_intersection({E_a_x, E_a_y}, Equation)
                                end
                            end;
                        true ->
                            if S_b_y > (E_a_y - C) ->
                                if S_b_y =< (S_a_y + C) ->
                                    % io:format("e~n"),
                                    % io:format("~p~n", [{S_b_x, S_b_y}]),
                                    compute_perpendicular_intersection({S_b_x, S_b_y}, Equation);
                                true ->
                                    % io:format("f~n"),
                                    % io:format("~p~n", [{S_a_x, S_a_y}]),
                                    compute_perpendicular_intersection({S_a_x, S_a_y}, Equation)
                                end;
                            true ->
                                if E_b_y =< (S_a_y + C) ->
                                    % io:format("g~n"),
                                    % io:format("~p~n", [{E_b_x, E_b_y}]),
                                    compute_perpendicular_intersection({E_b_x, E_b_y}, Equation);
                                true ->
                                    % io:format("h~n"),
                                    % io:format("~p~n", [{S_a_x, S_a_y}]),
                                    compute_perpendicular_intersection({S_a_x, S_a_y}, Equation)
                                end
                            end
                        end
    end.

distancePointPoint({P_x, P_y}, {S_x, S_y}) ->
    X_component = (S_x - P_x) * (S_x - P_x),
    Y_component = (S_y - P_y) * (S_y - P_y),
    Distance = math:sqrt(X_component + Y_component),
    Distance.
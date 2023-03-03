-module(geometry_utils).

-export([distancePointPoint/2, get_perpendicular_line/2, compute_perpendicular_intersection/2,
        check_intersection/2, check_parallel/2, distancePointLine/2, equation_coeffs/2,
        get_intersection_point/2]).


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

check_parallel({A_a, A_b, _A_c}, {B_a, B_b, _B_c}) ->
    Product1 = A_a * B_b,
    Product2 = A_b * B_a,
    case Product1 == Product2 of
        true -> true;
        false -> false
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


distancePointPoint({P_x, P_y}, {S_x, S_y}) ->
    X_component = (S_x - P_x) * (S_x - P_x),
    Y_component = (S_y - P_y) * (S_y - P_y),
    Distance = math:sqrt(X_component + Y_component),
    Distance.


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
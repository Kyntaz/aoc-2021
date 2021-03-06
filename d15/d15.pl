:- module(d15, []).
:- use_module("common/util").
:- dynamic d_expanded/3.
:- dynamic d_closed/2.

%! expanded(?Point, ?Cost)
expanded((X, Y), C) :- d_expanded(X, Y, C).

%! closed(?Point)
closed((X, Y)) :- d_closed(X, Y).

%! assert_expanded(+Point, +Cost)
assert_expanded((X, Y), C) :- assertz(d_expanded(X, Y, C)).

%! assert_closed(+Point)
assert_closed((X, Y)) :- assertz(d_closed(X, Y)).

%! retract_expanded(+Point, +Cost)
retract_expanded((X, Y), C) :- retract(d_expanded(X, Y, C)).

%! retract_closed(+Point)
retract_closed((X, Y)) :- retract(d_closed(X, Y)).

%! read_risk_matrix(-RiskMatrix)
read_risk_matrix(RiskMatrix) :-
    read_grid(Grid),
    maplist(maplist(char_number), Grid, RiskMatrix).

%! dimensions(+Matrix, -Width, -Height)
dimensions(Matrix, W, H) :-
    length(Matrix, H),
    nth0(0, Matrix, Line),
    length(Line, W).

%! get_value(+Matrix, +Point, -Value)
get_value(Matrix, (X, Y), Value) :-
    dimensions(Matrix, W, H),
    XMod is X mod W,
    YMod is Y mod H,
    nth0(YMod, Matrix, Line),
    nth0(XMod, Line, BaseValue),
    Value is 1 + ((BaseValue + (X // W) + (Y // H) - 1) mod 9).

%! next_position(+Point, -NextPoint) is multi
next_position((X, Y), (X1, Y)) :-
    X1 is X + 1.
next_position((X, Y), (X, Y1)) :-
    Y1 is Y + 1.
next_position((X, Y), (X1, Y)) :-
    X1 is X - 1.
next_position((X, Y), (X, Y1)) :-
    Y1 is Y - 1.

%! neighbor_cost(+Matrix, +ExpandedPoint, -ExpandedNeighbor)
neighbor_cost(Matrix, (Point, Cost), (Neighbor, Cost1)) :-
    next_position(Point, Neighbor),
    get_value(Matrix, Neighbor, NCost),
    Cost1 is Cost + NCost.

%! visit(+ExpandedPoint)
visit((Position, Cost)) :-
    expanded(Position, OldCost),
    OldCost =< Cost, !.
visit((Position, Cost)) :-
    expanded(Position, OldCost),
    OldCost > Cost, !,
    retract_expanded(Position, OldCost),
    assert_expanded(Position, Cost).
visit((Position, Cost)) :-
    assert_expanded(Position, Cost).

%! visit(+Matrix, +Limits, +CurrentPoint)
visit(Matrix, (MaxX, MaxY), Current) :-
    findall(Neighbor, (
        neighbor_cost(Matrix, Current, Neighbor),
        Neighbor = ((X, Y), _),
        X =< MaxX,
        Y =< MaxY,
        X >= 0,
        Y >= 0,
        \+ closed((X, Y))
    ), Neighbors),
    maplist(visit, Neighbors).

%! a_star(+Matrix, +EndPosition, -FinalCost)
a_star(_, EndPosition, FinalCost) :-
    expanded(EndPosition, FinalCost), !.
a_star(Matrix, EndPosition, FinalCost) :-
    EndPosition = (MaxX, MaxY),
    aggregate_all(min(H, (P, C)), (
        expanded(P, C),
        P = (X, Y),
        H is C + ((MaxX - X) + (MaxY  - Y))
    ), min(_, (Position, Cost))),
    assert_closed(Position),
    retract_expanded(Position, Cost),
    visit(Matrix, EndPosition, (Position, Cost)),
    a_star(Matrix, EndPosition, FinalCost).

p1 :-
    read_risk_matrix(RiskMatrix),
    dimensions(RiskMatrix, W, H),
    EndX is W - 1,
    EndY is H - 1,
    assert_expanded((0,0), 0),
    a_star(RiskMatrix, (EndX, EndY), Cost),
    write_answer(Cost).

p2 :-
    read_risk_matrix(RiskMatrix),
    dimensions(RiskMatrix, W, H),
    EndX is W * 5 - 1,
    EndY is H * 5 - 1,
    assert_expanded((0,0), 0),
    a_star(RiskMatrix, (EndX, EndY), Cost),
    write_answer(Cost).

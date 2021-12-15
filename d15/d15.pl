:- use_module("common/util").
:- dynamic d_expanded/3.
:- dynamic d_closed/2.

expanded((X, Y), C) :- d_expanded(X, Y, C).
closed((X, Y)) :- d_closed(X, Y).

assert_expanded((X, Y), C) :- assertz(d_expanded(X, Y, C)).
assert_closed((X, Y)) :- assertz(d_closed(X, Y)).
retract_expanded((X, Y), C) :- retract(d_expanded(X, Y, C)).
retract_closed((X, Y)) :- retract(d_closed(X, Y)).

read_risk_matrix(RiskMatrix) :-
    read_grid(Grid),
    maplist(maplist(char_number), Grid, RiskMatrix).

dimensions(Matrix, W, H) :-
    length(Matrix, H),
    nth0(0, Matrix, Line),
    length(Line, W).

get_value(Matrix, (X, Y), Value) :-
    dimensions(Matrix, W, H),
    XMod is X mod W,
    YMod is Y mod H,
    nth0(YMod, Matrix, Line),
    nth0(XMod, Line, BaseValue),
    Value is 1 + ((BaseValue + (X // W) + (Y // H) - 1) mod 9).

end_position(Matrix, (X, Y)) :-
    dimensions(Matrix, W, H),
    X is W - 1,
    Y is H - 1.

next_position((X, Y), (X1, Y)) :-
    X1 is X + 1.
next_position((X, Y), (X, Y1)) :-
    Y1 is Y + 1.
next_position((X, Y), (X1, Y)) :-
    X1 is X - 1.
next_position((X, Y), (X, Y1)) :-
    Y1 is Y - 1.

neighbor_cost(Matrix, (Point, Cost), (Neighbor, Cost1)) :-
    next_position(Point, Neighbor),
    get_value(Matrix, Neighbor, NCost),
    Cost1 is Cost + NCost.

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

dijkstra(_, EndPosition, FinalCost) :-
    expanded(EndPosition, FinalCost), !.
dijkstra(Matrix, EndPosition, FinalCost) :-
    EndPosition = (MaxX, MaxY),
    aggregate_all(min(H, (P, C)), (
        expanded(P, C),
        P = (X, Y),
        H is C + ((MaxX - X) + (MaxY  - Y))
    ), min(_, (Position, Cost))),
    assert_closed(Position),
    retract_expanded(Position, Cost),
    visit(Matrix, EndPosition, (Position, Cost)),
    dijkstra(Matrix, EndPosition, FinalCost).

p1 :-
    read_risk_matrix(RiskMatrix),
    end_position(RiskMatrix, EndPosition),
    assert_expanded((0,0), 0),
    dijkstra(RiskMatrix, EndPosition, Cost),
    writeln(Cost).

p2 :-
    read_risk_matrix(RiskMatrix),
    dimensions(RiskMatrix, W, H),
    EndX is W * 5 - 1,
    EndY is H * 5 - 1,
    assert_expanded((0,0), 0),
    dijkstra(RiskMatrix, (EndX, EndY), Cost),
    writeln(Cost).

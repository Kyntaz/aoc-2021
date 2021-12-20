:- module(d19, []).
:- use_module("common/util.pl").

read_position(Line, p(X, Y, Z)) :-
    split_string(Line, ",", "", [XStr, YStr, ZStr]),
    number_string(X, XStr),
    number_string(Y, YStr),
    number_string(Z, ZStr).

read_scanner([NameStr | Lines], scanner(Name, Beacons)) :-
    split_string(NameStr, " ", " ", ["---", "scanner", NameWord, "---"]),
    number_string(Name, NameWord),
    maplist(read_position, Lines, Beacons0),
    sort(Beacons0, Beacons).

read_scanners(Scanners) :-
    read_blocks(ScannerBlocks),
    maplist(read_scanner, ScannerBlocks, Scanners).

side(+).
side(-).

dir(x).
dir(y).
dir(z).

axis(ax(Side, Dir)) :-
    side(Side),
    dir(Dir).

neg_ax(ax(+,Side), ax(-,Side)).
neg_ax(ax(-,Side), ax(+,Side)).

:- table perpendicular/3.
perpendicular(ax(+,x), ax(+,y), ax(+,z)) :- !.
perpendicular(ax(+,y), ax(+,z), ax(+,x)) :- !.
perpendicular(ax(+,z), ax(+,x), ax(+,y)) :- !.
perpendicular(Ax1, Ax2, Ax3) :-
    perpendicular(Ax2, Ax1, NAx3),
    neg_ax(Ax3, NAx3), !.
perpendicular(Ax1, Ax2, Ax3) :-
    neg_ax(Ax1, NAx1),
    perpendicular(NAx1, Ax2, NAx3),
    neg_ax(Ax3, NAx3), !.
perpendicular(Ax1, Ax2, Ax3) :-
    neg_ax(Ax2, NAx2),
    perpendicular(Ax1, NAx2, NAx3),
    neg_ax(Ax3, NAx3), !.
perpendicular(Ax1, Ax2, Ax3) :-
    neg_ax(Ax1, NAx1),
    neg_ax(Ax2, NAx2),
    perpendicular(NAx1, NAx2, Ax3), !.

orientation(o(Facing, Up)) :-
    axis(Facing),
    axis(Up),
    \+ (
        Facing = ax(_, Dir),
        Up = ax(_, Dir)
    ).

oriented(o(Facing, Up), p(X0, Y0, Z0), p(X, Y, Z)) :-
    Facing = ax(Side, Dir),
    (Dir = x -> X1 is X0 ; true),
    (Dir = y -> X1 is Y0 ; true),
    (Dir = z -> X1 is Z0 ; true),
    (Side = + -> X is X1 ; true),
    (Side = - -> X is -X1 ; true),
    Up = ax(Side1, Dir1),
    (Dir1 = x -> Y1 is X0 ; true),
    (Dir1 = y -> Y1 is Y0 ; true),
    (Dir1 = z -> Y1 is Z0 ; true),
    (Side1 = + -> Y is Y1 ; true),
    (Side1 = - -> Y is -Y1 ; true),
    perpendicular(Facing, Up, ax(Side2, Dir2)),
    (Dir2 = x -> Z1 is X0 ; true),
    (Dir2 = y -> Z1 is Y0 ; true),
    (Dir2 = z -> Z1 is Z0 ; true),
    (Side2 = + -> Z is Z1 ; true),
    (Side2 = - -> Z is -Z1 ; true), !.

oriented(Orientation, scanner(S, Beacons0), scanner(S, Beacons)) :-
    maplist(oriented(Orientation), Beacons0, Beacons1),
    sort(Beacons1, Beacons).

translated(v(Vx, Vy, Vz), p(X0, Y0, Z0), p(X, Y, Z)) :-
    X is X0 + Vx,
    Y is Y0 + Vy,
    Z is Z0 + Vz.

translated(Vect, scanner(S, Beacons0), scanner(S, Beacons)) :-
    maplist(translated(Vect), Beacons0, Beacons).

to(v(Vx, Vy, Vz), p(X1, Y1, Z1), p(X2, Y2, Z2)) :-
    Vx is X2 - X1,
    Vy is Y2 - Y1,
    Vz is Z2 - Z1.

inverse_v(v(Vx,Vy,Vz), v(NVx,NVy,NVz)) :-
    NVx is -Vx,
    NVy is -Vy,
    NVz is -Vz.

add_v(p(X0,Y0,Z0), v(Vx,Vy,Vz), p(X,Y,Z)) :-
    X is X0 + Vx,
    Y is Y0 + Vy,
    Z is Z0 + Vz.

align(scanner(S1, Beacons1), scanner(S2, Beacons2), t((S1, S2), O, V)) :-
    orientation(O),
    oriented(O, scanner(S1, Beacons1), scanner(S1, Beacons3)),
    member(Beacon3, Beacons3),
    member(Beacon2, Beacons2),
    to(V, Beacon3, Beacon2),
    translated(V, scanner(S1, Beacons3), scanner(S1, Beacons4)),
    ord_intersect(Beacons4, Beacons2, IBeacons),
    length(IBeacons, Count),
    Count >= 12, !.

scanner_align(Scanners, Scanner, Transform) :-
    member(Scanner1, Scanners),
    Scanner1 \= Scanner,
    align(Scanner, Scanner1, Transform).

alignements(Scanners, Transforms) :-
    findall(Transform, (
        member(Scanner, Scanners),
        scanner_align(Scanners, Scanner, Transform)
    ), Transforms).

normalize(_, _, _, scanner(0, Beacons), scanner(0, Beacons), p(0,0,0)) :- !.
normalize(Transforms, _, TempPosition, scanner(S0, Beacons0), scanner(0, Beacons), NPosition) :-
    member(t((S0, 0), O, V), Transforms), !,
    oriented(O, scanner(S0, Beacons0), scanner(S0, Beacons1)),
    translated(V, scanner(S0, Beacons1), scanner(S0, Beacons)),
    oriented(O, TempPosition, TempPosition1),
    inverse_v(V, NV),
    add_v(TempPosition1, NV, NPosition).
normalize(Transforms, Previous, TempPosition, scanner(S0, Beacons0), scanner(S, Beacons), NPosition) :-
    member(t((S0, S1), O, V), Transforms),
    \+ member((S0, S1), Previous),
    oriented(O, scanner(S0, Beacons0), scanner(S0, Beacons1)),
    translated(V, scanner(S0, Beacons1), scanner(S0, Beacons2)),
    oriented(O, TempPosition, TempPosition1),
    inverse_v(V, NV),
    add_v(TempPosition1, NV, TempPosition2),
    normalize(Transforms, [(S0, S1) | Previous], TempPosition2, scanner(S1, Beacons2), scanner(S, Beacons), NPosition).

normalize_all(Scanners, Positions, Beacons) :-
    alignements(Scanners, Transforms),
    maplist(normalize(Transforms, [], p(0,0,0)), Scanners, NormalizedScanners, Positions),
    setof(Beacon, (Beacon, Beacons1)^(
        member(scanner(0, Beacons1), NormalizedScanners),
        member(Beacon, Beacons1)
    ), Beacons), !.

manhattan_dist(p(X1,Y1,Z1), p(X2,Y2,Z2), Dist) :-
    Dist is abs(X1-X2) + abs(Y1-Y2) + abs(Z1-Z2).

p1 :-
    read_scanners(Scanners),
    normalize_all(Scanners, _, Beacons),
    length(Beacons, NBeacons),
    write_answer(NBeacons).

p2 :-
    read_scanners(Scanners),
    normalize_all(Scanners, Positions, _),
    aggregate(max(Dist), (P1, P2, Dist)^(
        member(P1, Positions),
        member(P2, Positions),
        manhattan_dist(P1, P2, Dist)    
    ), MaxDist),
    write_answer(MaxDist).

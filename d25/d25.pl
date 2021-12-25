:- module(d25, []).
:- use_module("common/util.pl").
:- use_module(library(clpfd)).

read_position('.', empty).
read_position('>', right).
read_position('v', down).

read_map(Map) :-
    read_grid(Grid),
    maplist(maplist(read_position), Grid, Map).

positions(Map, Positions) :-
    length(Map, Height),
    Map = [Line | _],
    length(Line, Width),
    MaxX is Width - 1,
    MaxY is Height - 1,
    numlist(0, MaxX, Xs),
    numlist(0, MaxY, Ys),
    length(XMap, Height),
    maplist(=(Xs), XMap),
    length(TYMap, Width),
    maplist(=(Ys), TYMap),
    transpose(TYMap, YMap),
    maplist(maplist([X, Y, p(X,Y)]>>(true)), XMap, YMap, Positions).

at(Map, p(X,Y), Thing) :-
    Map = [Line0 | _],
    length(Map, Height),
    length(Line0, Width),
    X1 is X mod Width,
    Y1 is Y mod Height,
    nth0(Y1, Map, Line),
    nth0(X1, Line, Thing).

move_right(Map, p(X,Y), right, empty) :-
    X1 is X + 1,
    at(Map, p(X1,Y), empty).
move_right(Map, p(X,Y), right, right) :-
    X1 is X + 1,
    \+ at(Map, p(X1,Y), empty).
move_right(Map, p(X,Y), empty, right) :-
    X1 is X - 1,
    at(Map, p(X1,Y), right).
move_right(Map, p(X,Y), empty, empty) :-
    X1 is X - 1,
    \+ at(Map, p(X1,Y), right).
move_right(_, _, down, down).

move_down(Map, p(X,Y), down, empty) :-
    Y1 is Y + 1,
    at(Map, p(X,Y1), empty).
move_down(Map, p(X,Y), down, down) :-
    Y1 is Y + 1,
    \+ at(Map, p(X,Y1), empty).
move_down(Map, p(X,Y), empty, down) :-
    Y1 is Y - 1,
    at(Map, p(X,Y1), down).
move_down(Map, p(X,Y), empty, empty) :-
    Y1 is Y  - 1,
    \+ at(Map, p(X,Y1), down).
move_down(_, _, right, right).

move_all(Direction, Map0, Map1) :-
    positions(Map0, Positions),
    maplist(maplist(call(Direction, Map0)), Positions, Map0, Map1), !.

stabilize(Tries, TotalTries, Map) :-
    move_all(move_right, Map, Map1),
    move_all(move_down, Map1, Map2),
    (Map = Map2 -> TotalTries is Tries + 1 ; (
        Tries1 is Tries + 1,
        stabilize(Tries1, TotalTries, Map2)    
    )).
stabilize(Map, TotalTries) :-
    stabilize(0, TotalTries, Map).

write_cucumber(empty) :- write(".").
write_cucumber(down) :- write("v").
write_cucumber(right) :- write(">").

write_line(Line) :-
    maplist(write_cucumber, Line), nl.

write_map(Map) :-
    maplist(write_line, Map), nl.

p1 :-
    read_map(Map),
    stabilize(Map, Tries),
    write_answer(Tries).

p2 :-
    write_answer("Christmas is saved!").
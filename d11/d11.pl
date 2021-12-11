:- use_module("common/util.pl").

read_octopuses(OctopusMatrix) :-
    read_grid(Grid),
    maplist(maplist(char_number), Grid, OctopusMatrix).

adj((X, Y), (X1, Y1)) :-
    (X1 is X - 1 ; X1 is X + 1 ; X1 is X),
    (Y1 is Y - 1 ; Y1 is Y + 1 ; Y1 is Y),
    (X, Y) \= (X1, Y1).

in_matrix(Matrix, Point) :-
    get(Matrix, Point, _).

get(Matrix, (X, Y), Value) :-
    nth0(Y, Matrix, Line),
    nth0(X, Line, Value).

inc(Matrix, (X, Y), NewMatrix) :-
    get(Matrix, (X, Y), Value),
    NewValue is Value + 1,
    nth0(Y, Matrix, Line),
    replace(X, NewValue, Line, NewLine),
    replace(Y, NewLine, Matrix, NewMatrix).

inc_list([], Matrix, Matrix) :- !.
inc_list([Point | Points], Matrix, NewMatrix) :-
    inc(Matrix, Point, TempMatrix),
    inc_list(Points, TempMatrix, NewMatrix).

tozero(Matrix, (X, Y), NewMatrix) :-
    nth0(Y, Matrix, Line),
    replace(X, 0, Line, NewLine),
    replace(Y, NewLine, Matrix, NewMatrix).

tozero_list([], Matrix, Matrix) :- !.
tozero_list([Point | Points], Matrix, NewMatrix) :-
    tozero(Matrix, Point, TempMatrix),
    tozero_list(Points, TempMatrix, NewMatrix).

increment_energy(Matrix, NewMatrix) :-
    maplist(maplist(calc(+, 1)), Matrix, NewMatrix).

flash_point(Point, Matrix, NewMatrix) :-
    findall(Adj, (adj(Point, Adj), in_matrix(Matrix, Adj)), Adjs),
    inc_list(Adjs, Matrix, NewMatrix).

flash_list([], Matrix, Matrix) :- !.
flash_list([Point | Points], Matrix, NewMatrix) :-
    flash_point(Point, Matrix, TempMatrix),
    flash_list(Points, TempMatrix, NewMatrix).

flash(AlreadyFlashed, Matrix, Matrix, TotalFlashes) :-
    findall(Point, (
        get(Matrix, Point, Value),
        Value > 9,
        \+ member(Point, AlreadyFlashed)
    ), []), !,
    length(AlreadyFlashed, TotalFlashes).
flash(AlreadyFlashed, Matrix, NewMatrix, TotalFlashes) :-
    findall(Point, (
        get(Matrix, Point, Value),
        Value > 9,
        \+ member(Point, AlreadyFlashed)
    ), ToFlash),
    flash_list(ToFlash, Matrix, TempMatrix),
    append(AlreadyFlashed, ToFlash, NewFlashed),
    flash(NewFlashed, TempMatrix, NewMatrix, TotalFlashes).

clear_flashed(Matrix, NewMatrix) :-
    findall(Point, (
        get(Matrix, Point, Value),
        Value > 9    
    ), ToClear),
    tozero_list(ToClear, Matrix, NewMatrix).

step(Matrix, NewMatrix, Flashes) :-
    increment_energy(Matrix, IncMatrix),
    flash([], IncMatrix, FlashedMatrix, Flashes),
    clear_flashed(FlashedMatrix, NewMatrix).

steps(0, Matrix, Matrix, N, N) :- !.
steps(S, Matrix, NewMatrix, N, TotalFlashes) :-
    S1 is S - 1,
    step(Matrix, TempMatrix, Flashes),
    N1 is N + Flashes,
    steps(S1, TempMatrix, NewMatrix, N1, TotalFlashes).

synchronize(Matrix, S, S) :- forall(get(Matrix, _, Value), Value == 0), !.
synchronize(Matrix, Step, TotalSteps) :-
    step(Matrix, NewMatrix, _),
    Step1 is Step + 1,
    synchronize(NewMatrix, Step1, TotalSteps).

p1 :-
    read_octopuses(Matrix),
    steps(100, Matrix, _, 0, TotalFlashes),
    writeln(TotalFlashes).

p2 :-
    read_octopuses(Matrix),
    synchronize(Matrix, 0, Steps),
    writeln(Steps).

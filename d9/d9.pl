:- module(d9, []).
:- use_module("common/util.pl").

%! read_heightmap(-Heightmap)
read_heightmap(Heightmap) :-
    read_grid(Grid),
    maplist(maplist(char_number), Grid, Heightmap).

%! get_point(+Heightmap, +Point, -Value)
get_point(Heightmap, (X, Y), Value) :-
    nth0(Y, Heightmap, Line),
    nth0(X, Line, Value).

%! adjacent(+Point, -Adjacent)
adjacent((X, Y), (X1, Y)) :-
    X1 is X + 1.
adjacent((X, Y), (X1, Y)) :-
    X1 is X - 1.
adjacent((X, Y), (X, Y1)) :-
    Y1 is Y + 1.
adjacent((X, Y), (X, Y1)) :-
    Y1 is Y - 1.

%! point_in_heightmap(+Heightmap, +Point)
point_in_heightmap(Heightmap, Point) :- get_point(Heightmap, Point, _).

%! low_point(+Heightmap, ?Point, -Value)
low_point(Heightmap, Point, Value) :-
    point_in_heightmap(Heightmap, Point),
    get_point(Heightmap, Point, Value),
    findall(Adj, (
        adjacent(Point, Adj),
        point_in_heightmap(Heightmap, Adj)
    ), Adjacents),
    maplist(get_point(Heightmap), Adjacents, Values),
    maplist(<(Value), Values).

%! expand_basin(+Heightmap, +Basin, -CompleteBasin)
expand_basin(Heightmap, Basin, CompleteBasin) :-
    findall(Adj, (
        member(Point, Basin),
        adjacent(Point, Adj),
        get_point(Heightmap, Adj, V),
        V =\= 9
    ), Adjacents),
    append([Basin, Adjacents], NewBasinUnsrt),
    sort(NewBasinUnsrt, NewBasin),
    (NewBasin = Basin -> CompleteBasin = NewBasin ; expand_basin(Heightmap, NewBasin, CompleteBasin)).

%! find_basin(+Heightmap, +Point, -Basin)
find_basin(Heightmap, Point, Basin) :-
    expand_basin(Heightmap, [Point], Basin).

p1 :- 
    read_heightmap(Heightmap),
    findall(Value, low_point(Heightmap, _, Value), Lowpoints),
    maplist(calc(+, 1), Lowpoints, RiskLevels),
    sum_list(RiskLevels, TotalRisk),
    write_answer(TotalRisk).

p2 :-
    read_heightmap(Heightmap),
    findall(Point, low_point(Heightmap, Point, _), Lowpoints),
    maplist(find_basin(Heightmap), Lowpoints, Basins),
    maplist(length, Basins, BasinSizes),
    msort(BasinSizes, SortedSizes),
    append([_, [V1, V2, V3]], SortedSizes),
    V is V1 * V2 * V3,
    write_answer(V).

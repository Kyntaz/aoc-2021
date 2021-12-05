:- use_module("common/util.pl").

/** 
 * string_to_number_pair(+Str, -Pair)
*/
string_to_number_pair(Str, (X, Y)) :-
    split_string(Str, ",", "", [XStr, YStr]),
    number_string(X, XStr),
    number_string(Y, YStr).

/** 
 * line_to_vent(+Line, -Vent)
*/
line_to_vent(Line, vent(Start, End)) :-
    split_string(Line, " ", " ", [Str1, "->", Str2]),
    string_to_number_pair(Str1, Start),
    string_to_number_pair(Str2, End).

/** 
 * read_vents(-Vents)
*/
read_vents(Vents) :-
    read_non_empty_lines(Lines),
    maplist(line_to_vent, Lines, Vents).

/** 
 * xy(?X, ?Y, ?Pair)
 * yx(?Y, ?X, ?Pair)
*/
xy(X, Y, (X, Y)).
yx(Y, X, (X, Y)).

/** 
 * vent_points(+Diags, +Vent, -Points)
*/
vent_points(_, vent((X1, Y), (X2, Y)), Points) :- !,
    (X1 < X2 -> numlist(X1, X2, Xs) ; numlist(X2, X1, Xs)),
    maplist(yx(Y), Xs, Points).
vent_points(_, vent((X, Y1), (X, Y2)), Points) :- !,
    (Y1 < Y2 -> numlist(Y1, Y2, Ys) ; numlist(Y2, Y1, Ys)),
    maplist(xy(X), Ys, Points).
vent_points(true, vent((X1, Y1), (X2, Y2)), Points) :-
    (X1 < X2 -> numlist(X1, X2, Xs) ; (
        numlist(X2, X1, RXs),
        reverse(RXs, Xs)
    )),
    (Y1 < Y2 -> numlist(Y1, Y2, Ys) ; (
        numlist(Y2, Y1, RYs),
        reverse(RYs, Ys)
    )),
    maplist(xy, Xs, Ys, Points), !.
vent_points(_, _, []).

/** 
 * mark_point(+Point, +Map, -NewMap)
*/
mark_point((X, Y), Map, NewMap) :-
    nth0(Y, Map, Line),
    nth0(X, Line, N),
    N1 is N + 1,
    replace(X, N1, Line, NewLine),
    replace(Y, NewLine, Map, NewMap).

/** 
 * mark_points(+Points, +Map, -NewMap)
*/
mark_points([], Map, Map).
mark_points([Point | Points], Map, NewMap) :-
    mark_point(Point, Map, TempMap),
    mark_points(Points, TempMap, NewMap).

/** 
 * mark_vent(+Vent, +Map, -NewMap)
*/
mark_vent(Diags, Vent, Map, NewMap) :-
    vent_points(Diags, Vent, Points),
    mark_points(Points, Map, NewMap).

/** 
 * mark_vents(+Vents, +Map, -NewMap)
*/
mark_vents(_, [], Map, Map).
mark_vents(Diags, [Vent | Vents], Map, NewMap) :-
    mark_vent(Diags, Vent, Map, TempMap),
    mark_vents(Diags, Vents, TempMap, NewMap).

/** 
 * make_map(+Vents, -Map)
*/
make_map(Vents, Map) :-
    findall(X, member(vent((X, _), _), Vents), Xs1),
    findall(X, member(vent(_, (X, _)), Vents), Xs2),
    max_list(Xs1, MaxX1),
    max_list(Xs2, MaxX2),
    W is max(MaxX1, MaxX2) + 1,
    findall(Y, member(vent((_, Y), _), Vents), Ys1),
    findall(Y, member(vent(_, (_, Y)), Vents), Ys2),
    max_list(Ys1, MaxY1),
    max_list(Ys2, MaxY2),
    H is max(MaxY1, MaxY2) + 1,
    length(Map, H),
    maplist(length_alt(W), Map),
    maplist(maplist(=(0)), Map).

p1 :-
    read_vents(Vents),
    make_map(Vents, EmptyMap),
    mark_vents(false, Vents, EmptyMap, Map),
    findall(N, (
        member(Line, Map),
        member(N, Line),
        N >= 2
    ), Ns),
    length(Ns, Overlaps),
    writeln(Overlaps).

p2 :-
    read_vents(Vents),
    make_map(Vents, EmptyMap),
    mark_vents(true, Vents, EmptyMap, Map),
    findall(N, (
        member(Line, Map),
        member(N, Line),
        N >= 2
    ), Ns),
    length(Ns, Overlaps),
    writeln(Overlaps).

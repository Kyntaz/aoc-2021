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
 * vent_points(+Diags, +Vent, -Points)
*/
vent_points(_, vent((X1, Y), (X2, Y)), Points) :- !,
    (X1 < X2 -> numlist(X1, X2, Xs) ; numlist(X2, X1, Xs)),
    maplist([X,(X,Y)]>>(true), Xs, Points).
vent_points(_, vent((X, Y1), (X, Y2)), Points) :- !,
    (Y1 < Y2 -> numlist(Y1, Y2, Ys) ; numlist(Y2, Y1, Ys)),
    maplist([Y,(X,Y)]>>(true), Ys, Points).
vent_points(true, vent((X1, Y1), (X2, Y2)), Points) :-
    (X1 < X2 -> numlist(X1, X2, Xs) ; (
        numlist(X2, X1, RXs),
        reverse(RXs, Xs)
    )),
    (Y1 < Y2 -> numlist(Y1, Y2, Ys) ; (
        numlist(Y2, Y1, RYs),
        reverse(RYs, Ys)
    )),
    maplist([X,Y,(X,Y)]>>(true), Xs, Ys, Points), !.
vent_points(_, _, []).

/** 
 * mark_point(+Point, +Map, -NewMap)
*/
mark_point(Point, Map, NewMap) :-
    (get_assoc(Point, Map, V) ; V = 0),
    V1 is V + 1,
    put_assoc(Point, Map, V1, NewMap).

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

p1 :-
    read_vents(Vents),
    empty_assoc(EmptyMap),
    mark_vents(false, Vents, EmptyMap, Map),
    aggregate(count, (N, K)^(
        gen_assoc(K, Map, N),
        N >= 2
    ), Overlaps),
    writeln(Overlaps).

p2 :-
    read_vents(Vents),
    empty_assoc(EmptyMap),
    mark_vents(true, Vents, EmptyMap, Map),
    aggregate(count, (N, K)^(
        gen_assoc(K, Map, N),
        N >= 2
    ), Overlaps),
    writeln(Overlaps).

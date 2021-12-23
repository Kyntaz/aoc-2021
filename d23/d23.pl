:- module(d23, []).
:- use_module("common/util.pl").
:- dynamic cost_cache/2.
:- dynamic depth_levels/1.

%! str_kind(?Str, ?Kind)
str_kind("A", a).
str_kind("B", b).
str_kind("C", c).
str_kind("D", d).

%! read_shrimps(+Level1, +Level2, -Shrimps)
read_shrimps(Level1, Level2, Shrimps) :-
    Shrimps = [
        shrimp(K1, pos(a, Level1)),
        shrimp(K2, pos(a, Level2)),
        shrimp(K3, pos(b, Level1)),
        shrimp(K4, pos(b, Level2)),
        shrimp(K5, pos(c, Level1)),
        shrimp(K6, pos(c, Level2)),
        shrimp(K7, pos(d, Level1)),
        shrimp(K8, pos(d, Level2))    
    ],
    read_non_empty_lines([_, _, Line1, Line2, _]),
    split_string(Line1, "#", "#", [S1, S2, S3, S4]),
    split_string(Line2, "#", "#", [_, S5, S6, S7, S8]),
    maplist(str_kind, [S1,S2,S3,S4,S5,S6,S7,S8], [K1,K2,K3,K4,K5,K6,K7,K8]).

%! position(?Pos)
position(pos(Kind, Level)) :-
    member(Kind, [a,b,c,d]),
    depth_levels(Depths),
    member(Level, Depths).
position(pos(h, Level)) :-
    member(Level, [1,2,3,4,5,6,7,8,9,10,11]).

%! connected(?Pos1, ?Pos2)
:- table connected/2.
connected(pos(a, 2), pos(a, 1)).
connected(pos(a, 3), pos(a, 2)).
connected(pos(a, 4), pos(a, 3)).
connected(pos(b, 2), pos(b, 1)).
connected(pos(b, 3), pos(b, 2)).
connected(pos(b, 4), pos(b, 3)).
connected(pos(c, 2), pos(c, 1)).
connected(pos(c, 3), pos(c, 2)).
connected(pos(c, 4), pos(c, 3)).
connected(pos(d, 2), pos(d, 1)).
connected(pos(d, 3), pos(d, 2)).
connected(pos(d, 4), pos(d, 3)).
connected(pos(h, 1), pos(h, 2)).
connected(pos(h, 2), pos(h, 3)).
connected(pos(h, 3), pos(h, 4)).
connected(pos(h, 4), pos(h, 5)).
connected(pos(h, 5), pos(h, 6)).
connected(pos(h, 6), pos(h, 7)).
connected(pos(h, 7), pos(h, 8)).
connected(pos(h, 8), pos(h, 9)).
connected(pos(h, 9), pos(h, 10)).
connected(pos(h, 10), pos(h, 11)).
connected(pos(a, 1), pos(h, 3)).
connected(pos(b, 1), pos(h, 5)).
connected(pos(c, 1), pos(h, 7)).
connected(pos(d, 1), pos(h, 9)).
connected(Pos1, Pos2) :- connected(Pos2, Pos1).

%! stable(+Shrimps, +Shrimp)
% The shrimp can stop at this position.
stable(_, shrimp(_, Pos)) :-
    StablePositions = [
        pos(h, 1), pos(h, 2), pos(h, 4), pos(h, 6),
        pos(h, 8), pos(h, 10), pos(h, 11)
    ],
    member(Pos, StablePositions).
stable(Shrimps, Shrimp) :- done(Shrimps, Shrimp).

%! done(+Shrimps, +Shrimp)
% The shrimp is in its final position.
done(Shrimps, shrimp(Kind, pos(Kind, Level))) :-
    forall((
        position(pos(Kind, Level2)),
        Level2 > Level
    ), member(shrimp(Kind, pos(Kind, Level2)), Shrimps)).

%! free(+Shrimps, +Pos)
free(Shrimps, Pos) :-
    \+ member(shrimp(_, Pos), Shrimps).

%! distance(+Shrimps, +Visited, +Pos1, +Pos2, -Distance)
:- table distance/4.
distance(_, _, Pos, Pos, 0) :- !.
distance(Shrimps, Visited, Pos2, Pos1, Dist) :-
    findall(NextPos, (
        connected(Pos1, NextPos),
        free(Shrimps, NextPos),
        \+ member(NextPos, Visited)
    ), Possibilities),
    (Possibilities = [] -> Dist = inf ; 
        maplist(distance(Shrimps, [Pos1 | Visited], Pos2), Possibilities, Distances),
        min_list(Distances, PartialDistance),
        (PartialDistance =:= inf -> Dist = inf ; Dist is PartialDistance + 1)
    ).

%! cost(?Kins, ?Cost)
cost(a, 1).
cost(b, 10).
cost(c, 100).
cost(d, 1000).

%! move(+Shrimps, +Shrimp1, -Shrimp2, -Cost)
% Shrimp1 can move becoming Shrimp2 for Cost.
move(Shrimps, shrimp(Kind, Pos1), shrimp(Kind, Pos2), Cost) :-
    position(Pos1),
    position(Pos2),
    (Pos1 \= pos(h, _) ; Pos2 \= pos(h, _)),
    \+ done(Shrimps, shrimp(Kind, Pos1)),
    stable(Shrimps, shrimp(Kind, Pos2)),
    distance(Shrimps, [], Pos2, Pos1, Dist),
    Dist =\= inf,
    cost(Kind, BaseCost),
    Cost is BaseCost * Dist.

%! all_done(+Shrimps)
all_done(Shrimps) :-
    forall(member(Shrimp, Shrimps), done(Shrimps, Shrimp)).

%! move_something(+Shrimps1, -Shrimps2, -Cost)
% We can move a shrimp from Shrimps1, turning it into Shrimps2,
% for Cost.
move_something(Shrimps1, Shrimps2, Cost) :-
    member(Shrimp1, Shrimps1),
    move(Shrimps1, Shrimp1, Shrimp2, Cost),
    select(Shrimp1, Shrimps1, Shrimp2, Shrimps2),
    Shrimps1 \= Shrimps2.

%! cost_cached(?Shrimps, ?Cost)
% There is a cache entry saying that it costs at least Cost
% to organize these shrimps.
cost_cached(Shrimps, Cost) :- cost_cache(Shrimps, Cost).

%! cache_cost(+Shrimps, +Cost)
% Cache this cost as the minimum cost to organize these shrimps.
cache_cost(Shrimps, Cost) :- assertz(cost_cache(Shrimps, Cost)).

%! organize(+Visited, +Shrimps, -Cost)
% Cost is the minimum cost to organize the shrimps,
% avoiding already visited combinations of shrimps.
organize(_, Shrimps, Cost) :- cost_cached(Shrimps, Cost), !.
organize(_, Shrimps, 0) :- all_done(Shrimps), !.
organize(Visited, Shrimps, Cost) :-
    setof(next(CostTo, NextShrimps), (CostTo, NextShrimps)^(
        move_something(Shrimps, NextShrimps, CostTo),
        \+ member(NextShrimps, Visited)
    ), Possibilities),
    maplist({Visited}/[next(CostTo, NextShrimps), TotalCost]>>(
        organize([Shrimps | Visited], NextShrimps, CostFrom),
        (CostFrom =:= inf -> TotalCost = inf ; TotalCost is CostFrom + CostTo)
    ), Possibilities, Costs),
    min_list(Costs, Cost),
    cache_cost(Shrimps, Cost), !.
organize(_, _, inf).

p1 :-
    assertz(depth_levels([1,2])),
    read_shrimps(1, 2, Shrimps),
    organize([], Shrimps, Cost),
    write_answer(Cost).

p2 :-
    ExtraShrimps = [
        shrimp(d, pos(a,2)),
        shrimp(d, pos(a,3)),
        shrimp(c, pos(b,2)),
        shrimp(b, pos(b,3)),
        shrimp(b, pos(c,2)),
        shrimp(a, pos(c,3)),
        shrimp(a, pos(d,2)),
        shrimp(c, pos(d,3))
    ],
    assertz(depth_levels([1,2,3,4])),
    read_shrimps(1, 4, ReadShrimps),
    append(ReadShrimps, ExtraShrimps, Shrimps),
    organize([], Shrimps, Cost),
    write_answer(Cost).

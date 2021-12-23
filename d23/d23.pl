:- module(d23, []).
:- use_module("common/util.pl").
:- dynamic cost_cache/2.
:- dynamic depth_levels/1.
:- dynamic visited/2.
:- dynamic closed/1.

position(pos(Kind, Level)) :-
    member(Kind, [a,b,c,d]),
    depth_levels(Depths),
    member(Level, Depths).
position(pos(h, Level)) :-
    member(Level, [1,2,3,4,5,6,7,8,9,10,11]).

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

stable(_, shrimp(_, Pos)) :-
    StablePositions = [
        pos(h, 1), pos(h, 2), pos(h, 4), pos(h, 6),
        pos(h, 8), pos(h, 10), pos(h, 11)
    ],
    member(Pos, StablePositions).
stable(Shrimps, Shrimp) :- done(Shrimps, Shrimp).

done(Shrimps, shrimp(Kind, pos(Kind, Level))) :-
    forall((
        position(pos(Kind, Level2)),
        Level2 > Level
    ), member(shrimp(Kind, pos(Kind, Level2)), Shrimps)).

free(Shrimps, Pos) :-
    \+ member(shrimp(_, Pos), Shrimps).

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

cost(a, 1).
cost(b, 10).
cost(c, 100).
cost(d, 1000).

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

all_done(Shrimps) :-
    forall(member(Shrimp, Shrimps), done(Shrimps, Shrimp)).

move_something(Shrimps1, Shrimps2, Cost) :-
    member(Shrimp1, Shrimps1),
    move(Shrimps1, Shrimp1, Shrimp2, Cost),
    select(Shrimp1, Shrimps1, Shrimp2, Shrimps2),
    Shrimps1 \= Shrimps2.

cost_cached(Shrimps, Cost) :- cost_cache(Shrimps, Cost).

cache_cost(Shrimps, Cost) :- assertz(cost_cache(Shrimps, Cost)).

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

greedy_strat(_, Shrimps, 0) :- all_done(Shrimps), !.
greedy_strat(Visited, Shrimps, Cost) :-
    setof(next(CostTo, NextShrimps), (CostTo, NextShrimps)^(
        move_something(Shrimps, NextShrimps, CostTo),
        \+ member(NextShrimps, Visited)
    ), Possibilities),
    member(next(CostTo, NextShrimps), Possibilities),
    organize([Shrimps | Visited], NextShrimps, CostFrom),
    (CostFrom =:= inf -> Cost = inf ; Cost is CostFrom + CostTo).
greedy_strat(_, _, inf).

visit(Shrimps, _) :- closed(Shrimps), !.
visit(Shrimps, Cost) :-
    visited(Shrimps, Cost0),
    Cost0 =< Cost, !.
visit(Shrimps, Cost) :-
    visited(Shrimps, Cost0),
    Cost0 > Cost, !,
    retract(visited(Shrimps, Cost0)),
    assertz(visited(Shrimps, Cost)).
visit(Shrimps, Cost) :- assertz(visited(Shrimps, Cost)).

leave(Shrimps) :-
    retract(visited(Shrimps, _)),
    assertz(closed(Shrimps)).

min_visited(Shrimps, Cost) :-
    aggregate_all(min(Cost1, Shrimps1), visited(Shrimps1, Cost1), min(Cost, Shrimps)).

expand(BaseCost, Shrimps) :-
    findall(next(NextShrimps, CostTo), move_something(Shrimps, NextShrimps, CostTo), Possibilities),
    maplist({BaseCost}/[next(NextShrimps, CostTo)]>>(
        TotalCost is BaseCost + CostTo,
        visit(NextShrimps, TotalCost)
    ), Possibilities).

dijkstra(Cost) :-
    visited(Shrimps, Cost),
    all_done(Shrimps), !.
dijkstra(Cost) :-
    min_visited(Shrimps, BaseCost),
    write_debug("Shrimps", Shrimps),
    expand(BaseCost, Shrimps),
    leave(Shrimps),
    dijkstra(Cost).

start_dijkstra(Shrimps, Cost) :-
    visit(Shrimps, 0),
    dijkstra(Cost).

p1 :-
    Shrimps = [
        shrimp(a, pos(a,1)),
        shrimp(d, pos(a,2)),
        shrimp(c, pos(b,1)),
        shrimp(d, pos(b,2)),
        shrimp(b, pos(c,1)),
        shrimp(a, pos(c,2)),
        shrimp(b, pos(d,1)),
        shrimp(c, pos(d,2))    
    ],
    assertz(depth_levels([1,2])),
    start_dijkstra(Shrimps, Cost),
    write_answer(Cost).

p2 :-
    Shrimps = [
        shrimp(a, pos(a,1)),
        shrimp(d, pos(a,2)),
        shrimp(d, pos(a,3)),
        shrimp(d, pos(a,4)),
        shrimp(c, pos(b,1)),
        shrimp(c, pos(b,2)),
        shrimp(b, pos(b,3)),
        shrimp(d, pos(b,4)),
        shrimp(b, pos(c,1)),
        shrimp(b, pos(c,2)),
        shrimp(a, pos(c,3)),
        shrimp(a, pos(c,4)),
        shrimp(b, pos(d,1)),
        shrimp(a, pos(d,2)),
        shrimp(c, pos(d,3)),
        shrimp(c, pos(d,4)) 
    ],
    assertz(depth_levels([1,2,3,4])),
    organize([], Shrimps, Cost),
    write_answer(Cost).

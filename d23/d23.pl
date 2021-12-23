:- module(d23, []).
:- use_module("common/util.pl").
:- dynamic cheapest_organize_cache/2.

read_input(s([A1, A2], [B1, B2], [C1, C2], [D1, D2], Hallway)) :-
    read_non_empty_lines([_, _, Line1, Line2, _]),
    split_string(Line1, "#", "#", [A1, B1, C1, D1]),
    split_string(Line2, "#", "#", [_, A2, B2, C2, D2]),
    length(Hallway, 7),
    maplist(=(empty), Hallway).

cost("A", 1).
cost("B", 10).
cost("C", 100).
cost("D", 1000).

canonical_state(s([a1,a2],[b1,b2],[c1,c2],[d1,d2],[h1,h2,h3,h4,h5,h6,h7])).

pos_id(a1, 1).
pos_id(a2, 2).
pos_id(b1, 3).
pos_id(b2, 4).
pos_id(c1, 5).
pos_id(c2, 6).
pos_id(d1, 7).
pos_id(d2, 8).
pos_id(h1, 9).
pos_id(h2, 10).
pos_id(h3, 11).
pos_id(h4, 12).
pos_id(h5, 13).
pos_id(h6, 14).
pos_id(h7, 15).

corresponding_var(Vars, Pos, Var) :-
    pos_id(Pos, Id),
    nth1(Id, Vars, Var).

list_state(List, State) :-
    StateBody = [[_,_],[_,_],[_,_],[_,_],[_,_,_,_,_,_,_]],
    State =.. [s | StateBody],
    flatten(StateBody, List).

:- table to/3.
to(a1, h2, 2).
to(a1, h3, 2).
to(a2, a1, 1).
to(b1, h3, 2).
to(b1, h4, 2).
to(b2, b1, 1).
to(c1, h4, 2).
to(c1, h5, 2).
to(c2, c1, 1).
to(d1, h5, 2).
to(d1, h6, 2).
to(d2, d1, 1).
to(h1, h2, 1).
to(h2, h3, 2).
to(h3, h4, 2).
to(h4, h5, 2).
to(h5, h6, 2).
to(h6, h7, 1).
to(Pos1, Pos2, Cost) :- to(Pos2, Pos1, Cost).

room1(Pos) :- member(Pos, [a1, b1, c1, d1]).

room2(Pos) :- member(Pos, [a2, b2, c2, d2]).

hallway(Pos) :- member(Pos, [h1, h2, h3, h4, h5, h6, h7]).

:- table same_room/2.
same_room(a1, a2).
same_room(b1, b2).
same_room(c1, c2).
same_room(d1, d2).
same_room(R1, R2) :- same_room(R2, R1).

expected(a1, "A").
expected(a2, "A").
expected(b1, "B").
expected(b2, "B").
expected(c1, "C").
expected(c2, "C").
expected(d1, "D").
expected(d2, "D").


replace_state(State0, Pos1, Pos2, State1) :-
    list_state(List0, State0),
    canonical_state(Canon),
    list_state(CList, Canon),
    nth0(I1, CList, Pos1),
    nth0(I2, CList, Pos2),
    nth0(I1, List0, El1),
    nth0(I2, List0, El2),
    replace(I1, El2, List0, List1),
    replace(I2, El1, List1, List2),
    list_state(List2, State1).

state_pos(State, Pos, El) :-
    canonical_state(CState),
    list_state(CList, CState),
    list_state(List, State),
    nth0(I, CList, Pos),
    nth0(I, List, El).

possible_move(State0, State1, Pos1, Pos2, Cost, _) :-
    to(Pos1, Pos2, Cost),
    \+ state_pos(State0, Pos1, empty),
    state_pos(State0, Pos2, empty),
    replace_state(State0, Pos1, Pos2, State1), !.
possible_move(State0, State1, Pos1, Pos2, Cost, Between) :-
    to(Pos1, Pos3, PCost1),
    \+ member(Pos3, Between),
    \+ state_pos(State0, Pos1, empty),
    state_pos(State0, Pos3, empty),
    replace_state(State0, Pos1, Pos3, State2),
    possible_move(State2, State1, Pos3, Pos2, PCost2, [Pos3 | Between]),
    Cost is PCost1 + PCost2.

cheapest_move(State0, State1, Pos0, Pos1, MinCost) :-
    aggregate(min(Cost), (Cost)^possible_move(State0, State1, Pos0, Pos1, Cost, [Pos0]), MinCost).

valid_move(State0, State1, Cost) :-
    room1(Pos0),
    hallway(Pos1),
    state_pos(State0, Pos0, El),
    \+ (
        expected(Pos0, El),
        same_room(Pos0, OtherPos),
        state_pos(State0, OtherPos, El)
    ),
    cheapest_move(State0, State1, Pos0, Pos1, PCost),
    cost(El, Mult),
    Cost is PCost * Mult.
valid_move(State0, State1, Cost) :-
    room2(Pos0),
    hallway(Pos1),
    state_pos(State0, Pos0, El),
    \+ expected(Pos0, El),
    cheapest_move(State0, State1, Pos0, Pos1, PCost),
    cost(El, Mult),
    Cost is PCost * Mult.
valid_move(State0, State1, Cost) :-
    hallway(Pos0),
    room2(Pos1),
    state_pos(State0, Pos0, El),
    expected(Pos1, El),
    cheapest_move(State0, State1, Pos0, Pos1, PCost),
    cost(El, Mult),
    Cost is PCost * Mult.
valid_move(State0, State1, Cost) :-
    hallway(Pos0),
    room1(Pos1),
    state_pos(State0, Pos0, El),
    expected(Pos1, El),
    same_room(Pos1, OtherPos),
    state_pos(State1, OtherPos, El),
    cheapest_move(State0, State1, Pos0, Pos1, PCost),
    cost(El, Mult),
    Cost is PCost * Mult.

organized(State) :-
    forall(expected(Pos, Expected), state_pos(State, Pos, Expected)).

add_inf(N1, N2, inf) :-
    (N1 =:= inf ; N2 =:= inf), !.
add_inf(N1, N2, Add) :- Add is N1 + N2.

cheapest_organize(State, MinCost) :-
    cheapest_organize_cache(State, MinCost), !.
cheapest_organize(State, 0) :-
    organized(State), !.
cheapest_organize(State, MinCost) :-
    findall((State1, Cost), (
        valid_move(State, State1, Cost)
    ), Possibilities),
    maplist([(NewState, NewCost), NewState, NewCost]>>(true), Possibilities, PossibleStates, PossibleCosts),
    maplist(cheapest_organize, PossibleStates, FutureCosts),
    maplist(add_inf, FutureCosts, PossibleCosts, TotalCosts),
    (min_list(TotalCosts, MinCost) ; MinCost = inf),
    write_debug(MinCost),
    assertz(cheapest_organize_cache(State, MinCost)), !.

p1 :-
    read_input(State),
    cheapest_organize(State, Cost),
    write_answer(Cost).

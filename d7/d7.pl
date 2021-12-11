:- use_module("common/util.pl").

%! read_crabs(-Crabs)
read_crabs(Crabs) :-
    read_non_empty_lines([Line]),
    split_string(Line, ",", "", CrabsStr),
    maplist(number_string, Crabs, CrabsStr).

%! move_crab1(+End, +Start, -Fuel)
move_crab1(End, Start, Fuel) :-
    Fuel is abs(End - Start).

%! move_crab2(+End, +Start, -Fuel)
move_crab2(End, Start, Fuel) :-
    N is abs(End - Start),
    Fuel is N * (N + 1) / 2.

%! move_crabs(+Crabs, :Fn, +Target, -Fuel)
move_crabs(Crabs, Fn, Target, Fuel) :-
    maplist(call(Fn, Target), Crabs, Fuels),
    sum_list(Fuels, Fuel).

%! cheapest_target(+Crabs, :Fn, -Target, -Fuel)
cheapest_target(Crabs, Fn, Target, Fuel) :-
    min_list(Crabs, Min),
    max_list(Crabs, Max),
    numlist(Min, Max, Possible),
    findall((T, F), (
        member(T, Possible),
        move_crabs(Crabs, Fn, T, F)    
    ), PossibleTargetFuels),
    maplist([(T, F), F]>>(true), PossibleTargetFuels, PossibleFuels),
    min_list(PossibleFuels, Fuel),
    nth0(Idx, PossibleFuels, Fuel),
    nth0(Idx, PossibleTargetFuels, (Target, Fuel)).

p1 :-
    read_crabs(Crabs),
    cheapest_target(Crabs, move_crab1, _, Fuel),
    writeln(Fuel).

p2 :-
    read_crabs(Crabs),
    cheapest_target(Crabs, move_crab2, _, Fuel),
    writeln(Fuel).

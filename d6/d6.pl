:- module(d6, []).
:- use_module("common/util.pl").

%! count(+List, -Count)
count(List, N, Count) :-
    aggregate_all(count, member(N, List), Count).

%! read_lanternfish(-Counts)
read_lanternfish(Counts) :-
    read_non_empty_lines([Line]),
    split_string(Line, ",", "", StrtList),
    maplist(number_string, List, StrtList),
    numlist(0, 8, DaysList),
    maplist(count(List), DaysList, Counts).

%! * update_lanternfish(+Lanternfish, -NewLanternfish)
update_lanternfish(Lanternfish, NewLanternfish) :-
    append([[Zero], Other, [Six, Seven, Eight]], Lanternfish),
    NewSix is Zero + Seven,
    NewSeven is Eight,
    NewEight is Zero,
    append([Other, [Six, NewSix, NewSeven, NewEight]], NewLanternfish).

%! reproduce_for_days(+Days, +Lanternfish, -NewLanternfish)
reproduce_for_days(0, NewLanternfish, NewLanternfish) :- !.
reproduce_for_days(Days, Lanternfish, NewLanternfish) :-
    update_lanternfish(Lanternfish, TempLanternfish),
    Days1 is Days - 1,
    reproduce_for_days(Days1, TempLanternfish, NewLanternfish).

p1 :-
    read_lanternfish(Lanternfish),
    reproduce_for_days(80, Lanternfish, NewLanternfish),
    sum_list(NewLanternfish, NLanternfish),
    write_answer(NLanternfish).

p2 :-
    read_lanternfish(Lanternfish),
    reproduce_for_days(256, Lanternfish, NewLanternfish),
    sum_list(NewLanternfish, NLanternfish),
    write_answer(NLanternfish).

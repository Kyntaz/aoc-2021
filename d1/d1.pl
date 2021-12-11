:- use_module("common/util.pl").

%! read_measurements(-Measurements:list[number])
read_measurements(Measurements) :-
    read_non_empty_lines(Lines),
    maplist(number_string, Measurements, Lines).

%! increase(+Measurements:list[number], -V1:number, -V2:number) is indet
increase(Measurements, V1, V2) :-
    nextto(V1, V2, Measurements),
    V1 < V2.

%! sum_window(+Measurements:list[number], -Sum:number) is indet
sum_window(Measurements, Sum) :-
    length(Window, 3),
    append([_, Window, _], Measurements),
    sum_list(Window, Sum).

p1 :-
    read_measurements(Measurements),
    aggregate(count, (V1, V2)^increase(Measurements, V1, V2), Increments),
    writeln(Increments).

% We are taking advantage of the order in which append unifies a sub-list
p2 :-
    read_measurements(Measurements),
    findall(Sum, sum_window(Measurements, Sum), Coarse),
    aggregate(count, (V1, V2)^increase(Coarse, V1, V2), Increments),
    writeln(Increments).

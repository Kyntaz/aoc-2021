:- ["common/util.pl"].

/**
 * read_measurements(-Measurements:list[number]) is semidet
*/
read_measurements(Measurements) :-
    read_non_empty_lines(Lines),
    maplist(number_string, Measurements, Lines).

/**
 * increase(+Measurements:list[number], -V1:number, -V2:number) is indet
 * 
 *  V1 and V2 are adjacent measurements and V2 is greater than V1.
 */
increase(Measurements, V1, V2) :-
    nextto(V1, V2, Measurements),
    V1 < V2.

/** 
 * sum_window(+Measurements:list[number], -Sum:number) is indet
 * 
 * Sum is a sum of 3 consecutive measurements.
*/
sum_window(Measurements, Sum) :-
    length(Window, 3),
    append([_, Window, _], Measurements),
    sum_list(Window, Sum).

p1 :-
    read_measurements(Measurements),
    findall((V1, V2), increase(Measurements, V1, V2), Increments),
    length(Increments, L),
    writeln(L).

p2 :-
    read_measurements(Measurements),
    % We are taking advantage of the order in which append unifies a sub-list
    findall(Sum, sum_window(Measurements, Sum), Coarse),
    findall((V1, V2), increase(Coarse, V1, V2), Increments),
    length(Increments, L),
    writeln(L).

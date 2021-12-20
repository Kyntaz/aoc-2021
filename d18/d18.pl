:- module(d18, []).
:- use_module("common/util.pl").
:- use_module(library(dcg/basics)).

%! snailfish_number(-Number)//
snailfish_number(N) --> integer(N).
snailfish_number([N1, N2]) --> "[", snailfish_number(N1), ",", snailfish_number(N2), "]".

%! read_number(+Line, -Number)
read_number(Line, Number) :-
    string_codes(Line, LineCodes),
    phrase(snailfish_number(Number), LineCodes).

%! read_homework(-Numbers)
read_homework(Numbers) :-
    read_non_empty_lines(Lines),
    maplist(read_number, Lines, Numbers).

%! add(+Number1, +Number2, -Result)
add(Number1, Number2, Result) :-
    Result0 = [Number1, Number2],
    reduce(Result0, Result).

%! reduce(+Number, -Result)
% reuse, recycle.
reduce(Number, Result) :-
    explode(Number, Number1), !,
    reduce(Number1, Result).
reduce(Number, Result) :-
    split(Number, Number1), !,
    reduce(Number1, Result).
reduce(Number, Number).

%! explode(+Number, -Result, -Propagate, +Level)
explode([L,R], 0, [L,R], 4) :- !.
explode(Number, Result, Propagate, Level) :-
    Number = [LeftNumber, RightNumber],
    Level1 is Level + 1,
    explode(LeftNumber, Exploded, Propagate1, Level1), !,
    Propagate1 = [PLeft, PRight],
    (RightNumber = [A, B] -> (
        (inc(A, PRight, left, A1) ; A1 = A),
        Result = [Exploded, [A1, B]]
    ) ; (
        integer(RightNumber),
        RightNumber1 is RightNumber + PRight,
        Result = [Exploded, RightNumber1]
    )),
    Propagate = [PLeft, 0].
explode(Number, Result, Propagate, Level) :-
    Number = [LeftNumber, RightNumber],
    Level1 is Level + 1,
    explode(RightNumber, Exploded, Propagate1, Level1), !,
    Propagate1 = [PLeft, PRight],
    (LeftNumber = [A, B] -> (
        (inc(B, PLeft, right, B1) ; B1 = B),
        Result = [[A, B1], Exploded]
    ) ; (
        integer(LeftNumber),
        LeftNumber1 is LeftNumber + PLeft,
        Result = [LeftNumber1, Exploded]
    )),
    Propagate = [0, PRight].

%! inc(+Number, +Increment, +Direction, -Result)
inc([Left, Right], Inc, left, [Left1, Right]) :- !,
    inc(Left, Inc, left, Left1).
inc([Left, Right], Inc, right, [Left, Right1]) :- !,
    inc(Right, Inc, right, Right1).
inc(N, Inc, _, N1) :-
    integer(N),
    N1 is N + Inc.

%! explode(+Number, -Result)
explode(Number, Result) :- explode(Number, Result, _, 0).

%! split(+Number, -Result)
split([Left, Right], [Left1, Right]) :-
    split(Left, Left1), !.
split([Left, Right], [Left, Right1]) :-
    split(Right, Right1), !.
split(N, [Left, Right]) :-
    integer(N),
    N >= 10,
    Left is floor(N / 2),
    Right is ceiling(N / 2).

%! add_list(+Numbers, -Result)
add_list([Result], Result).
add_list([N1, N2 | Numbers], Result) :-
    add(N1, N2, Result1),
    add_list([Result1 | Numbers], Result).

%! magnitude(+Number, -Magnitude)
magnitude([Left, Right], Magnitude) :- !,
    magnitude(Left, MagLeft),
    magnitude(Right, MagRight),
    Magnitude is 3 * MagLeft + 2 * MagRight.
magnitude(N, N) :- integer(N), !.

%! add_2(+Numbers, -Magnitude) is multi
add_2(Numbers, Magnitude) :-
    member(N1, Numbers),
    member(N2, Numbers),
    N1 \= N2,
    add(N1, N2, Result),
    magnitude(Result, Magnitude).

p1 :-
    read_homework(Numbers),
    add_list(Numbers, Result),
    magnitude(Result, Magnitude),
    write_answer(Magnitude).

p2 :-
    read_homework(Numbers),
    aggregate(max(Magnitude), add_2(Numbers, Magnitude), MaxMagnitude),
    write_answer(MaxMagnitude).

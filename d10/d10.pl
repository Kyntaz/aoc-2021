:- use_module("common/util.pl").

%! read_code_lines(-Lines)
read_code_lines(Lines) :-
    read_non_empty_lines(Strings),
    maplist(string_chars, Strings, Lines).

%! match(?Opening, ?Closing)
match('(', ')').
match('[', ']').
match('{', '}').
match('<', '>').

%! is_open(?Char)
is_open(Char) :-
    member(Char, ['(', '[', '{', '<']).

%! is_close(?Char)
is_close(Char) :-
    member(Char, [')', ']', '}', '>']).

%! find_error(+CodeLine, +Stack, -Error)
find_error([], [], no_error) :- !.
find_error([], Stack, missing(Stack)) :- !.
find_error([Char | Chars], Stack, Error) :-
    is_open(Char), !,
    match(Char, Closing),
    find_error(Chars, [Closing | Stack], Error).
find_error([Char | Chars], [Char | Stack], Error) :-
    is_close(Char), !,
    find_error(Chars, Stack, Error).
find_error([Char | _], [OtherChar | _], error(OtherChar, Char)) :-
    is_close(Char),
    Char \= OtherChar, !.

%! points(?Char, ?Points)
points(')', 3).
points(']', 57).
points('}', 1197).
points('>', 25137).

%! points(?Char, ?Points)
completion_points(')', 1).
completion_points(']', 2).
completion_points('}', 3).
completion_points('>', 4).

%! calc_completion_points(++0, +MissingStack, -Points)
calc_completion_points(Points, [], Points).
calc_completion_points(TempPoints, [Char | Chars], Points) :-
    completion_points(Char, N),
    NewTempPoints is TempPoints * 5 + N,
    calc_completion_points(NewTempPoints, Chars, Points).

p1 :- 
    read_code_lines(Lines),
    findall(FoundInstead, (
        member(Line, Lines),
        find_error(Line, [], error(_, FoundInstead))    
    ), Errors),
    maplist(points, Errors, Points),
    sum_list(Points, Total),
    writeln(Total).

p2 :- 
    read_code_lines(Lines),
    findall(Missing, (
        member(Line, Lines),
        find_error(Line, [], missing(Missing))    
    ), Missings),
    maplist(calc_completion_points(0), Missings, Scores),
    msort(Scores, ScoresSrt),
    length(Scores, NScores),
    MiddleIdx is floor(NScores / 2),
    nth0(MiddleIdx, ScoresSrt, MiddleScore),
    writeln(MiddleScore).

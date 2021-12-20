:- module(d8, []).
:- use_module("common/util.pl").
:- dynamic segment_display/2.

%! segment_display(?Number, ?LitSegments)
segment_display(0, ['a', 'b', 'c', 'e', 'f', 'g']).
segment_display(1, ['c', 'f']).
segment_display(2, ['a', 'c', 'd', 'e', 'g']).
segment_display(3, ['a', 'c', 'd', 'f', 'g']).
segment_display(4, ['b', 'c', 'd', 'f']).
segment_display(5, ['a', 'b', 'd', 'f', 'g']).
segment_display(6, ['a', 'b', 'd', 'e', 'f', 'g']).
segment_display(7, ['a', 'c', 'f']).
segment_display(8, ['a', 'b', 'c', 'd', 'e', 'f', 'g']).
segment_display(9, ['a', 'b', 'c', 'd', 'f', 'g']).

%! assert_permutation(+Number, +Permutation)
% Asserts a new fact to the program knowledge.
assert_permutation(N, Perm) :- segment_display(N, Perm), !.
assert_permutation(N, Perm) :- assertz(segment_display(N, Perm)).

%! assert_permutations(+Number)
assert_permutations(N) :-
    segment_display(N, List), !,
    findall(Perm, permutation(List, Perm), Perms),
    maplist(assert_permutation(N), Perms).

%! assert_all_permutations
% Given the existing segment_display facts,
% asserts new facts for all permutations of segments.
assert_all_permutations :-
    numlist(0, 9, Ns),
    maplist(assert_permutations, Ns).

%! make_vars(-Vars)
% Creates a list of 7 unbound variables.
make_vars(Vars) :- length(Vars, 7).

%! read_entry(+Line, -Entry)
read_entry(Line, (Input, Output)) :-
    length(InputStrs, 10),
    length(OutputStrs, 4),
    append([InputStrs, ["|"], OutputStrs], SplitLine),
    split_string(Line, " ", " ", SplitLine),
    maplist(string_chars, InputStrs, Input),
    maplist(string_chars, OutputStrs, Output).

%! read_entries(-Entries)
read_entries(Entries) :-
    read_non_empty_lines(Lines),
    maplist(read_entry, Lines, Entries).

%! char_to_var(+VarList, +Char, -Var)
% Replaces a char of characters with a corresponding var.
char_to_var(VarList, Char, Var) :-
    char_code('a', A),
    char_code(Char, Code),
    Idx is Code - A,
    nth0(Idx, VarList, Var).

%! chars_to_vars(+VarList, +Char, -Var)
% Replaces a list of char with the corresponding vars.
chars_to_vars(VarList, Chars, Vars) :-
    maplist(char_to_var(VarList), Chars, Vars).

%! entry_to_nums(+Entry, -EntryNumbers)
% Converts an entry to single digit decimal numbers.
entry_to_nums((Input, Output), (InNums, OutNums)) :-
    make_vars(VarList),
    maplist(chars_to_vars(VarList), Input, InVars),
    maplist(segment_display, InNums, InVars),
    maplist(chars_to_vars(VarList), Output, OutVars),
    maplist(segment_display, OutNums, OutVars).

%! count_nums_in_entry(+Nums, +Entry, -Count)
count_nums_in_entry(Nums, (_, Output), Count) :-
    (aggregate(count, N^(member(N, Nums), member(N, Output)), Count) ; Count = 0).

%! digits_to_num(+Digits, ++0, -Num)
digits_to_num([], Num, Num).
digits_to_num([Dig | Digs], TempNum, Num) :-
    NewNum is TempNum * 10 + Dig,
    digits_to_num(Digs, NewNum, Num).

%! entry_to_num(+Entry, -Num)
% Calculates the output number of an entry.
entry_to_num((_, Output), Num) :-
    digits_to_num(Output, 0, Num).

p1 :-
    assert_all_permutations,
    read_entries(Entries),
    concurrent_maplist(entry_to_nums, Entries, Nums),
    maplist(count_nums_in_entry([1, 4, 7, 8]), Nums, Counts),
    sum_list(Counts, Count),
    write_answer(Count).

p2 :-
    assert_all_permutations,
    read_entries(Entries),
    concurrent_maplist(entry_to_nums, Entries, Digs),
    maplist(entry_to_num, Digs, Nums),
    sum_list(Nums, Sum),
    write_answer(Sum).

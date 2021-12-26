:- module(util, [
    read_lines/1,
    read_non_empty_lines/1,
    read_grid/1,
    read_blocks/1,
    calc/4,
    operatorium/3,
    summatorium/2,
    productorium/2,
    fact/2,
    remove_last/2,
    replace/4,
    no_duplicates/1,
    char_number/5,
    char_number/2,
    rev_base_chars_number/3,
    base_chars_number/3,
    base_string_number/3,
    write_answer/1,
    write_debug/1,
    write_debug/2
]).
:- use_module(library(clpfd)).
:- encoding(utf8).
:- set_prolog_flag(color_term, true).
:- set_stream(current_output, tty(true)).
:- set_stream(current_output, encoding(utf8)).
 
%! read_lines(-Lines:list[string]) is semidet
% Reads lines from the stdin as a list of strings.
read_lines(Lines) :-
    read_string(user_input, _, String),
    split_string(String, "\n", "", Lines), !.

%! read_non_empty_lines(-Lines:list[string]) is semidet 
% Reads lines from the stdin, discarding empty lines.
read_non_empty_lines(Lines) :-
    read_lines(AllLines),
    exclude(==(""), AllLines, Lines), !.

%! read_grid(-Grid:list[list[char]]) is semidet
% Reads a grid (list of lists) of characters from the stdin.
read_grid(Grid) :-
    read_non_empty_lines(Lines),
    maplist(string_chars, Lines, Grid), !.

%! read_blocks(-Blocks:list[list[string]]) is semidet
% Reads blocks of lines from the stdin.
% Each block is separated by an emppty line.
read_blocks(Blocks) :-
    read_lines(Lines),
    aux_read_blocks(Lines, [], [], Blocks).
aux_read_blocks(Lines, LastBlock, TempBlocks, Blocks) :-
    (Lines = [] ; Lines = [""]), !,
    append(TempBlocks, [LastBlock], Blocks).
aux_read_blocks([""|Lines], TempBlock, TempBlocks, Blocks) :- !,
    append(TempBlocks, [TempBlock], NewTempBlocks),
    aux_read_blocks(Lines, [], NewTempBlocks, Blocks).
aux_read_blocks([Line|Lines], TempBlock, TempBlocks, Blocks) :- !,
    append(TempBlock, [Line], NewTempBlock),
    aux_read_blocks(Lines, NewTempBlock, TempBlocks, Blocks).

%! calc(+Op:literal[*,+,-,/], +X:number, +Y:number, -Res:number) is det
% Applies the operation to X and Y, unifying Res with the result.
calc(*, X, Y, Res) :- Res is X * Y.
calc(+, X, Y, Res) :- Res is X + Y.
calc(-, X, Y, Res) :- Res is X - Y.
calc(/, X, Y, Res) :- Res is X / Y.

%! operatorium(+List:list[number], -Res:number, +Op:literal[*,+,-,/]) is det 
% Reduces a list of numbers with the given operation.
operatorium([X], X, _) :- !.
operatorium([X|Tail], Y, Op) :-
    operatorium(Tail, Z, Op),
    calc(Op, X, Z, Y).

 
%! summatorium(+List:list[number], -Res) is det
summatorium(L, X) :- operatorium(L, X, +).
 
%! productorium(+List:list[number], -Res) is det
productorium(L, X) :- operatorium(L, X, *).

%! fact(+N:number, -Res:number) is det
% Factorial of N.
fact(0, 1).
fact(N, Fact) :-
    N #> 0,
    N1 #= N - 1,
    Fact #= Fact1 * N,
    fact(N1, Fact1).

%! remove_last(+List:list[any], -Res:list[any]) is det
remove_last([_], []) :- !.
remove_last([X|Xs], [X|Ys]) :-
    remove_last(Xs, Ys).

%! replace(+Idx:number, +New:any, +List:list[any], -NewList:list[any]) is det
% Replace element at index Idx with the new element.
replace(_, _, [], []).
replace(0, E, [_|T], [E|T]).
replace(N, O, [E|T1], [E|T2]) :-
    N #> 0,
    N1 #= N - 1,
    replace(N1, O, T1, T2).

%! no_duplicates(+List) is semidet
% States that a list has no duplicate members.
no_duplicates(List) :-
    sort(List, Set),
    length(List, L),
    length(Set, L).

%! char_number(
%!     +Char:char,
%!     -N:number,
%!     +MinC:char,
%!     +MaxC:char,
%!     +Disp:number
%! ) is det
% Turns a character between MinC and MaxC into a number larger than Disp.
% Mostly useful as a helper for char_number/2.
char_number(Char, N, MinC, MaxC, Disp) :-
    char_code(MinC, Min),
    char_code(MaxC, Max),
    char_code(Char, Code),
    Code >= Min,
    Code =< Max,
    N is Code - Min + Disp.

%! char_number(+Char:char, -N:number) is det
% Chars between 0 and 9 become their corresponding number.
% Chars between a/A and z/Z become the corresponding number + 10 (mostly for hexadecimal).
char_number(Char, N) :- char_number(Char, N, '0', '9', 0).
char_number(Char, N) :- char_number(Char, N, 'a', 'z', 10).
char_number(Char, N) :- char_number(Char, N, 'A', 'Z', 10).

%! rev_base_chars_number(+Base:number, +Chars:list[char], -Number:number) is det
% Turns a reversed list of characters into the corresponding number in base Base.
% Mostly useful as a helper for base_chars_number.
rev_base_chars_number(_, [], 0).
rev_base_chars_number(Base, [Char | Chars], Number) :-
    rev_base_chars_number(Base, Chars, Number1),
    char_number(Char, N),
    Number is Number1 * Base + N.

%! base_chars_number(+Base:number, +Chars:list[chars], -Number:number) is det
% Turns a list of characters into the corresponding number in base Base.
base_chars_number(Base, Chars, Number) :-
    reverse(Chars, RevChars),
    rev_base_chars_number(Base, RevChars, Number).

%! base_string_number(+Base:number, +String:string, -Number:number) is det
% Turns a string into the corresponding number in base Base.
base_string_number(Base, String, Number) :-
    string_chars(String, Chars),
    base_chars_number(Base, Chars, Number).

%! answer(+Answer) is det
% Prints the answer to stdout.
write_answer(Answer) :-
    ansi_format([bold, fg8(green)], "[Answer]: ~w", [Answer]), nl.

%! debug(+Object) is det
% Prints something to the stdout as a debug line.
write_debug(Object) :-
    ansi_format(fg8(yellow), "[Debug]: ~W", [Object, []]), nl.

%! debug(+Message, +Object) is det
% Prints something to the stdout as a debug line.
write_debug(Message, Object) :-
    ansi_format(fg8(yellow), "[~w]: ~W", [Message, Object, []]), nl.

%! useful_traces
% Useful tracing settings for debug purposes.
useful_traces(Module) :-
    write_debug("Starting traces"),
    leash(-all),
    trace(Module:_).

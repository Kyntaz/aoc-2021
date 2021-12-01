% > Reading from stdin <

/** 
 * read_lines(-Lines:list[string]) is semidet
 * 
 * Reads lines from the stdin as a list of strings.
*/
read_lines(Lines) :-
    read_string(user_input, _, String),
    split_string(String, "\n", "", Lines), !.

/**
 * read_non_empty_lines(-Lines:list[string]) is semidet
 * 
 * Reads lines from the stdin, discarding empty lines.
 */
read_non_empty_lines(Lines) :-
    read_lines(AllLines),
    exclude(==(""), AllLines, Lines), !.

/**
 * read_grid(-Grid:list[list[char]]) is semidet
 * 
 * Reads a grid (list of lists) of characters from the stdin.
 */
read_grid(Grid) :-
    read_non_empty_lines(Lines),
    maplist(string_chars, Lines, Grid), !.

/** 
 * read_blocks(-Blocks:list[list[string]]) is semidet
 * 
 * Reads blocks of lines from the stdin.
 * Each block is separated by an emppty line.
*/
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

%  > Math <

/** 
 * calc(+Op:literal[*,+,-,/], +X:number, +Y:number, -Res:number) is det
 * 
 * Applies the operation to X and Y, unifying Res with the result.
*/
calc(*, X, Y, Res) :- Res is X * Y.
calc(+, X, Y, Res) :- Res is X + Y.
calc(-, X, Y, Res) :- Res is X - Y.
calc(/, X, Y, Res) :- Res is X / Y.

/** 
 * operatorium(+List:list[number], -Res:number, +Op:literal[*,+,-,/]) is det
 * 
 * Reduces a list of numbers with the given operation.
*/
operatorium([X], X, _) :- !.
operatorium([X|Tail], Y, Op) :-
    operatorium(Tail, Z, Op),
    calc(Op, X, Z, Y).

/** 
 * summatorium(+List:list[number], -Res) is det
*/
summatorium(L, X) :- operatorium(L, X, +).

/** 
 * productorium(+List:list[number], -Res) is det
*/
productorium(L, X) :- operatorium(L, X, *).

/**
 * fact(+N:number, -Res:number) is det
 * 
 * Factorial of N.
 */
fact(0, 1)  :- !.
fact(N, Fact) :-
    N1 is N - 1,
    fact(N1, Fact1),
    Fact is Fact1 * N.

% > Lists <

/** 
 * remove_last(+List:list[any], -Res:list[any]) is det
*/
remove_last([_], []) :- !.
remove_last([X|Xs], [X|Ys]) :-
    remove_last(Xs, Ys).

/** 
 * replace(+Idx:number, +New:any, +List:list[any], -NewList:list[any]) is det
 * 
 * Replace element at index Idx with the new element.
*/
replace(_, _, [], []) :- !.
replace(0, E, [_|T], [E|T]) :- !.
replace(N, O, [E|T1], [E|T2]) :-
    N1 is N - 1,
    replace(N1, O, T1, T2).

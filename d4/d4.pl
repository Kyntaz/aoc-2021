:- use_module("common/util.pl").
:- use_module(library(clpfd)).

%! line_to_numbers(+Line, -Numbers)
line_to_numbers(Line, Numbers) :-
    split_string(Line, " ", " ", NumbersStr),
    maplist(number_string, Numbers, NumbersStr).

%! block_to_board(+Block, -Board)
block_to_board(Block, Board) :- maplist(line_to_numbers, Block, Board).

%! read_input(-Numbers, -Boards)
read_input(Numbers, Boards) :-
    read_blocks([[NumbersStr] | BoardBlocks]),
    split_string(NumbersStr, ",", "", NumbersStrs),
    maplist(number_string, Numbers, NumbersStrs),
    maplist(block_to_board, BoardBlocks, Boards).

%! board_won(+Board)
board_won_aux(Board) :-
    member(Line, Board),
    forall(member(N, Line), N == x), !.
% Calling an aux ensures this terminates after transposing only once
board_won(Board) :- board_won_aux(Board), !.
board_won(Board) :-
    transpose(Board, Transposed),
    board_won_aux(Transposed), !.

%! play_number(+Number, +Board, -NewBoard)
play_number(Number, Board, NewBoard) :-
    nth0(LineIdx, Board, Line),
    nth0(NIdx, Line, Number), !,
    replace(NIdx, x, Line, NewLine),
    replace(LineIdx, NewLine, Board, NewBoard).
play_number(_, Board, Board).

%! board_score(+Board, -Score)
board_score(Board, Score) :-
    findall(N, (
        member(Line, Board),
        member(N, Line),
        N \= x
    ), Unmarked),
    sum_list(Unmarked, Score).

%! board_wins(+Numbers,
%!     +InitialTime,
%!     +LastNumber,
%!     +Board,
%!     -Score,
%!     -FinalTime
%! ) 
% If the board doesn't win, Score is -inf and FinalTime is inf.
board_wins([], _, _, _, -inf, inf).
board_wins(_, Time, LastNumber, Board, Score, Time) :-
    board_won(Board), !,
    board_score(Board, PartialScore),
    Score is PartialScore * LastNumber.
board_wins([Number | Numbers], Time, _, Board, Score, FinalTime) :-
    play_number(Number, Board, NewBoard),
    Time1 is Time + 1,
    board_wins(Numbers, Time1, Number, NewBoard, Score, FinalTime).

p1 :- 
    read_input(Numbers, Boards),
    maplist(board_wins(Numbers, 0, 0), Boards, Scores, Times),
    min_list(Times, MinTime),
    nth0(WinningIdx, Times, MinTime),
    nth0(WinningIdx, Scores, WinningScore),
    write_answer(WinningScore).

p2 :-
    read_input(Numbers, Boards),
    maplist(board_wins(Numbers, 0, 0), Boards, Scores, Times),
    max_list(Times, MaxTime),
    nth0(WinningIdx, Times, MaxTime),
    nth0(WinningIdx, Scores, WinningScore),
    write_answer(WinningScore).

:- use_module("common/util.pl").

%! read_dot(+Line, -Dot)
read_dot(Line, (X, Y)) :-
    split_string(Line, ",", "", [XStr, YStr]),
    number_string(X, XStr),
    number_string(Y, YStr).

%! read_instruction(+Line, -Instruction)
read_instruction(Line, (Axis, Position)) :-
    split_string(Line, " ", " ", ["fold", "along", AxisPosStr]),
    split_string(AxisPosStr, "=", "", [Axis, PosStr]),
    number_string(Position, PosStr).

%! read_dots_instructions(-Dots, -Instructions)
read_dots_instructions(Dots, Instructions) :-
    read_blocks([DotLines, InstructionLines]),
    maplist(read_dot, DotLines, Dots),
    maplist(read_instruction, InstructionLines, Instructions).

%! fold_dot(+Instruction, +Dot, -FoldedDot)
fold_dot(("x", Position), (X, Y), (X1, Y)) :-
    X1 is Position - abs(X - Position).
fold_dot(("y", Position), (X, Y), (X, Y1)) :-
    Y1 is Position - abs(Y - Position).

%! fold_dots(+Instruction, +Dots, -NewDots)
fold_dots(Instruction, Dots, NewDots) :-
    maplist(fold_dot(Instruction), Dots, NewDots).

%! do_instructions(+Instructions, +Dots, -NewDots)
do_instructions([], Dots, Dots).
do_instructions([Instruction | Instructions], Dots, NewDots) :-
    fold_dots(Instruction, Dots, TempDots),
    do_instructions(Instructions, TempDots, NewDots).

%! dot_grid(+Dots, +EmptyGrid, -PrintedGrid)
dot_grid([], Grid, Grid).
dot_grid([(X, Y) | Dots], Grid, NewGrid) :-
    nth0(Y, Grid, Line),
    replace(X, '#', Line, NewLine),
    replace(Y, NewLine, Grid, TempGrid),
    dot_grid(Dots, TempGrid, NewGrid).

%! initialize_grid(+Dots, -EmptyGrid)
initialize_grid(Dots, Grid) :-
    maplist([(X, _), X]>>(true), Dots, Xs),
    maplist([(_, Y), Y]>>(true), Dots, Ys),
    max_list(Xs, MaxX),
    max_list(Ys, MaxY),
    W is MaxX + 1,
    H is MaxY + 1,
    length(Grid, H),
    maplist([List]>>(length(List, W)), Grid),
    maplist(maplist(=(' ')), Grid).

%! write_gridpoint(+Char)
write_gridpoint(Char) :-
    write(Char),
    write(' ').

%! write_gridline(+Line)
write_gridline(Line) :-
    maplist(write_gridpoint, Line), nl.

%! write_grid(+Grid)
write_grid(Grid) :-
    maplist(write_gridline, Grid).

p1 :-
    read_dots_instructions(Dots, [Instruction | _]),
    fold_dots(Instruction, Dots, FoldedDots),
    sort(FoldedDots, UniqueDots),
    length(UniqueDots, NVisible),
    writeln(NVisible).

p2 :-
    read_dots_instructions(Dots, Instructions),
    do_instructions(Instructions, Dots, FoldedDots),
    sort(FoldedDots, UniqueDots),
    initialize_grid(UniqueDots, Grid),
    dot_grid(FoldedDots, Grid, PrintedGrid),
    write_grid(PrintedGrid). 

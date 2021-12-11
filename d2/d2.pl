:- use_module("common/util.pl").
 
%! line_to_command(+Line:string, -Command:(string, number))
line_to_command(Line, (Command, Value)) :-
    split_string(Line, " ", " ", [Command, ValueStr]),
    number_string(Value, ValueStr).

%! read_commands(-Commands:list[(string, number)])
read_commands(Commands) :-
    read_non_empty_lines(Lines),
    maplist(line_to_command, Lines, Commands).

%! do_command(
%!     +Command:(string, number),
%!     -NewState:(number, number)
%! )
do_command(("forward", Value), (Position, Depth), (Position1, Depth)) :- Position1 is Position + Value.
do_command(("down", Value), (Position, Depth), (Position, Depth1)) :- Depth1 is Depth + Value.
do_command(("up", Value), (Position, Depth), (Position, Depth1)) :- Depth1 is Depth - Value.

%! do_commands(
%!     +Commands:list[(string, number)],
%!     +State:(number, number),
%!     -FinalState(number, number)
%! )
do_commands([], State, State).
do_commands([Command | Commands], State, FinalState) :-
    do_command(Command, State, State1),
    do_commands(Commands, State1, FinalState).

%! do_command2(
%!     +Command:(string, number),
%!     +State:(number, number, number),
%!     -NewState:(number, number, number)
%! )
do_command2(("forward", Value), (Position, Depth, Aim), (Position1, Depth1, Aim)) :-
    Position1 is Position + Value,
    Depth1 is Depth + Aim * Value.
do_command2(("down", Value), (Position, Depth, Aim), (Position, Depth, Aim1)) :-
    Aim1 is Aim + Value.
do_command2(("up", Value), (Position, Depth, Aim), (Position, Depth, Aim1)) :-
    Aim1 is Aim - Value.

%! do_commands2(
%!     +Commands:list[(string, number)],
%!     +State:(number, number, number),
%!     -FinalState(number, number, number)
%! )
do_commands2([], State, State).
do_commands2([Command | Commands], State, FinalState) :-
    do_command2(Command, State, State1),
    do_commands2(Commands, State1, FinalState).

p1 :-
    read_commands(Commands),
    do_commands(Commands, (0,0), (Position, Depth)),
    N is Depth * Position,
    writeln(N).

p2 :-
    read_commands(Commands),
    do_commands2(Commands, (0,0,0), (Position, Depth, _)),
    N is Depth * Position,
    writeln(N).

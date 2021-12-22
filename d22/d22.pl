:- module(d22, []).
:- use_module("common/util.pl").
:- use_module(library(dcg/basics)).

%! instruction(-Instruction)//
instruction(i(State, cube(XMin, YMin, ZMin, XMax, YMax, ZMax))) -->
    state(State), " x=", integer(XMin), "..", integer(XMax),
    ",y=", integer(YMin), "..",  integer(YMax),
    ",z=", integer(ZMin), "..", integer(ZMax).

%! state(-OnOff)//
state(on) --> "on".
state(off) --> "off".

%! read_instruction(+Line, -Instruction)
read_instruction(Line, Instruction) :-
    string_codes(Line, Codes),
    phrase(instruction(Instruction), Codes).

%! read_instructions(-Instructions)
read_instructions(Instructions) :-
    read_non_empty_lines(Lines),
    maplist(read_instruction, Lines, Instructions).

%! in_scope(+Instruction) is semidet
in_scope(i(_, cube(MinX, MinY, MinZ, MaxX, MaxY, MaxZ))) :-
    Checks = [MinX, MinY, MinZ, MaxX, MaxY, MaxZ],
    maplist([N]>>(N =< 50, N >= -50), Checks).

%! filter_instructions(+Instructions, -FilteredInstructions)
% Only instructions in scope for part 1.
filter_instructions(Instructions, Filtered) :-
    include(in_scope, Instructions, Filtered).

%! intersect(+Cube1, +Cube2, -Intersection)
intersect(Cube1, Cube2, ICube) :-
    Cube1 = cube(MinX1, MinY1, MinZ1, MaxX1, MaxY1, MaxZ1),
    Cube2 = cube(MinX2, MinY2, MinZ2, MaxX2, MaxY2, MaxZ2),
    ICube = cube(MinX, MinY, MinZ, MaxX, MaxY, MaxZ),
    MinX is max(MinX1, MinX2),
    MinY is max(MinY1, MinY2),
    MinZ is max(MinZ1, MinZ2),
    MaxX is min(MaxX1, MaxX2),
    MaxY is min(MaxY1, MaxY2),
    MaxZ is min(MaxZ1, MaxZ2),
    MinX =< MaxX,
    MinY =< MaxY,
    MinZ =< MaxZ, !.
intersect(_, _, empty).

%! volume(+Cube, -Vol).
volume(Cube, Vol) :-
    Cube = cube(MinX, MinY, MinZ, MaxX, MaxY, MaxZ),
    Vol is (MaxX - MinX + 1) * (MaxY - MinY + 1) * (MaxZ - MinZ + 1).

%! pvolume(+Cube, +CubesToRemove, -Volume)
% Partial value of Cube, removing the volume of the union of CubesToRemove.
pvolume(Cube, [], Vol) :- volume(Cube, Vol).
pvolume(Cube, [empty | ToRemove], Vol) :- !,
    pvolume(Cube, ToRemove, Vol).
pvolume(Cube, [RemoveCube | ToRemove], Vol) :-
    maplist(intersect(RemoveCube), ToRemove, SubToRemove),
    pvolume(RemoveCube, SubToRemove, VolToRemove),
    pvolume(Cube, ToRemove, PVol),
    Vol is PVol - VolToRemove.

%! number_ons(+Instruction, +FutureInstructions, -Ons)
% Number of lights that will remain on and unaffected in the future
% due to this particular instruction.
number_ons(i(off, _), _, 0).
number_ons(i(on, Cube), FutureInstructions, N) :-
    maplist([i(_, C), C]>>(true), FutureInstructions, FutureCubes),
    maplist(intersect(Cube), FutureCubes, ToRemove),
    pvolume(Cube, ToRemove, N).

%! total_ons(+Instructions, ++0, -N)
total_ons([], N, N).
total_ons([Instruction | FutureInstructions], N0, N) :-
    number_ons(Instruction, FutureInstructions, N1),
    N2 is N0 + N1,
    total_ons(FutureInstructions, N2, N).

p1 :-
    read_instructions(Instructions0),
    filter_instructions(Instructions0, Instructions),
    total_ons(Instructions, 0, N),
    write_answer(N).

p2 :-
    read_instructions(Instructions),
    total_ons(Instructions, 0, N),
    write_answer(N).

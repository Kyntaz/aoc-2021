:- use_module("common/util.pl").
:- use_module(library(dcg/basics)).

%! target(-XMin, -XMax, -YMin, -YMax)//
target(XMin, XMax, YMin, YMax) --> "target area: x=", integer(XMin), "..", integer(XMax), ", y=",
    integer(YMin), "..", integer(YMax).

%! read_target_area(-XMin, -XMax, -YMin, -YMax)
read_target_area(XMin, XMax, YMin, YMax) :-
    read_non_empty_lines([Line]),
    string_codes(Line, Codes),
    phrase(target(XMin, XMax, YMin, YMax), Codes).

%! sign(+N, -Sign)
sign(0, 0).
sign(N, 1) :- N > 0.
sign(N, -1) :- N < 0.

%! simulate(+ProbeState0, -ProbeState1)
simulate(probe(X, Y, Vx, Vy), probe(X1, Y1, Vx1, Vy1)) :-
    X1 is X + Vx,
    Y1 is Y + Vy,
    sign(Vx, SVx),
    Vx1 is Vx - SVx,
    Vy1 is Vy - 1.

%! valid_state(+ProbeState, +YMin, +XMin, +XMax)
valid_state(probe(X, Y, Vx, _), YMin, _, XMax) :-
    Y >= YMin,
    Vx > 0, !,
    X =< XMax.
valid_state(probe(X, Y, Vx, _), YMin, XMin, _) :-
    Y >= YMin,
    Vx < 0, !,
    X >= XMin.
valid_state(probe(X, Y, Vx, _), YMin, XMin, XMax) :-
    Y >= YMin,
    Vx = 0, !,
    X >= XMin,
    X =< XMax.

%! winning_state(+ProbeState, +XMin, +XMax, +YMin, +YMax)
winning_state(probe(X, Y, _, _), XMin, XMax, YMin, YMax) :-
    X >= XMin,
    X =< XMax,
    Y >= YMin,
    Y =< YMax.

%! simulate_win(+ProbeState, +XMin, +XMax, +YMin, +YMax, +0, -MaxHeight)
simulate_win(State, XMin, XMax, YMin, YMax, H, H) :-
    winning_state(State, XMin, XMax, YMin, YMax), !.
simulate_win(State, XMin, XMax, YMin, YMax, Ch, H) :-
    valid_state(State, YMin, XMin, XMax),
    State = probe(_, Y, _, _),
    Ch1 is max(Y, Ch),
    simulate(State, State1),
    simulate_win(State1, XMin, XMax, YMin, YMax, Ch1, H).

%! good_velocity(-Vx, -Vy, +XMin, +XMax, +YMin, +YMax, -MaxHeight) is multi
% From the creators of Dr. Shack, The Good Velocity, now on HBO!
good_velocity(Vx, Vy, XMin, XMax, YMin, YMax, H) :-
    VxMin is 0,
    VxMax is XMax,
    VyMin is YMin,
    VyMax is -YMin,
    numlist(VxMin, VxMax, VxDomain),
    numlist(VyMin, VyMax, VyDomain),
    member(Vx, VxDomain),
    member(Vy, VyDomain),
    simulate_win(probe(0, 0, Vx, Vy), XMin, XMax, YMin, YMax, 0, H).

p1 :-
    read_target_area(XMin, XMax, YMin, YMax),
    aggregate(max(H), (Vx, Vy, H)^good_velocity(Vx, Vy, XMin, XMax, YMin, YMax, H), MaxH),
    write_answer(MaxH).

p2 :-
    read_target_area(XMin, XMax, YMin, YMax),
    aggregate(count, (Vx, Vy, H)^good_velocity(Vx, Vy, XMin, XMax, YMin, YMax, H), N),
    write_answer(N).

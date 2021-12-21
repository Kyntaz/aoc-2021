:- module(d21, []).
:- use_module("common/util.pl").
:- use_module(library(dcg/basics)).
:- dynamic cached_state/3.

player_pos(I, X) --> "Player ", integer(I), " starting position: ", integer(X).

read_player_info(Line, player(I, X, 0)) :-
    string_codes(Line, Codes),
    phrase(player_pos(I, X), Codes).

read_players(Player1, Player2) :-
    read_non_empty_lines([Line1, Line2]),
    read_player_info(Line1, Player1),
    read_player_info(Line2, Player2).

turn(D0, D, player(I, X0, S0), player(I, X, S)) :-
    X1 is X0 + 3*D0 + 3,
    X is ((X1 - 1) mod 10) + 1,
    S is S0 + X,
    D1 is D0 + 3,
    D is ((D1 - 1) mod 100) + 1.

turn_pair(D0, D, Player0, Cpu0, Player, Cpu) :-
    turn(D0, D1, Player0, Player),
    Player = player(_,_,S),
    (S < 1000 -> turn(D1, D, Cpu0, Cpu) ; Cpu = Cpu0).

until_win(Turns, FinalTurns, _, Player, Cpu, Player, Cpu) :-
    Player = player(_, _, S),
    S >= 1000, !,
    FinalTurns is Turns - 3.
until_win(Turns, FinalTurns, _, Player, Cpu, Player, Cpu) :-
    Cpu = player(_, _, S),
    S >= 1000, !,
    FinalTurns is Turns.
until_win(Turns, FinalTurns, Dice, Player, Cpu, FinalPlayer, FinalCpu) :-
    turn_pair(Dice, Dice1, Player, Cpu, Player1, Cpu1),
    Turns1 is Turns + 6,
    until_win(Turns1, FinalTurns, Dice1, Player1, Cpu1, FinalPlayer, FinalCpu).

losing_score(player(_,_,S1), player(_,_,S2), Score) :-
    (S1 >= 1000 -> Score = S2 ; Score = S1).

cache_state(State, Wins1, Wins2) :-
    cached_state(State, Wins1, Wins2), !,
    write_debug("Repeated cache").
cache_state(State, Wins1, Wins2) :-
    assertz(cached_state(State, Wins1, Wins2)).

qturn(s(1, player(1, X0, S0), Player2), s(2, player(1, X, S), Player2)) :-
    Possibilities = [1, 2, 3],
    member(D1, Possibilities),
    member(D2, Possibilities),
    member(D3, Possibilities),
    X1 is X0 + D1 + D2 + D3,
    X is ((X1 - 1) mod 10) + 1,
    S is S0 + X.
qturn(s(2, Player1, player(2, X0, S0)), s(1, Player1, player(2, X, S))) :-
    Possibilities = [1, 2, 3],
    member(D1, Possibilities),
    member(D2, Possibilities),
    member(D3, Possibilities),
    X1 is X0 + D1 + D2 + D3,
    X is ((X1 - 1) mod 10) + 1,
    S is S0 + X.

winning_state(1, s(_, player(_,_,S), _)) :-
    S >= 21.
winning_state(2, s(_, _, player(_,_,S))) :-
    S >= 21.

possible_outcomes(State, Wins1, Wins2) :-
    cached_state(State, Wins1, Wins2), !.
possible_outcomes(State, 1, 0) :-
    winning_state(1, State), !.
possible_outcomes(State, 0, 1) :-
    winning_state(2, State), !.
possible_outcomes(State, Wins1, Wins2) :-
    findall(NextState, qturn(State, NextState), NextStates),
    maplist(possible_outcomes, NextStates, OutWins1, OutWins2),
    sum_list(OutWins1, Wins1),
    sum_list(OutWins2, Wins2),
    cache_state(State, Wins1, Wins2).

p1 :-
    read_players(Player, Cpu),
    until_win(0, FinalTurns, 1, Player, Cpu, FinalPlayer, FinalCpu),
    losing_score(FinalPlayer, FinalCpu, Score),
    Out is Score * FinalTurns,
    write_answer(Out).

p2 :-
    read_players(Player, Cpu),
    possible_outcomes(s(1, Player, Cpu), Wins1, Wins2),
    Wins is max(Wins1, Wins2),
    writeln(Wins).

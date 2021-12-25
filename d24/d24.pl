:- module(d24, []).
:- use_module("common/util.pl").
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(clpb)).

parse_var(w) --> "w".
parse_var(x) --> "x".
parse_var(y) --> "y".
parse_var(z) --> "z".

parse_constvar(Int) --> integer(Int).
parse_constvar(Var) --> parse_var(Var).

parse_instr(i(inp, A)) --> "inp ", parse_var(A).
parse_instr(i(add, A, B)) --> "add ", parse_var(A), " ", parse_constvar(B).
parse_instr(i(mul, A, B)) --> "mul ", parse_var(A), " ", parse_constvar(B).
parse_instr(i(div, A, B)) --> "div ", parse_var(A), " ", parse_constvar(B).
parse_instr(i(mod, A, B)) --> "mod ", parse_var(A), " ", parse_constvar(B).
parse_instr(i(eql, A, B)) --> "eql ", parse_var(A), " ", parse_constvar(B).

read_instruction(Line, Instr) :-
    string_codes(Line, Codes),
    phrase(parse_instr(Instr), Codes).

read_instructions(Instructions) :-
    read_non_empty_lines(Lines),
    maplist(read_instruction, Lines, Instructions).

input_state(Input, s(Input, [w(0), x(0), y(0), z(0)])).

resolve(Val, _, Val) :- integer(Val), !.
resolve(Var, s(_, Vars), Val) :-
    Record =.. [Var, Val],
    member(Record, Vars).

set(Var, Val, s(I, Vars0), s(I, Vars1)) :-
    Record0 =.. [Var, _],
    Record1 =.. [Var, Val],
    select(Record0, Vars0, Record1, Vars1).

ex(i(inp, A), State0, State1, [Val]) :-
    State0 = s([Val | In], _),
    State1 = s(In, Vars1),
    set(A, Val, State0, s(_, Vars1)).
ex(i(add, A, B), State0, State1, [Val]) :-
    resolve(A, State0, Val1),
    resolve(B, State0, Val2),
    Val #= Val1 + Val2,
    set(A, Val, State0, State1).
ex(i(mul, A, B), State0, State1, [Val]) :-
    resolve(A, State0, Val1),
    resolve(B, State0, Val2),
    Val #= Val1 * Val2,
    set(A, Val, State0, State1).
ex(i(div, A, B), State0, State1, [Val]) :-
    resolve(A, State0, Val1),
    resolve(B, State0, Val2),
    Val2 #\= 0,
    Val #= Val1 // Val2,
    set(A, Val, State0, State1).
ex(i(mod, A, B), State0, State1, [Val]) :-
    resolve(A, State0, Val1),
    resolve(B, State0, Val2),
    Val1 #>= 0,
    Val2 #> 0,
    Val #= Val1 mod Val2,
    set(A, Val, State0, State1).
ex(i(eql, A, B), State0, State1, [Val]) :-
    resolve(A, State0, Val1),
    resolve(B, State0, Val2),
    Val in 0..1,
    (Val1 #= Val2) #<==> (Val #= 1),
    (Val1 #\= Val2) #<==> (Val #= 0),
    set(A, Val, State0, State1).

run([], State, State, Vars, Vars).
run([Inst | Instructions], State0, FinalState, Vars, FinalVars) :-
    ex(Inst, State0, State1, NewVars),
    append(Vars, NewVars, Vars1),
    run(Instructions, State1, FinalState, Vars1, FinalVars).

write_try1(Var) :-
    var(Var), !,
    write("X").
write_try1(Int) :-
    integer(Int),
    write(Int).

write_try(Input) :-
    maplist(write_try1, Input), nl.

p1 :-
    read_instructions(Instructions),
    length(Input, 14),
    Input ins 1..9,
    input_state(Input, State0),
    run(Instructions, State0, FinalState, [], AllVars),
    FinalState = s(_, Vars),
    member(z(0), Vars),
    write_debug(FinalState),
    labeling([ff, down, bisect], AllVars), !,
    write_answer(Input).

p2 :-
    read_instructions(Instructions),
    length(Input, 14),
    Input ins 1..9,
    input_state(Input, State0),
    run(Instructions, State0, FinalState, [], AllVars),
    FinalState = s(_, Vars),
    member(z(0), Vars),
    labeling([ff, up, bisect], AllVars), !,
    write_answer(Input).

:- use_module("common/util.pl").
:- dynamic connection/2.

%! read_connection(+Line, -Connection)
read_connection(Line, (From, To)) :-
    split_string(Line, "-", "", [From, To]).

%! read_connections(-Connections)
read_connections(Connections) :-
    read_non_empty_lines(Lines),
    maplist(read_connection, Lines, Connections).

%! assert_connection(+Connection)
% Asserts a prolog facts stating that the two caves are connected.
assert_connection((From, To)) :-
    assertz(connection(From, To)),
    assertz(connection(To, From)).

%! assert_connections(+Connections)
assert_connections(Connections) :- maplist(assert_connection, Connections).

%! small_cave(+Cave)
small_cave(Cave) :- string_lower(Cave, Cave).

%! can_be_visited(+Cave, +Visited, +AllowDup)
can_be_visited(Cave, Visited, _) :-
    \+ member(Cave, Visited), !.
can_be_visited(Cave, Visited, true) :-
    Cave \= "start",
    Cave \= "end",
    no_duplicates(Visited).

%! connected_path(+Visited, -Path, +AllowDup)
connected_path(_, ["end"], _).
connected_path(Visited, [From, To | Others], AllowDup) :-
    From \= "end",
    connection(From, To),
    can_be_visited(To, Visited, AllowDup),
    (small_cave(To) -> 
        append(Visited, [To], NewVisited) ;
        NewVisited = Visited
    ),
    connected_path(NewVisited, [To | Others], AllowDup).

%! valid_path(-Path, +AllowDup)
valid_path(Path, AllowDup) :-
    nth0(0, Path, "start"),
    connected_path(["start"], Path, AllowDup).

p1 :-
    read_connections(Connections),
    assert_connections(Connections),
    findall(Path, valid_path(Path, false), Paths),
    length(Paths, NPaths),
    writeln(NPaths).

p2 :-
    read_connections(Connections),
    assert_connections(Connections),
    findall(Path, valid_path(Path, true), Paths),
    length(Paths, NPaths),
    writeln(NPaths).

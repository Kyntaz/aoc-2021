:- module(d12, []).
:- use_module("common/util.pl").

%! read_connection(+Line, -Connection)
read_connection(Line, From-To) :-
    split_string(Line, "-", "", [From, To]).

%! read_connections(-Graph)
read_connections(Graph) :-
    read_non_empty_lines(Lines),
    maplist(read_connection, Lines, Connections),
    vertices_edges_to_ugraph([], Connections, UGraph),
    transpose_ugraph(UGraph, NGraph),
    ugraph_union(UGraph, NGraph, Graph).

%! small_cave(+Cave)
small_cave(Cave) :- string_lower(Cave, Cave).

%! can_be_visited(+Cave, +Visited, +AllowDup)
can_be_visited(Cave, Visited, _) :-
    \+ member(Cave, Visited), !.
can_be_visited(Cave, Visited, true) :-
    Cave \= "start",
    Cave \= "end",
    no_duplicates(Visited).

%! connected_path(+Graph, +Visited, -Path, +AllowDup)
connected_path(_, _, ["end"], _).
connected_path(Graph, Visited, [From, To | Others], AllowDup) :-
    From \= "end",
    neighbors(From, Graph, Tos),
    member(To, Tos),
    can_be_visited(To, Visited, AllowDup),
    (small_cave(To) -> 
        append(Visited, [To], NewVisited) ;
        NewVisited = Visited
    ),
    connected_path(Graph, NewVisited, [To | Others], AllowDup).

%! valid_path(+Graph, -Path, +AllowDup)
valid_path(Graph, Path, AllowDup) :-
    Path = ["start" | _],
    connected_path(Graph, ["start"], Path, AllowDup).

p1 :-
    read_connections(Graph),
    aggregate(count, Path^valid_path(Graph, Path, false), NPaths),
    write_answer(NPaths).

p2 :-
    read_connections(Graph),
    aggregate(count, Path^valid_path(Graph, Path, true), NPaths),
    write_answer(NPaths).

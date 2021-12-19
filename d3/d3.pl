:- use_module("common/util.pl").
:- use_module(library(clpfd)).

%! most_common(+ListOfBits, -Bit)
most_common(ListOfBits, Bit) :-
    member(Bit, ['1', '0']),
    include(==(Bit), ListOfBits, EqualsList),
    exclude(==(Bit), ListOfBits, DiffList),
    length(EqualsList, Le),
    length(DiffList, Ld),
    Le >= Ld, !.

%! criteria(+Idx, +BitsToSelect, +Bits)
criteria(Idx, BitsToSelect, Bits) :-
    nth0(Idx, BitsToSelect, Bit),
    nth0(Idx, Bits, Bit).

%! find_number(+Bits, +Flip:bool, +Idx, -NumberInBits)
find_number([Number], _, _, Number) :- !.
find_number(Bits, Flip, Idx, Number) :-
    transpose(Bits, BitsPerPosition),
    % This is not very efficient, but the input is small...
    maplist(most_common, BitsPerPosition, CommonBits),
    (Flip ->
        maplist([X,Y]>>((X='0',Y='1') ; (X='1',Y='0')), CommonBits, BitsToChoose) ;
        BitsToChoose = CommonBits
    ),
    include(criteria(Idx, BitsToChoose), Bits, FilteredBits),
    Idx1 is Idx + 1,
    find_number(FilteredBits, Flip, Idx1, Number).

p1 :-
    read_grid(Bits),
    transpose(Bits, BitsPerPosition),
    maplist(most_common, BitsPerPosition, CommonBits),
    base_chars_number(2, CommonBits, Gamma),
    maplist([X,Y]>>((X='0',Y='1') ; (X='1',Y='0')), CommonBits, UncommonBits),
    base_chars_number(2, UncommonBits, Epsilon),
    Power is Gamma * Epsilon,
    write_answer(Power).

p2 :-
    read_grid(Bits),
    find_number(Bits, false, 0, OxigenBits),
    find_number(Bits, true, 0, ScrubberBits),
    base_chars_number(2, OxigenBits, Oxigen),
    base_chars_number(2, ScrubberBits, Scrubber),
    Life is Oxigen * Scrubber,
    write_answer(Life).

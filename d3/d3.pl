:- ["common/util.pl"].
:- use_module(library(clpfd)).

/** 
 * most_common(+ListOfBits, -Bit) is det
*/
most_common(ListOfBits, Bit) :-
    member(Bit, ['1', '0']),
    include(==(Bit), ListOfBits, EqualsList),
    exclude(==(Bit), ListOfBits, DiffList),
    length(EqualsList, Le),
    length(DiffList, Ld),
    Le >= Ld, !.

/** 
 * rev_bits_to_number(+ReversedBits, -Number) is det
*/
rev_bits_to_number([], 0) :- !.
rev_bits_to_number(['0' | Bits], Number) :-
    rev_bits_to_number(Bits, Number1),
    Number is Number1 * 2.
rev_bits_to_number(['1' | Bits], Number) :-
    rev_bits_to_number(Bits, Number1),
    Number is 1 + Number1 * 2.

/** 
 * bits_to_number(+Bits, -Number) is det
*/
bits_to_number(Bits, Number) :-
    reverse(Bits, RevBits),
    rev_bits_to_number(RevBits, Number).

/** 
 * flip(?Bit, ?NewBit) is det
*/
flip('0', '1').
flip('1', '0').

/** 
 * criteria(+Idx, +BitsToSelect, +Bits) is semidet
*/
criteria(Idx, BitsToSelect, Bits) :-
    nth0(Idx, BitsToSelect, Bit),
    nth0(Idx, Bits, Bit).

/** 
 * find_number(+Bits, +Flip:bool, +Idx, -NumberInBits) is semidet
*/
find_number([Number], _, _, Number) :- !.
find_number(Bits, Flip, Idx, Number) :-
    transpose(Bits, BitsPerPosition),
    % This is not very efficient, but the input is small...
    maplist(most_common, BitsPerPosition, CommonBits),
    (Flip -> maplist(flip, CommonBits, BitsToChoose) ; BitsToChoose = CommonBits),
    include(criteria(Idx, BitsToChoose), Bits, FilteredBits),
    Idx1 is Idx + 1,
    find_number(FilteredBits, Flip, Idx1, Number).

p1 :-
    read_grid(Bits),
    transpose(Bits, BitsPerPosition),
    maplist(most_common, BitsPerPosition, CommonBits),
    bits_to_number(CommonBits, Gamma),
    maplist(flip, CommonBits, UncommonBits),
    bits_to_number(UncommonBits, Epsilon),
    Power is Gamma * Epsilon,
    writeln(Power).

p2 :-
    read_grid(Bits),
    find_number(Bits, false, 0, OxigenBits),
    find_number(Bits, true, 0, ScrubberBits),
    bits_to_number(OxigenBits, Oxigen),
    bits_to_number(ScrubberBits, Scrubber),
    Life is Oxigen * Scrubber,
    writeln(Life).

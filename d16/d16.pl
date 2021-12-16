:- use_module("common/util.pl").

hex_bin('0', "0000").
hex_bin('1', "0001").
hex_bin('2', "0010").
hex_bin('3', "0011").
hex_bin('4', "0100").
hex_bin('5', "0101").
hex_bin('6', "0110").
hex_bin('7', "0111").
hex_bin('8', "1000").
hex_bin('9', "1001").
hex_bin('A', "1010").
hex_bin('B', "1011").
hex_bin('C', "1100").
hex_bin('D', "1101").
hex_bin('E', "1110").
hex_bin('F', "1111").

read_binary(Bin) :-
    read_non_empty_lines([Hex]),
    string_chars(Hex, Chars),
    maplist(hex_bin, Chars, BinStrs),
    maplist(string_chars, BinStrs, BinLayered),
    flatten(BinLayered, Bin).

packet(num(V, N)) --> version(V), type(4), number_blocks(Blocks), trail, {flatten(Blocks, Bits), base_chars_number(2, Bits, N)}.
packet(op(V, T, SubPackets)) --> version(V), type(T), {T \= 4}, length_type(0), n_bit_number(15, N), sub_packets_z(N, SubPackets).
packet(op(V, T, SubPackets)) --> version(V), type(T), {T \= 4}, length_type(1), n_bit_number(11, N), sub_packets_o(N, SubPackets).
version(V) --> n_bit_number(3, V).
type(T) --> n_bit_number(3, T).
number_blocks([Block]) --> {length(Block, 4)}, ['0' | Block].
number_blocks([Block | Blocks]) --> {length(Block, 4)}, ['1' | Block], number_blocks(Blocks).
length_type(0) --> ['0'].
length_type(1) --> ['1'].
n_bit_number(L, N) --> {length(Bits, L)}, Bits, {base_chars_number(2, Bits, N)}.
sub_packets_z(N, SubPackets) --> {length(Bits, N)}, Bits, {phrase(sub_packets_z_aux(SubPackets), Bits)}.
sub_packets_z_aux([]) --> [].
sub_packets_z_aux([Packet | Packets]) --> packet(Packet), sub_packets_z_aux(Packets).
sub_packets_o(0, []) --> [].
sub_packets_o(N, [Packet | Packets]) --> {N1 is N - 1}, packet(Packet), sub_packets_o(N1, Packets).
trail --> [].
trail --> ['0'], trail.

sum_version_numbers(num(V, _), V).
sum_version_numbers(op(V0, _, Sub), V) :-
    maplist(sum_version_numbers, Sub, Vs),
    sum_list(Vs, V1),
    V is V0 + V1.

value(num(_, N), N).
value(op(_, 0, Sub), N) :-
    maplist(value, Sub, Ns),
    sum_list(Ns, N).
value(op(_, 1, Sub), N) :-
    maplist(value, Sub, Ns),
    productorium(Ns, N).
value(op(_, 2, Sub), N) :-
    maplist(value, Sub, Ns),
    min_list(Ns, N).
value(op(_, 3, Sub), N) :-
    maplist(value, Sub, Ns),
    max_list(Ns, N).
value(op(_, 5, Sub), N) :-
    maplist(value, Sub, [N1, N2]),
    (N1 > N2 -> N = 1 ; N = 0).
value(op(_, 6, Sub), N) :-
    maplist(value, Sub, [N1, N2]),
    (N1 < N2 -> N = 1 ; N = 0).
value(op(_, 7, Sub), N) :-
    maplist(value, Sub, [N1, N2]),
    (N1 == N2 -> N = 1 ; N = 0).

p1 :-
    read_binary(Bin),
    phrase(packet(Packet), Bin, _), !,
    sum_version_numbers(Packet, V),
    writeln(V).

p2 :-
    read_binary(Bin),
    phrase(packet(Packet), Bin, _), !,
    value(Packet, V),
    writeln(V).

:- use_module("common/util.pl").

to_binary('.', '0').
to_binary('#', '1').

rev('0', '1').
rev('1', '0').

read_binary_line(Line, Table) :-
    string_chars(Line, TableChars),
    maplist(to_binary, TableChars, Table).

read_image(Block, Image) :-
    maplist(read_binary_line, Block, BinBlock),
    setof(Point, (X, Y, Line, Point)^(
        nth0(Y, BinBlock, Line),
        nth0(X, Line, '1'),
        Point = p(Y, X)
    ), Image).

read_input(i('0', Image), Table) :-
    read_blocks([[TableStr], ImageBlock]),
    read_binary_line(TableStr, Table),
    read_image(ImageBlock, Image).

read_pixel(i(Bg, Pixels), Point, Fg) :-
    ord_memberchk(Point, Pixels), !,
    rev(Bg, Fg).
read_pixel(i(Bg, _), _, Bg).

region_of_interest(i(_, Image), p(MinY1, MinX1), p(MaxY1, MaxX1)) :-
    aggregate(r(min(X), max(X), min(Y), max(Y)), (X, Y)^(
        member(p(Y,X), Image)
    ), r(MinX, MaxX, MinY, MaxY)),
    MaxX1 is MaxX + 3,
    MaxY1 is MaxY + 3,
    MinX1 is MinX - 3,
    MinY1 is MinY - 3.

read_window(i(Bg, Image), Table, p(CY, CX), Out) :-
    MaxX is CX + 1,
    MaxY is CY + 1,
    MinX is CX - 1,
    MinY is CY - 1,
    numlist(MinX, MaxX, DomX),
    numlist(MinY, MaxY, DomY),
    setof(Point, (Point, X, Y)^(
        member(X, DomX),
        member(Y, DomY),
        Point = p(Y, X)
    ), Points),
    maplist(read_pixel(i(Bg, Image)), Points, Binary),
    base_chars_number(2, Binary, Decimal),
    nth0(Decimal, Table, Out).

enhance_image(Table, i(Bg, Pixels), i(NewBg, NewPixels)) :-
    region_of_interest(i(Bg, Pixels), p(MinY, MinX), p(MaxY, MaxX)),
    numlist(MinX, MaxX, DomX),
    numlist(MinY, MaxY, DomY),
    (Bg = '0' -> Table = [NewBg | _] ; last(Table, NewBg)),
    rev(NewBg, NewFg),
    setof(Point, (X, Y, Point)^(
        member(X, DomX),
        member(Y, DomY),
        Point = p(Y, X),
        read_window(i(Bg, Pixels), Table, Point, NewFg)
    ), NewPixels).

repeat_enhance(_, 0, Image, Image) :- !.
repeat_enhance(Table, N, Image0, Image) :-
    write_debug("Enhancings Left", N),
    N1 is N - 1,
    enhance_image(Table, Image0, Image1),
    repeat_enhance(Table, N1, Image1, Image).

p1 :-
    read_input(Image, Table),
    write_debug("Table", Table),
    repeat_enhance(Table, 2, Image, Image1),
    Image1 = i('0', Pixels),
    length(Pixels, LitPixels),
    write_answer(LitPixels).

p2 :-
    read_input(Image, Table),
    write_debug("Table", Table),
    repeat_enhance(Table, 50, Image, Image1),
    Image1 = i('0', Pixels),
    length(Pixels, LitPixels),
    write_answer(LitPixels).

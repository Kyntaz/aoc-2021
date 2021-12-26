:- module(d14, []).
:- use_module("common/util.pl").

%! read_template(+Line, -Template:Element[])
read_template(Line, Template) :-
    string_chars(Line, Template).

%! read_insertion_rule(+Line, -InsertionRule:(Pair, Element))
read_insertion_rule(Line, (Pair, Insert)) :-
    split_string(Line, " ", " ", [PairStr, "->", InsertStr]),
    string_chars(PairStr, Pair),
    string_chars(InsertStr, [Insert]).

%! read_polymerization(-Template:Element[], -InsertionRules:Assoc[Element[], Element])
read_polymerization(Template, InsertionRules) :-
    read_non_empty_lines([TemplateLine | InsertionRuleLines]),
    read_template(TemplateLine, Template),
    maplist(read_insertion_rule, InsertionRuleLines, PairStructs),
    maplist([(Key, Value), Key, Value]>>(true), PairStructs, Keys, Values),
    pairs_keys_values(Pairs, Keys, Values),
    list_to_assoc(Pairs, InsertionRules).

%! increment_count(+Assoc, +Key, +N, -NewAssoc)
increment_count(Assoc, Key, N, NewAssoc) :-
    (get_assoc(Key, Assoc, Count) ; Count = 0), !,
    Count1 is Count + N,
    put_assoc(Key, Assoc, Count1, NewAssoc).

%! template_pairs(+Template:Element[], +EmptyAssoc, -Pairs:Assoc[Element[], Count])
template_pairs([_], Pairs, Pairs).
template_pairs([X, Y | Rest], Pairs, NewPairs) :-
    increment_count(Pairs, [X, Y], 1, TempPairs),
    template_pairs([Y | Rest], TempPairs, NewPairs).

%! template_occurrences(+Template:Element[], +EmptyAssoc, -Occurrences:Assoc[Element, Count])
template_occurrences([], Occurrences, Occurrences).
template_occurrences([El | Els], Occurrences, NewOccurrences) :-
    increment_count(Occurrences, El, 1, TempOccurrences),
    template_occurrences(Els, TempOccurrences, NewOccurrences).

%! replace_pair(+InsertionRules, +ToReplace:(Element[], Count), +Pairs, -NewPairs, +Occs, -NewOccs)
replace_pair(InsertionRules, ([X, Y], Count), Pairs, NewPairs, Occs, NewOccs) :-
    get_assoc([X, Y], InsertionRules, Ins),
    increment_count(Pairs, [X, Ins], Count, TempPairs),
    increment_count(TempPairs, [Ins, Y], Count, NewPairs),
    increment_count(Occs, Ins, Count, NewOccs).

%! aux_replace_pairs(+InsertionRules, +PairsToReplace, +Pairs, -NewPairs, +Occs, -NewOccs)
aux_replace_pairs(_, [], RPairs, RPairs, Occs, Occs).
aux_replace_pairs(InsertionRules, [Pair | Rest], Pairs, NewPairs, Occs, NewOccs) :-
    replace_pair(InsertionRules, Pair, Pairs, TempPairs, Occs, TempOccs),
    aux_replace_pairs(InsertionRules, Rest, TempPairs, NewPairs, TempOccs, NewOccs).

%! replace_pairs(+InsertionRules, +Pairs, -NewPairs, +Occs, -NewOccs)
replace_pairs(InsertionRules, Pairs, NewPairs, Occs, NewOccs) :-
    empty_assoc(EmptyPairs),
    findall((Pair, Count), gen_assoc(Pair, Pairs, Count), ToReplace),
    aux_replace_pairs(InsertionRules, ToReplace, EmptyPairs, NewPairs, Occs, NewOccs).

%! replace_steps(+InsertionRules, +Steps, +Pairs, -NewPairs, +Occs, -NewOccs)
replace_steps(_, 0, Pairs, Pairs, Occs, Occs).
replace_steps(InsertionRules, Steps, Pairs, NewPairs, Occs, NewOccs) :-
    Steps > 0,
    replace_pairs(InsertionRules, Pairs, TempPairs, Occs, TempOccs),
    Steps1 is Steps - 1,
    replace_steps(InsertionRules, Steps1, TempPairs, NewPairs, TempOccs, NewOccs).

p1 :-
    read_polymerization(Template, InsertionRules),
    empty_assoc(EmptyPairs),
    empty_assoc(EmptyOccs),
    template_pairs(Template, EmptyPairs, TemplatePairs),
    template_occurrences(Template, EmptyOccs, Occs),
    replace_steps(InsertionRules, 10, TemplatePairs, _, Occs, FinalOccs),
    findall(Count, gen_assoc(_, FinalOccs, Count), Counts),
    max_list(Counts, Max),
    min_list(Counts, Min),
    Result is Max - Min,
    write_answer(Result).

p2 :-
    read_polymerization(Template, InsertionRules),
    empty_assoc(EmptyPairs),
    empty_assoc(EmptyOccs),
    template_pairs(Template, EmptyPairs, TemplatePairs),
    template_occurrences(Template, EmptyOccs, Occs),
    replace_steps(InsertionRules, 40, TemplatePairs, _, Occs, FinalOccs),
    findall(Count, gen_assoc(_, FinalOccs, Count), Counts),
    max_list(Counts, Max),
    min_list(Counts, Min),
    Result is Max - Min,
    write_answer(Result).

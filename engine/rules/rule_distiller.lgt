%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Rule Distiller - Learns patterns from audit data

:- object(rule_distiller).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2025-12-29,
        comment is 'Distills neural patterns into symbolic rules'
    ]).

    :- public([
        load_training_data/1,
        distill_rules/0,
        export_rules/1,
        get_pattern_frequency/2,
        get_top_issues/2
    ]).

    :- private([
        training_example/4,
        distilled_rule/3,
        pattern_count/2
    ]).

    :- dynamic([
        training_example/4,
        distilled_rule/3,
        pattern_count/2
    ]).

    %% ============================================================
    %% TRAINING DATA LOADER
    %% ============================================================

    %% Load from ERROR-CATALOG.scm format
    load_training_data(FilePath) :-
        open(FilePath, read, Stream),
        read_examples(Stream),
        close(Stream),
        count_patterns.

    read_examples(Stream) :-
        read(Stream, Term),
        ( Term == end_of_file ->
            true
        ; process_term(Term),
          read_examples(Stream)
        ).

    process_term(error(Id, Category, Severity, AutoFix, Desc, Fix)) :-
        assertz(training_example(Id, Category, Severity, fix(Desc, Fix, AutoFix))).
    process_term(_).

    %% Count pattern frequencies
    count_patterns :-
        retractall(pattern_count(_, _)),
        findall(Id, training_example(Id, _, _, _), Ids),
        count_occurrences(Ids).

    count_occurrences([]).
    count_occurrences([Id|Rest]) :-
        ( pattern_count(Id, N) ->
            retract(pattern_count(Id, N)),
            N1 is N + 1,
            assertz(pattern_count(Id, N1))
        ; assertz(pattern_count(Id, 1))
        ),
        count_occurrences(Rest).

    %% ============================================================
    %% RULE DISTILLATION
    %% ============================================================

    distill_rules :-
        retractall(distilled_rule(_, _, _)),
        findall(
            rule(Id, Severity, Fix),
            (
                training_example(Id, _, Severity, fix(_, Fix, _)),
                pattern_count(Id, Count),
                Count >= 3  % Only distill if seen 3+ times
            ),
            Rules
        ),
        assert_unique_rules(Rules).

    assert_unique_rules([]).
    assert_unique_rules([rule(Id, Severity, Fix)|Rest]) :-
        ( distilled_rule(Id, _, _) ->
            true  % Already have this rule
        ; assertz(distilled_rule(Id, Severity, Fix))
        ),
        assert_unique_rules(Rest).

    %% ============================================================
    %% EXPORTS
    %% ============================================================

    export_rules(FilePath) :-
        open(FilePath, write, Stream),
        write(Stream, '%% SPDX-License-Identifier: PMPL-1.0-or-later\n'),
        write(Stream, '%% Auto-distilled rules from cicd-hyper-a learning\n\n'),
        forall(
            distilled_rule(Id, Severity, Fix),
            format(Stream, 'learned_rule(~q, ~q, ~q).~n', [Id, Severity, Fix])
        ),
        close(Stream).

    %% ============================================================
    %% QUERIES
    %% ============================================================

    get_pattern_frequency(Id, Count) :-
        pattern_count(Id, Count).

    get_top_issues(N, TopN) :-
        findall(Count-Id, pattern_count(Id, Count), Pairs),
        sort(0, @>=, Pairs, Sorted),
        take(N, Sorted, TopN).

    take(0, _, []) :- !.
    take(_, [], []) :- !.
    take(N, [H|T], [H|Rest]) :-
        N > 0,
        N1 is N - 1,
        take(N1, T, Rest).

:- end_object.

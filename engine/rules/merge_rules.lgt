%% SPDX-License-Identifier: PMPL-1.0-or-later
%%
%% merge_rules.lgt — Neurosymbolic merge conflict reasoning predicates
%%
%% Predicates for AI-assisted merge conflict resolution.
%% Used by merge-resolver tool and visualized in PanLL.
%%
%% Facts are populated from merge-resolver session data
%% via the feedback-o-tron → VeriSimDB pipeline.

:- object(merge_rules).

    :- info([
        version is '1.0.0',
        author is 'Jonathan D.A. Jewell',
        comment is 'Merge conflict reasoning predicates'
    ]).

    %% ================================================================
    %% Facts (populated from merge-resolver sessions)
    %% ================================================================

    %% conflict(Repo, Branch, File, ConflictType)
    %% A detected merge conflict in a repository.
    :- public(conflict/4).
    :- dynamic(conflict/4).

    %% conflict_context(Repo, File, OursContext, TheirsContext)
    %% Content context for both sides of a conflict.
    :- public(conflict_context/4).
    :- dynamic(conflict_context/4).

    %% resolution_history(Repo, File, Strategy, Confidence, Timestamp)
    %% Historical conflict resolution decisions.
    :- public(resolution_history/5).
    :- dynamic(resolution_history/5).

    %% file_dependency(Repo, File, DependsOnFile)
    %% Import/dependency relationships between files.
    :- public(file_dependency/3).
    :- dynamic(file_dependency/3).

    %% test_covers(Repo, TestFile, SourceFile)
    %% Which test files cover which source files.
    :- public(test_covers/3).
    :- dynamic(test_covers/3).

    %% post_merge_test_result(Repo, Session, Status)
    %% Build/test result after merge was accepted.
    :- public(post_merge_test_result/3).
    :- dynamic(post_merge_test_result/3).

    %% ================================================================
    %% Derived predicates — Resolution recommendations
    %% ================================================================

    %% conflict_resolution(File, Strategy, Confidence)
    %% AI recommendation for how to resolve a conflict.
    :- public(conflict_resolution/3).

    %% If ours uses modern API and theirs uses deprecated, choose ours
    conflict_resolution(File, chose_ours, 0.95) :-
        conflict_context(_, File, OursContext, TheirsContext),
        uses_modern_api(OursContext),
        uses_deprecated_api(TheirsContext).

    %% If theirs uses modern API and ours uses deprecated, choose theirs
    conflict_resolution(File, chose_theirs, 0.95) :-
        conflict_context(_, File, OursContext, TheirsContext),
        uses_deprecated_api(OursContext),
        uses_modern_api(TheirsContext).

    %% If one side is a deletion and the other adds functionality, keep addition
    conflict_resolution(File, chose_theirs, 0.80) :-
        conflict(_, _, File, delete_modify).

    %% If historical resolution for same file exists with high confidence, reuse
    conflict_resolution(File, Strategy, AdjustedConf) :-
        resolution_history(_, File, Strategy, HistConf, _),
        HistConf >= 0.85,
        AdjustedConf is HistConf * 0.9.  %% Slight discount for reuse

    %% Default: suggest manual merge with low confidence
    conflict_resolution(File, manual_merge, 0.5) :-
        conflict(_, _, File, _),
        \+ conflict_context(_, File, _, _).

    %% ================================================================
    %% Derived predicates — Safety checks
    %% ================================================================

    %% safe_to_merge(Repo, Branch)
    %% Derivable when all conflicts are resolved and safety checks pass.
    :- public(safe_to_merge/2).
    safe_to_merge(Repo, Branch) :-
        \+ unresolved_conflict(Repo, Branch),
        \+ introduces_deprecated_api(Repo, Branch),
        \+ breaks_dependency(Repo, Branch).

    %% unresolved_conflict(Repo, Branch)
    %% There exists at least one unresolved conflict.
    :- public(unresolved_conflict/2).
    unresolved_conflict(Repo, Branch) :-
        conflict(Repo, Branch, File, _),
        \+ resolution_history(Repo, File, _, _, _).

    %% introduces_deprecated_api(Repo, Branch)
    %% The merge resolution introduces deprecated API usage.
    :- public(introduces_deprecated_api/2).
    introduces_deprecated_api(Repo, Branch) :-
        conflict(Repo, Branch, File, _),
        resolution_history(Repo, File, Strategy, _, _),
        strategy_keeps_deprecated(Strategy, Repo, File).

    %% breaks_dependency(Repo, Branch)
    %% A resolved conflict breaks a dependency chain.
    :- public(breaks_dependency/2).
    breaks_dependency(Repo, Branch) :-
        conflict(Repo, Branch, File, _),
        file_dependency(Repo, DependentFile, File),
        conflict(Repo, Branch, DependentFile, _),
        \+ resolution_history(Repo, DependentFile, _, _, _).

    %% rollback_recommended(Session, Reason)
    %% Derivable when post-merge tests fail.
    :- public(rollback_recommended/2).
    rollback_recommended(Session, test_failure) :-
        post_merge_test_result(_, Session, failing).
    rollback_recommended(Session, low_confidence) :-
        session_average_confidence(Session, AvgConf),
        AvgConf < 0.6.
    rollback_recommended(Session, deprecated_introduced) :-
        session_introduces_deprecated(Session).

    %% ================================================================
    %% Merge ordering predicates
    %% ================================================================

    %% resolve_order(Repo, Branch, OrderedFiles)
    %% Recommended order for resolving conflicts (dependencies first).
    :- public(resolve_order/3).
    resolve_order(Repo, Branch, Ordered) :-
        findall(File, conflict(Repo, Branch, File, _), AllFiles),
        topo_sort_by_deps(Repo, AllFiles, Ordered).

    %% ================================================================
    %% Helper predicates
    %% ================================================================

    %% uses_modern_api(Context) — check if text context uses modern APIs
    :- private(uses_modern_api/1).
    uses_modern_api(Context) :-
        (   sub_atom(Context, _, _, _, 'Array.')
        ;   sub_atom(Context, _, _, _, 'Dict.')
        ;   sub_atom(Context, _, _, _, 'Console.')
        ;   sub_atom(Context, _, _, _, 'String.')
        ;   sub_atom(Context, _, _, _, 'Promise.')
        ;   sub_atom(Context, _, _, _, 'Option.')
        ).

    %% uses_deprecated_api(Context) — check if text context uses deprecated APIs
    :- private(uses_deprecated_api/1).
    uses_deprecated_api(Context) :-
        (   sub_atom(Context, _, _, _, 'Js.')
        ;   sub_atom(Context, _, _, _, 'Belt.')
        ).

    %% strategy_keeps_deprecated(Strategy, Repo, File)
    :- private(strategy_keeps_deprecated/3).
    strategy_keeps_deprecated(chose_ours, Repo, File) :-
        conflict_context(Repo, File, OursContext, _),
        uses_deprecated_api(OursContext).
    strategy_keeps_deprecated(chose_theirs, Repo, File) :-
        conflict_context(Repo, File, _, TheirsContext),
        uses_deprecated_api(TheirsContext).

    %% session_average_confidence(Session, AvgConf)
    :- private(session_average_confidence/2).
    session_average_confidence(Session, AvgConf) :-
        findall(Conf, resolution_history(_, _, _, Conf, Session), Confs),
        Confs \= [],
        sum_list(Confs, Sum),
        length(Confs, Len),
        AvgConf is Sum / Len.

    %% session_introduces_deprecated(Session)
    :- private(session_introduces_deprecated/1).
    session_introduces_deprecated(Session) :-
        resolution_history(Repo, File, Strategy, _, Session),
        strategy_keeps_deprecated(Strategy, Repo, File).

    %% topo_sort_by_deps(Repo, Files, Sorted)
    %% Simple topological sort: files with no dependencies come first.
    :- private(topo_sort_by_deps/3).
    topo_sort_by_deps(Repo, Files, Sorted) :-
        findall(
            dep_count(Count, File),
            (   member(File, Files),
                aggregate_all(count, file_dependency(Repo, File, _), Count)
            ),
            Counts
        ),
        sort(1, @=<, Counts, SortedCounts),
        findall(F, member(dep_count(_, F), SortedCounts), Sorted).

    :- private(sum_list/2).
    sum_list([], 0).
    sum_list([H|T], Sum) :-
        sum_list(T, Rest),
        Sum is H + Rest.

:- end_object.

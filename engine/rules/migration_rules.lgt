%% SPDX-License-Identifier: PMPL-1.0-or-later
%%
%% migration_rules.lgt — ReScript migration reasoning predicates
%%
%% Neurosymbolic rules for planning, verifying, and tracking
%% ReScript migration across the hyperpolymath ecosystem.
%%
%% These predicates are populated from panic-attack scan data
%% via the Hypatia → ScanIngester pipeline.

:- object(migration_rules).

    :- info([
        version is '1.0.0',
        author is 'Jonathan D.A. Jewell',
        comment is 'ReScript migration reasoning predicates'
    ]).

    %% ================================================================
    %% Facts (populated from panic-attack scan data)
    %% ================================================================

    %% deprecated_api(Repo, ApiName, Count, Severity)
    %% Facts about deprecated API usage in a repository.
    %% Populated by panic-attack migration-snapshot → Hypatia pipeline.
    :- public(deprecated_api/4).
    :- dynamic(deprecated_api/4).

    %% modern_api(Repo, ApiName, Count)
    %% Facts about modern @rescript/core API usage.
    :- public(modern_api/3).
    :- dynamic(modern_api/3).

    %% migration_health(Repo, HealthScore)
    %% Overall migration health score for a repository (0.0 - 1.0).
    :- public(migration_health/2).
    :- dynamic(migration_health/2).

    %% version_bracket(Repo, Bracket)
    %% Detected ReScript version bracket.
    :- public(version_bracket/2).
    :- dynamic(version_bracket/2).

    %% config_format(Repo, Format)
    %% Configuration format: bsconfig, rescript_json, both, none.
    :- public(config_format/2).
    :- dynamic(config_format/2).

    %% repo_dependency(Repo, DependsOn)
    %% Inter-repo dependency relationship.
    :- public(repo_dependency/2).
    :- dynamic(repo_dependency/2).

    %% merge_conflict(Repo, Branch, FileCount)
    %% Known merge conflicts blocking migration.
    :- public(merge_conflict/3).
    :- dynamic(merge_conflict/3).

    %% build_status(Repo, Status)
    %% Last known build status: passing, failing, unknown.
    :- public(build_status/2).
    :- dynamic(build_status/2).

    %% ================================================================
    %% Derived predicates — Migration readiness
    %% ================================================================

    %% migration_ready(Repo) — repo is ready for migration
    %% A repo is ready when:
    %% - deprecated count is below threshold (< 10)
    %% - no merge conflicts
    %% - build is passing
    :- public(migration_ready/1).
    migration_ready(Repo) :-
        migration_health(Repo, Health),
        Health >= 0.8,
        \+ migration_blocked(Repo, _),
        build_status(Repo, passing).

    %% migration_blocked(Repo, Reason) — repo cannot be migrated
    :- public(migration_blocked/2).
    migration_blocked(Repo, merge_conflicts) :-
        merge_conflict(Repo, _, Count),
        Count > 0.
    migration_blocked(Repo, build_failing) :-
        build_status(Repo, failing).
    migration_blocked(Repo, heavy_deprecated_usage) :-
        total_deprecated(Repo, Total),
        Total > 100.

    %% ================================================================
    %% Derived predicates — Migration ordering
    %% ================================================================

    %% recommended_order(RepoList) — optimal migration sequence
    %% Repos with fewer dependencies and higher health scores go first.
    :- public(recommended_order/1).
    recommended_order(Sorted) :-
        findall(
            priority(Score, Repo),
            migration_priority(Repo, Score),
            Priorities
        ),
        sort(1, @>=, Priorities, SortedPriorities),
        findall(Repo, member(priority(_, Repo), SortedPriorities), Sorted).

    %% migration_priority(Repo, Score) — priority score for ordering
    :- public(migration_priority/2).
    migration_priority(Repo, Score) :-
        migration_health(Repo, Health),
        dependency_count(Repo, DepCount),
        total_deprecated(Repo, Deprecated),
        %% Higher health and fewer deps = higher priority
        %% Fewer deprecated = less work = higher priority
        Score is Health * 100 - DepCount * 5 - Deprecated * 0.1.

    %% ================================================================
    %% Derived predicates — Proof obligations
    %% ================================================================

    %% proof_obligation(Repo, Property) — things to verify post-migration
    :- public(proof_obligation/2).
    proof_obligation(Repo, no_js_dict_calls) :-
        deprecated_api(Repo, 'Js.Dict', Count, _),
        Count > 0.
    proof_obligation(Repo, no_belt_array_calls) :-
        deprecated_api(Repo, 'Belt.Array', Count, _),
        Count > 0.
    proof_obligation(Repo, no_js_log_calls) :-
        deprecated_api(Repo, 'Js.log', Count, _),
        Count > 0.
    proof_obligation(Repo, rescript_json_only) :-
        config_format(Repo, bsconfig).
    proof_obligation(Repo, rescript_json_only) :-
        config_format(Repo, both).
    proof_obligation(Repo, jsx_v4) :-
        version_bracket(Repo, Bracket),
        member(Bracket, [bucklescript, v11]).
    proof_obligation(Repo, uncurried_mode) :-
        version_bracket(Repo, Bracket),
        member(Bracket, [bucklescript, v11, v12_alpha]).
    proof_obligation(Repo, build_passes) :-
        build_status(Repo, failing).

    %% obligation_met(Repo, Property) — obligation verified
    :- public(obligation_met/2).
    :- dynamic(obligation_met/2).

    %% all_obligations_met(Repo) — all proof obligations satisfied
    :- public(all_obligations_met/1).
    all_obligations_met(Repo) :-
        \+ (proof_obligation(Repo, Prop), \+ obligation_met(Repo, Prop)).

    %% ================================================================
    %% Helper predicates
    %% ================================================================

    %% total_deprecated(Repo, Total) — sum of all deprecated API counts
    :- public(total_deprecated/2).
    total_deprecated(Repo, Total) :-
        findall(Count, deprecated_api(Repo, _, Count, _), Counts),
        sum_list(Counts, Total).

    %% dependency_count(Repo, Count) — number of repos this depends on
    :- public(dependency_count/2).
    dependency_count(Repo, Count) :-
        findall(Dep, repo_dependency(Repo, Dep), Deps),
        length(Deps, Count).

    %% dependent_repos(Repo, Dependents) — repos that depend on this one
    :- public(dependent_repos/2).
    dependent_repos(Repo, Dependents) :-
        findall(Dep, repo_dependency(Dep, Repo), Dependents).

    %% ================================================================
    %% Severity classification for deprecated patterns
    %% ================================================================

    %% classify_deprecated_severity(ApiName, Severity)
    :- public(classify_deprecated_severity/2).
    classify_deprecated_severity('Js.Array2', high).
    classify_deprecated_severity('Js.String2', high).
    classify_deprecated_severity('Js.Dict', high).
    classify_deprecated_severity('Belt.Array', high).
    classify_deprecated_severity('Belt.List', high).
    classify_deprecated_severity('Belt.Map', medium).
    classify_deprecated_severity('Belt.Set', medium).
    classify_deprecated_severity('Belt.Option', medium).
    classify_deprecated_severity('Js.Console', low).
    classify_deprecated_severity('Js.log', low).
    classify_deprecated_severity('Js.Promise', medium).
    classify_deprecated_severity('Js.Nullable', medium).
    classify_deprecated_severity('Js.Float', low).
    classify_deprecated_severity('Js.Int', low).
    classify_deprecated_severity('Js.Math', low).
    classify_deprecated_severity('Js.Json', medium).
    classify_deprecated_severity('Js.Re', low).
    classify_deprecated_severity('Js.Date', low).
    classify_deprecated_severity(_, info).

    %% ================================================================
    %% Auto-fix suggestions
    %% ================================================================

    %% suggest_migration_fix(ApiName, Replacement, Strategy)
    :- public(suggest_migration_fix/3).
    suggest_migration_fix('Js.Array2', 'Array', search_replace).
    suggest_migration_fix('Js.String2', 'String', search_replace).
    suggest_migration_fix('Js.Dict', 'Dict', module_replace).
    suggest_migration_fix('Js.Console', 'Console', module_replace).
    suggest_migration_fix('Js.log', 'Console.log', search_replace).
    suggest_migration_fix('Js.Promise', 'Promise', module_replace).
    suggest_migration_fix('Js.Nullable', 'Nullable', module_replace).
    suggest_migration_fix('Js.Float', 'Float', module_replace).
    suggest_migration_fix('Js.Int', 'Int', module_replace).
    suggest_migration_fix('Js.Math', 'Math', module_replace).
    suggest_migration_fix('Js.Json', 'JSON', module_replace).
    suggest_migration_fix('Js.Re', 'RegExp', module_replace).
    suggest_migration_fix('Belt.Array', 'Array', module_replace).
    suggest_migration_fix('Belt.List', 'List', module_replace).
    suggest_migration_fix('Belt.Map', 'Map', module_replace).
    suggest_migration_fix('Belt.Set', 'Set', module_replace).
    suggest_migration_fix('Belt.Option', 'Option', module_replace).
    suggest_migration_fix('Belt.Result', 'Result', module_replace).

    %% ================================================================
    %% Utility predicates
    %% ================================================================

    :- private(sum_list/2).
    sum_list([], 0).
    sum_list([H|T], Sum) :-
        sum_list(T, Rest),
        Sum is H + Rest.

:- end_object.

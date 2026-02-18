% SPDX-License-Identifier: PMPL-1.0-or-later

% Pattern detection rules for scan results

:- object(pattern_detector).

    % Detect widespread unsafe pattern
    :- public(widespread_unsafe/2).
    widespread_unsafe(Pattern, Repos) :-
        findall(Repo, weak_point(Repo, _, Pattern, high), Repos),
        length(Repos, Count),
        Count >= 3.

    % Detect deteriorating repo
    :- public(deteriorating/1).
    deteriorating(Repo) :-
        % TODO: Compare current vs previous scan
        % Needs temporal data
        fail.

    % Detect critical weak points in a repo
    :- public(critical_weak_points/2).
    critical_weak_points(Repo, Count) :-
        findall(1, weak_point(Repo, _, _, critical), List),
        length(List, Count),
        Count > 0.

    % Find all repos with a specific weakness type
    :- public(repos_with_weakness/2).
    repos_with_weakness(WeaknessType, Repos) :-
        findall(Repo, weak_point(Repo, _, WeaknessType, _), RepoList),
        list::unique(RepoList, Repos).

    % Calculate risk score for a repo
    :- public(repo_risk_score/2).
    repo_risk_score(Repo, Score) :-
        findall(S, (weak_point(Repo, _, _, Severity), severity_score(Severity, S)), Scores),
        list::sum(Scores, Score).

    % Helper: severity to numeric score
    severity_score(critical, 10).
    severity_score(high, 5).
    severity_score(medium, 2).
    severity_score(low, 1).

:- end_object.

% SPDX-License-Identifier: PMPL-1.0-or-later
% Security Lessons - Logtalk Rules for VeriSimDB Security Fixes
% Intended for incorporation into reposystem and gitbot-fleet repos

:- object(security_lessons).

    :- info([
        version is 1.0,
        author is 'VeriSimDB Security Team',
        date is 2026-01-22,
        comment is 'Lessons learned from Dependabot and OpenSSF Scorecard remediation'
    ]).

    %% Dependency Management Rules

    :- public(should_update_dependency/3).
    :- mode(should_update_dependency(+atom, +atom, +atom), zero_or_one).
    :- info(should_update_dependency/3, [
        comment is 'Determine if a dependency should be updated based on vulnerability',
        argnames is ['Package', 'CurrentVersion', 'TargetVersion']
    ]).

    % Rule: Update protobuf from 2.x to 3.x to fix recursion CVE
    should_update_dependency(protobuf, CurrentVersion, '3.7.2') :-
        atom_concat('2.', _, CurrentVersion),
        !.  % Recursion crash vulnerability

    % Rule: Update prometheus when it depends on vulnerable protobuf
    should_update_dependency(prometheus, '0.13.4', '0.14.0') :-
        transitive_depends_on(prometheus, protobuf, '2.28.0'),
        !.

    % Rule: Update tantivy to fix lru Stacked Borrows issue
    should_update_dependency(tantivy, CurrentVersion, '0.25.0') :-
        version_less_than(CurrentVersion, '0.25.0'),
        transitive_depends_on(tantivy, lru, LruVersion),
        vulnerable_lru(LruVersion),
        !.

    :- public(vulnerable_lru/1).
    vulnerable_lru('0.12.5').  % Stacked Borrows violation

    :- public(transitive_depends_on/3).
    :- mode(transitive_depends_on(+atom, +atom, +atom), zero_or_one).
    transitive_depends_on(Parent, Child, ChildVersion) :-
        % Placeholder: would query Cargo.lock/package-lock.json
        % In gitbot-fleet, implement via cargo tree or npm ls
        fail.

    %% Workflow Security Rules

    :- public(workflow_needs_fixing/2).
    :- mode(workflow_needs_fixing(+atom, -list), one).
    :- info(workflow_needs_fixing/2, [
        comment is 'Identify security issues in GitHub Actions workflows',
        argnames is ['WorkflowPath', 'Issues']
    ]).

    workflow_needs_fixing(Path, Issues) :-
        findall(Issue, workflow_issue(Path, Issue), Issues).

    :- private(workflow_issue/2).

    % Missing SPDX header
    workflow_issue(Path, missing_spdx_header) :-
        read_first_line(Path, Line),
        \+ atom_concat('# SPDX-License-Identifier:', _, Line).

    % Unpinned GitHub Actions
    workflow_issue(Path, unpinned_action(Action, Line)) :-
        read_workflow_line(Path, LineNum, Line),
        atom_concat('uses: ', Rest, Line),
        atom_concat(Action, '@', Rest),
        \+ (atom_concat(_, '@', Rest), atom_length(Rest, Len), Len > 40),  % SHA is 40 chars
        Line.

    % Missing permissions declaration
    workflow_issue(Path, missing_permissions) :-
        \+ workflow_has_permissions(Path).

    :- public(fix_unpinned_action/3).
    :- mode(fix_unpinned_action(+atom, +atom, -atom), one).
    :- info(fix_unpinned_action/3, [
        comment is 'Convert version tag to SHA-pinned reference',
        argnames is ['Action', 'Version', 'SHA']
    ]).

    % Known SHA mappings (January 2026)
    fix_unpinned_action('actions/checkout', 'v6.0.1', 'b4ffde65f46336ab88eb53be808477a3936bae11').
    fix_unpinned_action('actions/checkout', 'v4', 'b4ffde65f46336ab88eb53be808477a3936bae11').
    fix_unpinned_action('actions/configure-pages', 'v5', '983d7736d9b0ae728b81ab479565c72886d7745b').
    fix_unpinned_action('actions/upload-pages-artifact', 'v4', '56afc609e74202658d3ffba0e8f6dda462b719fa').
    fix_unpinned_action('actions/deploy-pages', 'v4', 'd6db90164ac5ed86f2b6aed7e0febac5b3c0c03e').
    fix_unpinned_action('actions/jekyll-build-pages', 'v1', '44a6e6beabd48582f863aeeb6cb2151cc1716697').
    fix_unpinned_action('ruby/setup-ruby', 'v1.207.0', '708024e6c902387ab41de36e1669e43b5ee7085e').
    fix_unpinned_action('dtolnay/rust-toolchain', 'stable', '6d9817901c499d6b02debbb57edb38d33daa680b').

    %% Branch Protection Rules

    :- public(branch_protection_config/2).
    :- mode(branch_protection_config(+atom, -term), one).
    :- info(branch_protection_config/2, [
        comment is 'Required branch protection settings for OpenSSF Scorecard',
        argnames is ['Branch', 'Config']
    ]).

    branch_protection_config(main, config(
        required_approving_review_count(1),
        enforce_admins(false),
        required_status_checks(null),
        restrictions(null),
        allow_force_pushes(false),
        allow_deletions(false),
        allow_fork_syncing(true)
    )).

    %% OpenSSF Scorecard Compliance Rules

    :- public(scorecard_check_status/3).
    :- mode(scorecard_check_status(+atom, +atom, -atom), one).
    :- info(scorecard_check_status/3, [
        comment is 'Determine if OpenSSF Scorecard check passes',
        argnames is ['CheckID', 'RepoPath', 'Status']
    ]).

    % Branch-Protection check
    scorecard_check_status('Branch-Protection', Repo, pass) :-
        branch_protected(Repo, main),
        !.
    scorecard_check_status('Branch-Protection', _, fail).

    % Code-Review check
    scorecard_check_status('Code-Review', Repo, pass) :-
        branch_protected(Repo, main),
        required_reviews(Repo, main, Count),
        Count >= 1,
        !.
    scorecard_check_status('Code-Review', _, fail).

    % Pinned-Dependencies check
    scorecard_check_status('Pinned-Dependencies', Repo, pass) :-
        forall(workflow_file(Repo, Path), all_actions_pinned(Path)),
        !.
    scorecard_check_status('Pinned-Dependencies', _, fail).

    % Vulnerabilities check
    scorecard_check_status('Vulnerabilities', Repo, pass) :-
        \+ has_dependabot_alerts(Repo),
        !.
    scorecard_check_status('Vulnerabilities', _, fail).

    %% Automation Helpers

    :- public(generate_fix_pr/3).
    :- mode(generate_fix_pr(+atom, +list, -atom), one).
    :- info(generate_fix_pr/3, [
        comment is 'Generate PR branch with automated security fixes',
        argnames is ['Repo', 'Issues', 'BranchName']
    ]).

    generate_fix_pr(Repo, Issues, BranchName) :-
        atom_concat('security-fixes-', Timestamp, BranchName),
        get_time(Timestamp),
        create_branch(Repo, BranchName),
        forall(member(Issue, Issues), apply_fix(Repo, BranchName, Issue)),
        commit_fixes(Repo, BranchName, 'chore(security): automated Scorecard fixes'),
        create_pull_request(Repo, BranchName, main, 'Security: OpenSSF Scorecard Fixes').

    %% Integration Points for gitbot-fleet

    :- public(scan_repo_for_issues/2).
    :- mode(scan_repo_for_issues(+atom, -list), one).
    :- info(scan_repo_for_issues/2, [
        comment is 'Scan repository and return all security issues',
        argnames is ['RepoPath', 'Issues']
    ]).

    scan_repo_for_issues(Repo, AllIssues) :-
        findall(dependency_issue(Pkg, Old, New),
            (cargo_dependency(Repo, Pkg, Old),
             should_update_dependency(Pkg, Old, New)),
            DepIssues),
        findall(workflow_issue(Path, Issue),
            (workflow_file(Repo, Path),
             workflow_issue(Path, Issue)),
            WorkflowIssues),
        findall(scorecard_fail(CheckID),
            scorecard_check_status(CheckID, Repo, fail),
            ScorecardIssues),
        append([DepIssues, WorkflowIssues, ScorecardIssues], AllIssues).

    %% Priority Rules

    :- public(issue_priority/2).
    :- mode(issue_priority(+term, -atom), one).

    issue_priority(dependency_issue(_, Version, _), critical) :-
        atom_concat('2.', _, Version),  % Major version jump (e.g., protobuf 2.x → 3.x)
        !.

    issue_priority(workflow_issue(_, unpinned_action(_, _)), high).
    issue_priority(workflow_issue(_, missing_permissions), high).
    issue_priority(workflow_issue(_, missing_spdx_header), medium).

    issue_priority(scorecard_fail('Branch-Protection'), high).
    issue_priority(scorecard_fail('Code-Review'), high).
    issue_priority(scorecard_fail('Vulnerabilities'), critical).
    issue_priority(scorecard_fail('Pinned-Dependencies'), medium).
    issue_priority(scorecard_fail(_), low).

    %% Lesson Summary

    :- public(security_lesson/2).
    :- mode(security_lesson(+atom, -atom), one).

    security_lesson(transitive_dependencies,
        'Update parent crates when transitive dependencies are vulnerable').
    security_lesson(major_version_jumps,
        'Major version updates (2.x→3.x) often fix critical CVEs').
    security_lesson(sha_pinning,
        'Always SHA-pin GitHub Actions to prevent tag manipulation').
    security_lesson(branch_protection,
        'Require at least 1 review for Code-Review scorecard compliance').
    security_lesson(automation,
        'Batch fix similar issues across repos to save time').

:- end_object.

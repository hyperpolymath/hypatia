% SPDX-License-Identifier: PMPL-1.0-or-later
% Comprehensive Error Catalog for CI/CD Security
% This file serves as both documentation and a queryable database
% Origin: security-audit/logtalk-db/error_catalog.lgt

:- object(error_catalog).

    :- info([
        version is 1:0:0,
        author is 'hyperpolymath',
        date is 2025-12-31,
        comment is 'Catalog of CI/CD security errors with prevention mechanisms'
    ]).

    %% Error type definitions with prevention strategies
    :- public(error_type/6).
    :- mode(error_type(?atom, ?atom, ?atom, ?atom, ?list, ?list), zero_or_more).
    % error_type(ID, Name, Severity, Description, DetectionMethods, PreventionMethods)

    error_type(
        'ERR-WF-001',
        unpinned_action,
        high,
        'GitHub Action uses version tag instead of SHA pin',
        [workflow_linter, grep_pattern],
        [pre_commit_hook, ci_check, code_review]
    ).

    error_type(
        'ERR-WF-002',
        missing_permissions,
        high,
        'Workflow file lacks explicit permissions declaration',
        [workflow_linter, grep_pattern],
        [pre_commit_hook, ci_check, template]
    ).

    error_type(
        'ERR-WF-003',
        missing_spdx,
        medium,
        'Workflow file missing SPDX license header',
        [grep_pattern],
        [pre_commit_hook, template]
    ).

    error_type(
        'ERR-WF-004',
        codeql_mismatch,
        medium,
        'CodeQL language matrix includes unsupported or non-existent languages',
        [workflow_run_failure, manual_review],
        [language_detection_hook, ci_check]
    ).

    error_type(
        'ERR-WF-005',
        duplicate_workflow,
        low,
        'Multiple workflow files doing the same thing',
        [manual_review, file_comparison],
        [template_standardization]
    ).

    error_type(
        'ERR-WF-006',
        undefined_secret,
        high,
        'Workflow references secret not defined in repo or org',
        [workflow_run_failure],
        [secret_validation_hook, conditional_guards]
    ).

    error_type(
        'ERR-WF-007',
        empty_workflow,
        low,
        'Workflow file is empty or contains only stub code',
        [file_size_check],
        [template_cleanup]
    ).

    error_type(
        'ERR-DEP-001',
        vulnerable_dependency,
        critical,
        'Dependency has known security vulnerability',
        [dependabot, trivy_scan],
        [dependabot_auto_update, renovate, lockfile]
    ).

    error_type(
        'ERR-SEC-001',
        missing_branch_protection,
        high,
        'Default branch lacks required protections',
        [scorecard, gh_api_check],
        [ruleset, branch_protection_api]
    ).

    error_type(
        'ERR-SEC-002',
        missing_security_md,
        medium,
        'Repository lacks SECURITY.md policy file',
        [file_existence_check],
        [template]
    ).

    %% Detection patterns for grep-based detection
    :- public(detection_pattern/3).
    :- mode(detection_pattern(?atom, ?atom, ?atom), zero_or_more).
    % detection_pattern(ErrorType, PatternType, Pattern)

    detection_pattern(unpinned_action, grep, 'uses:.*@v[0-9]').
    detection_pattern(missing_permissions, grep_negative, '^permissions:').
    detection_pattern(missing_spdx, grep_negative, '^# SPDX-License-Identifier:').

    %% SHA pins database (current as of 2025-12-31)
    :- public(action_sha/4).
    :- mode(action_sha(?atom, ?atom, ?atom, ?atom), zero_or_more).
    % action_sha(Owner, Repo, Version, SHA)

    action_sha(actions, checkout, 'v4.2.2', '11bd71901bbe5b1630ceea73d27597364c9af683').
    action_sha(github, 'codeql-action', 'v3', '662472033e021d55d94146f66f6058822b0b39fd').
    action_sha(ossf, 'scorecard-action', 'v2.4.0', '62b2cac7ed8198b15735ed49ab1e5cf35480ba46').
    action_sha(dtolnay, 'rust-toolchain', stable, '6d9817901c499d6b02debbb57edb38d33daa680b').
    action_sha('Swatinem', 'rust-cache', 'v2', 'ad397744b0d591a723ab90405b7247fac0e6b8db').
    action_sha(codecov, 'codecov-action', 'v5', '671740ac38dd9b0130fbe1cec585b89eea48d3de').
    action_sha(trufflesecurity, trufflehog, 'v3', '8a8ef8526528d8a4ff3e2c90be08e25ef8efbd9b').
    action_sha(webfactory, 'ssh-agent', 'v0.9.0', 'dc588b651fe13675774614f8e6a936a468676387').
    action_sha(ocaml, 'setup-ocaml', 'v3', '4c1df9105efb7a8b996c21e052e4fb8b64a8f2fc').
    action_sha(softprops, 'action-gh-release', 'v2', 'da05d552573ad5aba039eaac05058a918a7bf631').
    action_sha(actions, 'configure-pages', 'v5', '983d7736d9b0ae728b81ab479565c72886d7745b').
    action_sha(actions, 'jekyll-build-pages', 'v1', '44a6e6beabd48582f863aeeb6cb2151cc1716697').
    action_sha(actions, 'upload-pages-artifact', 'v3', '56afc609e74202658d3ffba0e8f6dda462b719fa').
    action_sha(actions, 'deploy-pages', 'v4', 'd6db90164ac5ed86f2b6aed7e0febac5b3c0c03e').
    action_sha(ruby, 'setup-ruby', 'v1', '4a9ddd6f338a97768b8006bf671dfbad383215f4').
    action_sha('editorconfig-checker', 'action-editorconfig-checker', main, '9f8f6065f4db902c0c56cafa67cea18b3ebbb680').

    %% Prevention mechanism definitions
    :- public(prevention_mechanism/4).
    :- mode(prevention_mechanism(?atom, ?atom, ?atom, ?list), zero_or_more).
    % prevention_mechanism(ID, Type, Location, Config)

    prevention_mechanism(
        pre_commit_sha_pins,
        git_hook,
        '.git/hooks/pre-commit',
        [script_path('hooks/validate-sha-pins.sh')]
    ).

    prevention_mechanism(
        pre_commit_spdx,
        git_hook,
        '.git/hooks/pre-commit',
        [script_path('hooks/validate-spdx.sh')]
    ).

    prevention_mechanism(
        pre_commit_permissions,
        git_hook,
        '.git/hooks/pre-commit',
        [script_path('hooks/validate-permissions.sh')]
    ).

    prevention_mechanism(
        ci_workflow_linter,
        github_action,
        '.github/workflows/workflow-linter.yml',
        [trigger([push, pull_request]), paths('.github/workflows/**')]
    ).

    prevention_mechanism(
        ci_scorecard,
        github_action,
        '.github/workflows/scorecard.yml',
        [trigger([push, schedule]), publishes_sarif(true)]
    ).

    prevention_mechanism(
        dependabot_config,
        dependabot,
        '.github/dependabot.yml',
        [ecosystems([npm, pip, cargo, github_actions]), schedule(weekly)]
    ).

    prevention_mechanism(
        asdf_tools,
        asdf,
        '.tool-versions',
        [tools([actionlint, gitleaks, trivy, cosign])]
    ).

    %% Query helpers
    :- public(errors_by_severity/2).
    :- mode(errors_by_severity(+atom, -list), one).
    errors_by_severity(Severity, Errors) :-
        findall(ID, error_type(ID, _, Severity, _, _, _), Errors).

    :- public(prevention_for_error/2).
    :- mode(prevention_for_error(+atom, -list), one).
    prevention_for_error(ErrorType, Preventions) :-
        error_type(_, ErrorType, _, _, _, Preventions).

    :- public(all_sha_pins/1).
    :- mode(all_sha_pins(-list), one).
    all_sha_pins(Pins) :-
        findall(
            pin(Owner, Repo, Version, SHA),
            action_sha(Owner, Repo, Version, SHA),
            Pins
        ).

:- end_object.

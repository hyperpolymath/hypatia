% SPDX-License-Identifier: PMPL-1.0-or-later
% Prevention Hooks - Git and CI prevention mechanisms
% Origin: security-audit/logtalk-db/prevention_hooks.lgt

:- object(prevention_hooks).

    :- info([
        version is 1:0:0,
        author is 'hyperpolymath',
        date is 2025-12-31,
        comment is 'Prevention mechanisms for security errors'
    ]).

    % Hook definitions: hook(Name, Type, TriggerEvent, Script, ApplicableErrors)
    :- public(hook/5).
    :- mode(hook(?atom, ?atom, ?atom, ?atom, ?list), zero_or_more).

    hook(
        validate_sha_pins,
        git_pre_commit,
        'pre-commit',
        'hooks/validate-sha-pins.sh',
        [unpinned_action]
    ).

    hook(
        validate_spdx_headers,
        git_pre_commit,
        'pre-commit',
        'hooks/validate-spdx.sh',
        [missing_spdx]
    ).

    hook(
        validate_workflow_permissions,
        git_pre_commit,
        'pre-commit',
        'hooks/validate-permissions.sh',
        [workflow_permission]
    ).

    hook(
        validate_codeql_matrix,
        git_pre_commit,
        'pre-commit',
        'hooks/validate-codeql.sh',
        [codeql_mismatch]
    ).

    hook(
        workflow_linter,
        github_action,
        'push/pull_request',
        '.github/workflows/workflow-linter.yml',
        [unpinned_action, missing_spdx, workflow_permission, empty_workflow]
    ).

    hook(
        scorecard_enforcer,
        github_action,
        'push/schedule',
        '.github/workflows/scorecard.yml',
        [branch_protection, workflow_permission, dependabot_vuln]
    ).

    % CI configuration templates
    :- public(ci_template/2).
    :- mode(ci_template(?atom, ?atom), zero_or_more).

    ci_template(dependabot, '.github/dependabot.yml').
    ci_template(codeowners, '.github/CODEOWNERS').
    ci_template(security_policy, 'SECURITY.md').
    ci_template(workflow_linter, '.github/workflows/workflow-linter.yml').
    ci_template(pre_commit_config, '.pre-commit-config.yaml').

    % ASDF tool version management
    :- public(asdf_tool/3).
    :- mode(asdf_tool(?atom, ?atom, ?atom), zero_or_more).
    asdf_tool(actionlint, 'latest', 'Validates GitHub Actions workflow files').
    asdf_tool(gitleaks, 'latest', 'Scans for secrets in git history').
    asdf_tool(trivy, 'latest', 'Security scanner for dependencies').
    asdf_tool(cosign, 'latest', 'Signs and verifies container images').

:- end_object.

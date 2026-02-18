% SPDX-License-Identifier: PMPL-1.0-or-later
% Security Error Database for Hyperpolymath Repos
% Logtalk Object for learning about CI/CD security errors
% Origin: security-audit/logtalk-db/security_errors.lgt

:- object(security_errors).

    :- info([
        version is 1:0:0,
        author is 'hyperpolymath',
        date is 2025-12-31,
        comment is 'Database of security errors and prevention mechanisms'
    ]).

    % Error categories
    :- public(error_category/2).
    :- mode(error_category(?atom, ?atom), zero_or_more).
    error_category(workflow_permission, 'Missing or incorrect workflow permissions').
    error_category(unpinned_action, 'GitHub Action not SHA-pinned').
    error_category(codeql_mismatch, 'CodeQL language matrix does not match repo').
    error_category(missing_spdx, 'SPDX license header missing').
    error_category(duplicate_workflow, 'Duplicate workflow files').
    error_category(missing_secret, 'Workflow references undefined secret').
    error_category(dependabot_vuln, 'Dependabot security vulnerability').
    error_category(code_scanning, 'Code scanning alert').
    error_category(branch_protection, 'Missing branch protection').
    error_category(empty_workflow, 'Empty or stub workflow file').

    % Error severity levels
    :- public(severity/2).
    :- mode(severity(?atom, ?integer), zero_or_more).
    severity(critical, 1).
    severity(high, 2).
    severity(medium, 3).
    severity(low, 4).
    severity(info, 5).

    % Prevention mechanisms
    :- public(prevention/3).
    :- mode(prevention(?atom, ?atom, ?atom), zero_or_more).
    prevention(unpinned_action, git_hook, 'pre-commit hook to validate SHA pins').
    prevention(unpinned_action, ci_check, 'workflow-linter.yml validates pins').
    prevention(missing_spdx, git_hook, 'pre-commit hook to check SPDX headers').
    prevention(workflow_permission, ci_check, 'scorecard workflow checks permissions').
    prevention(codeql_mismatch, git_hook, 'pre-commit validates CodeQL matrix').
    prevention(dependabot_vuln, dependabot, 'dependabot.yml auto-updates').
    prevention(branch_protection, github_ruleset, 'Ruleset enforces reviews').

    % SHA pins for common actions (reference database)
    :- public(action_sha/3).
    :- mode(action_sha(?atom, ?atom, ?atom), zero_or_more).
    action_sha('actions/checkout', 'v4', 'b4ffde65f46336ab88eb53be808477a3936bae11').
    action_sha('github/codeql-action', 'v3', '662472033e021d55d94146f66f6058822b0b39fd').
    action_sha('ossf/scorecard-action', 'v2.4.0', '62b2cac7ed8198b15735ed49ab1e5cf35480ba46').
    action_sha('dtolnay/rust-toolchain', 'stable', '6d9817901c499d6b02debbb57edb38d33daa680b').
    action_sha('Swatinem/rust-cache', 'v2', 'ad397744b0d591a723ab90405b7247fac0e6b8db').
    action_sha('codecov/codecov-action', 'v5', '671740ac38dd9b0130fbe1cec585b89eea48d3de').
    action_sha('trufflesecurity/trufflehog', 'v3', '8a8ef8526528d8a4ff3e2c90be08e25ef8efbd9b').
    action_sha('webfactory/ssh-agent', 'v0.9.0', 'dc588b651fe13675774614f8e6a936a468676387').
    action_sha('ocaml/setup-ocaml', 'v3', '4c1df9105efb7a8b996c21e052e4fb8b64a8f2fc').
    action_sha('softprops/action-gh-release', 'v2', 'da05d552573ad5aba039eaac05058a918a7bf631').
    action_sha('actions/configure-pages', 'v5', '983d7736d9b0ae728b81ab479565c72886d7745b').
    action_sha('actions/jekyll-build-pages', 'v1', '44a6e6beabd48582f863aeeb6cb2151cc1716697').
    action_sha('actions/upload-pages-artifact', 'v3', '56afc609e74202658d3ffba0e8f6dda462b719fa').
    action_sha('actions/deploy-pages', 'v4', 'd6db90164ac5ed86f2b6aed7e0febac5b3c0c03e').
    action_sha('ruby/setup-ruby', 'v1.207.0', '4a9ddd6f338a97768b8006bf671dfbad383215f4').
    action_sha('editorconfig-checker/action-editorconfig-checker', 'main', '9f8f6065f4db902c0c56cafa67cea18b3ebbb680').
    action_sha('slsa-framework/slsa-github-generator', 'v2.1.0', 'f7dd8c54c2067bafc12ca7a55595d5ee9b75204a').
    action_sha('google/clusterfuzzlite/actions/build_fuzzers', 'v1', '884713a6c30a92e5e8544c39945cd7cb630abcd1').
    action_sha('google/clusterfuzzlite/actions/run_fuzzers', 'v1', '884713a6c30a92e5e8544c39945cd7cb630abcd1').

    % CodeQL language support
    :- public(codeql_language/2).
    :- mode(codeql_language(?atom, ?list), zero_or_more).
    codeql_language(javascript, ['.js', '.jsx', '.ts', '.tsx', '.mjs', '.cjs']).
    codeql_language(python, ['.py']).
    codeql_language(go, ['.go']).
    codeql_language('java-kotlin', ['.java', '.kt']).
    codeql_language(ruby, ['.rb']).
    codeql_language(csharp, ['.cs']).
    codeql_language(cpp, ['.cpp', '.c', '.h', '.hpp']).
    codeql_language(swift, ['.swift']).
    % Languages NOT supported by CodeQL (use 'actions' for workflow scanning)
    codeql_language(unsupported, ['.rs', '.ml', '.mli', '.res', '.resi', '.nim', '.zig']).

:- end_object.

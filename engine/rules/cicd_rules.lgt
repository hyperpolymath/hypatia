%% SPDX-License-Identifier: AGPL-3.0-or-later
%% cicd-hyper-a Rule Engine
%% Learned rules from 228 repos, 1922 alerts across hyperpolymath org

:- object(cicd_rules).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2025-12-29,
        comment is 'Core CI/CD rules distilled from neural learning'
    ]).

    :- public([
        repo_must_have/2,
        block_commit_if/2,
        auto_fix/2,
        classify_severity/2,
        is_auto_fixable/1,
        suggest_fix/2,
        get_prevention_workflow/2,
        %% Learning integration
        suggest_fix_with_learning/2,
        record_fix_outcome/3,
        get_best_fix/2
    ]).

    %% ============================================================
    %% DECLARATIVE RULES - What repos must have
    %% ============================================================

    repo_must_have(Repo, 'SECURITY.md') :-
        repo_is_public(Repo).

    repo_must_have(Repo, '.github/dependabot.yml') :-
        repo_uses_dependencies(Repo).

    repo_must_have(Repo, '.github/workflows/scorecard.yml') :-
        repo_is_public(Repo).

    repo_must_have(Repo, 'permissions: read-all') :-
        repo_has_workflows(Repo).

    %% ============================================================
    %% PREVENTIVE RULES - Block bad commits
    %% ============================================================

    block_commit_if(Commit, typescript_detected) :-
        commit_adds_file(Commit, File),
        file_extension(File, '.ts').

    block_commit_if(Commit, nodejs_detected) :-
        commit_adds_file(Commit, 'package-lock.json').

    block_commit_if(Commit, golang_detected) :-
        commit_adds_file(Commit, File),
        file_extension(File, '.go').

    block_commit_if(Commit, python_detected) :-
        commit_adds_file(Commit, File),
        file_extension(File, '.py'),
        \+ file_in_saltstack(File).

    block_commit_if(Commit, makefile_detected) :-
        commit_adds_file(Commit, 'Makefile').

    block_commit_if(Commit, secret_detected) :-
        commit_content_matches(Commit, Pattern),
        secret_pattern(Pattern).

    block_commit_if(Commit, unpinned_action) :-
        commit_modifies_workflow(Commit),
        workflow_has_unpinned_action(Commit).

    block_commit_if(Commit, missing_permissions) :-
        commit_modifies_workflow(Commit),
        workflow_missing_permissions(Commit).

    block_commit_if(Commit, missing_spdx) :-
        commit_adds_file(Commit, File),
        \+ file_has_spdx_header(File).

    %% ============================================================
    %% CURATIVE RULES - Auto-fix existing issues
    %% ============================================================

    auto_fix(Repo, unpinned_actions) :-
        find_unpinned_actions(Repo, Actions),
        pin_actions_to_sha(Repo, Actions).

    auto_fix(Repo, missing_permissions) :-
        find_workflows_missing_permissions(Repo, Workflows),
        add_read_all_permissions(Repo, Workflows).

    auto_fix(Repo, missing_spdx) :-
        find_files_missing_spdx(Repo, Files),
        add_spdx_headers(Repo, Files).

    auto_fix(Repo, missing_security_md) :-
        \+ repo_has_file(Repo, 'SECURITY.md'),
        inject_security_md(Repo).

    auto_fix(Repo, missing_branch_protection) :-
        \+ repo_has_branch_protection(Repo),
        enable_branch_protection(Repo).

    auto_fix(Repo, unused_local_variable) :-
        find_unused_variables(Repo, Vars),
        remove_or_prefix_underscore(Repo, Vars).

    %% ============================================================
    %% SEVERITY CLASSIFICATION - From 1922 alert analysis
    %% ============================================================

    classify_severity('hard-coded-cryptographic-value', critical).
    classify_severity('sql-injection', critical).
    classify_severity('VulnerabilitiesID', critical).

    classify_severity('remote-property-injection', high).
    classify_severity('TokenPermissionsID', high).
    classify_severity('missing-workflow-permissions', high).

    classify_severity('PinnedDependenciesID', medium).
    classify_severity('SecurityPolicyID', medium).
    classify_severity('BranchProtectionID', medium).
    classify_severity('CodeReviewID', medium).
    classify_severity('syntax-error', medium).

    classify_severity('unused-local-variable', low).
    classify_severity('MaintainedID', low).
    classify_severity('FuzzingID', low).
    classify_severity('CIIBestPracticesID', low).

    classify_severity(_, info).

    %% ============================================================
    %% AUTO-FIXABLE DETECTION - From training data
    %% ============================================================

    is_auto_fixable('TokenPermissionsID').
    is_auto_fixable('PinnedDependenciesID').
    is_auto_fixable('missing-workflow-permissions').
    is_auto_fixable('SecurityPolicyID').
    is_auto_fixable('BranchProtectionID').
    is_auto_fixable('unused-local-variable').
    is_auto_fixable('missing-spdx-header').

    %% ============================================================
    %% FIX SUGGESTIONS - Distilled from 228 repo fixes
    %% ============================================================

    suggest_fix('TokenPermissionsID', 'Add "permissions: read-all" at workflow level').
    suggest_fix('PinnedDependenciesID', 'Replace @vN with @SHA # vN format').
    suggest_fix('missing-workflow-permissions', 'Add "permissions: read-all" at workflow level').
    suggest_fix('SecurityPolicyID', 'Create SECURITY.md with vulnerability reporting instructions').
    suggest_fix('BranchProtectionID', 'Enable branch protection via Settings > Branches').
    suggest_fix('CodeReviewID', 'Require at least 1 approving review before merge').
    suggest_fix('CIIBestPracticesID', 'Register at bestpractices.coreinfrastructure.org').
    suggest_fix('FuzzingID', 'Add ClusterFuzzLite or cargo-fuzz infrastructure').
    suggest_fix('VulnerabilitiesID', 'Run cargo audit / npm audit and update dependencies').
    suggest_fix('hard-coded-cryptographic-value', 'Use environment variables or secrets manager').
    suggest_fix('remote-property-injection', 'Add allowlist validation for dynamic property access').
    suggest_fix('unused-local-variable', 'Remove unused code or prefix with underscore').
    suggest_fix('syntax-error', 'Fix syntax error using linter output').

    %% ============================================================
    %% PREVENTION WORKFLOWS - Maps alerts to workflows
    %% ============================================================

    get_prevention_workflow('TokenPermissionsID', 'workflow-linter.yml').
    get_prevention_workflow('PinnedDependenciesID', 'workflow-linter.yml').
    get_prevention_workflow('missing-workflow-permissions', 'workflow-linter.yml').
    get_prevention_workflow('hard-coded-cryptographic-value', 'secret-scanner.yml').
    get_prevention_workflow('VulnerabilitiesID', 'cargo-audit.yml').
    get_prevention_workflow('SecurityPolicyID', 'scorecard-enforcer.yml').
    get_prevention_workflow('BranchProtectionID', 'scorecard-enforcer.yml').
    get_prevention_workflow('CodeReviewID', 'scorecard-enforcer.yml').

    %% ============================================================
    %% SECRET PATTERNS - Learned from secret scanning
    %% ============================================================

    secret_pattern('(?i)api[_-]?key\\s*[=:]\\s*["\'][^"\']+["\']').
    secret_pattern('(?i)secret\\s*[=:]\\s*["\'][^"\']+["\']').
    secret_pattern('(?i)password\\s*[=:]\\s*["\'][^"\']+["\']').
    secret_pattern('ghp_[a-zA-Z0-9]{36}').  % GitHub PAT
    secret_pattern('github_pat_[a-zA-Z0-9]{22}_[a-zA-Z0-9]{59}'). % Fine-grained PAT
    secret_pattern('glpat-[a-zA-Z0-9\\-]{20}'). % GitLab PAT
    secret_pattern('AKIA[0-9A-Z]{16}'). % AWS Access Key
    %% Additional patterns added 2026-01-09 from security scan session
    secret_pattern('xoxb-[0-9]{10,}-[0-9]{10,}-[a-zA-Z0-9]+').  % Slack Bot Token
    secret_pattern('xoxp-[0-9]{10,}-[0-9]{10,}-[a-zA-Z0-9]+').  % Slack User Token
    secret_pattern('pypi-AgE[a-zA-Z0-9\\-_]{50,}').  % PyPI API Token
    secret_pattern('-----BEGIN [A-Z]+ PRIVATE KEY-----').  % SSH/PGP Private Keys
    secret_pattern('sk-[a-zA-Z0-9]{48}').  % OpenAI API Key
    secret_pattern('sk-proj-[a-zA-Z0-9]{48}').  % OpenAI Project Key
    secret_pattern('sk-ant-api[0-9]{2}-[a-zA-Z0-9\\-_]{86}').  % Anthropic API Key
    secret_pattern('npm_[a-zA-Z0-9]{36}').  % npm Access Token
    secret_pattern('snyk_[a-zA-Z0-9\\-]{36}').  % Snyk API Token
    secret_pattern('SG\\.[a-zA-Z0-9\\-_]{22}\\.[a-zA-Z0-9\\-_]{43}').  % SendGrid API Key

    %% ============================================================
    %% RSR LANGUAGE POLICY
    %% ============================================================

    allowed_language(rescript).
    allowed_language(rust).
    allowed_language(gleam).
    allowed_language(julia).
    allowed_language(logtalk).
    allowed_language(haskell).
    allowed_language(bash).
    allowed_language(nickel).
    allowed_language(guile).
    allowed_language(ocaml).
    allowed_language(ada).

    banned_language(typescript).
    banned_language(nodejs).
    banned_language(golang).
    banned_language(python).  % Except SaltStack
    banned_language(java).
    banned_language(kotlin).
    banned_language(swift).

    %% ============================================================
    %% HELPER PREDICATES
    %% ============================================================

    file_extension(File, Ext) :-
        atom_concat(_, Ext, File).

    file_in_saltstack(File) :-
        atom_concat('salt/', _, File).

    %% ============================================================
    %% CODEQL LANGUAGE DETECTION - Added 2026-01-09
    %% Maps repo languages to valid CodeQL languages
    %% ============================================================

    %% CodeQL supported languages
    codeql_supported('javascript-typescript').
    codeql_supported('python').
    codeql_supported('go').
    codeql_supported('java-kotlin').
    codeql_supported('ruby').
    codeql_supported('csharp').
    codeql_supported('cpp').
    codeql_supported('swift').
    codeql_supported('actions').  % GitHub Actions workflow scanning

    %% CodeQL NOT supported (use 'actions' instead)
    codeql_unsupported(rust).
    codeql_unsupported(ocaml).
    codeql_unsupported(haskell).
    codeql_unsupported(ada).
    codeql_unsupported(nickel).
    codeql_unsupported(logtalk).
    codeql_unsupported(julia).
    codeql_unsupported(gleam).

    %% Validate CodeQL language matrix for a repo
    validate_codeql_matrix(RepoLanguages, ValidMatrix) :-
        findall(Lang,
            (member(Lang, RepoLanguages), codeql_supported(Lang)),
            SupportedLangs),
        ( SupportedLangs = [] ->
            ValidMatrix = ['actions']  % Fallback to actions scanning
        ; ValidMatrix = SupportedLangs
        ).

    %% Detect language mismatch in CodeQL config
    detect_codeql_mismatch(RepoType, ConfiguredLang, Reason) :-
        codeql_unsupported(RepoType),
        ConfiguredLang \= 'actions',
        format(atom(Reason), '~w is not supported by CodeQL, use actions instead', [RepoType]).

    %% ============================================================
    %% LICENSE HEADER VALIDATION - Added 2026-01-09
    %% Enforces correct SPDX license identifiers
    %% ============================================================

    %% Required license for hyperpolymath org
    required_spdx_license('AGPL-3.0-or-later').

    %% Common wrong licenses found in repos
    wrong_spdx_license('MPL-2.0').
    wrong_spdx_license('MIT').
    wrong_spdx_license('Apache-2.0').

    %% Block commit if wrong license header
    block_commit_if(Commit, wrong_license_header) :-
        commit_adds_file(Commit, File),
        file_has_spdx_header(File, License),
        wrong_spdx_license(License).

    %% Auto-fix suggestion for license headers
    is_auto_fixable('wrong-spdx-license').
    suggest_fix('wrong-spdx-license', 'Change SPDX-License-Identifier to AGPL-3.0-or-later').
    classify_severity('wrong-spdx-license', high).

    %% Auto-fix license header
    auto_fix(Repo, wrong_license_header) :-
        find_files_with_wrong_license(Repo, Files),
        fix_license_headers(Repo, Files).

    %% ============================================================
    %% LEARNING INTEGRATION
    %% ============================================================

    %% Suggest fix using learned knowledge first, then static rules
    suggest_fix_with_learning(IssueType, Fix) :-
        ( learning::recommend_fix(IssueType, LearnedFix),
          learning::get_confidence(IssueType, Conf),
          Conf >= 0.7 ->
            Fix = LearnedFix
        ; suggest_fix(IssueType, Fix)
        ).

    %% Record the outcome of applying a fix (feeds back into learning)
    %% Outcome: success | failure | partial
    record_fix_outcome(IssueType, Fix, Outcome) :-
        learning::learn_from_fix(IssueType, Fix, Outcome).

    %% Get best fix combining static rules and learned knowledge
    %% Returns: fix(Fix, Source, Confidence) where Source is static | learned
    get_best_fix(IssueType, fix(Fix, Source, Confidence)) :-
        ( learning::get_confidence(IssueType, LearnedConf),
          LearnedConf >= 0.75,
          learning::recommend_fix(IssueType, LearnedFix) ->
            Fix = LearnedFix,
            Source = learned,
            Confidence = LearnedConf
        ; suggest_fix(IssueType, StaticFix) ->
            Fix = StaticFix,
            Source = static,
            Confidence = 1.0
        ; Fix = 'No fix available',
          Source = none,
          Confidence = 0.0
        ).

:- end_object.

%% SPDX-License-Identifier: PMPL-1.0-or-later
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
        get_best_fix/2,
        %% CI/CD waste detection (2026-01-18)
        detect_workflow_waste/2,
        is_waste_pattern/1,
        suggest_waste_fix/2,
        repo_language_for_tool/3
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
    %% Session 2026-01-09: New severity classifications
    classify_severity('upload-pages-artifact-transitive-deps', high).
    classify_severity('scorecard-run-step-restriction', medium).
    classify_severity('rust-toolchain-sha-missing-input', medium).
    classify_severity('deno-lint-include-pattern', low).
    classify_severity('workflow-linter-self-detection', low).

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
    %% Session 2026-01-09: New auto-fixable patterns
    is_auto_fixable('upload-pages-artifact-transitive-deps').
    is_auto_fixable('scorecard-run-step-restriction').
    is_auto_fixable('deno-lint-include-pattern').
    is_auto_fixable('rust-toolchain-sha-missing-input').
    is_auto_fixable('workflow-linter-self-detection').

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
    %% Session 2026-01-09: New fix suggestions
    suggest_fix('upload-pages-artifact-transitive-deps',
        'Update to actions/upload-pages-artifact@7b1f4a764d45c48632c6b24a0339c27f5614fb0b (v4)').
    suggest_fix('scorecard-run-step-restriction',
        'Move run: steps from scorecard job to separate check-critical job').
    suggest_fix('deno-lint-include-pattern',
        'Add explicit include patterns to deno.json lint config or use deno task lint').
    suggest_fix('rust-toolchain-sha-missing-input',
        'Add "with: toolchain: stable" to dtolnay/rust-toolchain SHA-pinned step').
    suggest_fix('workflow-linter-self-detection',
        'Add grep -v filters for grep commands, echo statements, and comments').

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
    %% Session 2026-01-09: New prevention workflow mappings
    get_prevention_workflow('upload-pages-artifact-transitive-deps', 'workflow-linter.yml').
    get_prevention_workflow('scorecard-run-step-restriction', 'scorecard-enforcer.yml').
    get_prevention_workflow('rust-toolchain-sha-missing-input', 'rust-ci.yml').
    get_prevention_workflow('workflow-linter-self-detection', 'workflow-linter.yml').

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
    required_spdx_license('PMPL-1.0-or-later').

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
    suggest_fix('wrong-spdx-license', 'Change SPDX-License-Identifier to PMPL-1.0-or-later').
    classify_severity('wrong-spdx-license', high).

    %% Auto-fix license header
    auto_fix(Repo, wrong_license_header) :-
        find_files_with_wrong_license(Repo, Files),
        fix_license_headers(Repo, Files).

    %% ============================================================
    %% CI/CD WASTE DETECTION - Added 2026-01-18
    %% Rules for detecting wasteful/redundant CI runs
    %% ============================================================

    :- public([
        detect_workflow_waste/2,
        is_waste_pattern/1,
        suggest_waste_fix/2,
        repo_language_for_tool/3
    ]).

    %% Waste pattern detection
    is_waste_pattern(duplicate_workflow).
    is_waste_pattern(unused_publish_workflows).
    is_waste_pattern(mirror_missing_secrets).
    is_waste_pattern(npm_in_workflow).
    is_waste_pattern(spec_repo_full_ci).
    is_waste_pattern(semgrep_language_mismatch).
    is_waste_pattern(excessive_workflow_count).
    is_waste_pattern(missing_directory_workflow).

    %% Severity for waste patterns
    classify_severity(duplicate_workflow, medium).
    classify_severity(unused_publish_workflows, low).
    classify_severity(mirror_missing_secrets, medium).
    classify_severity(npm_in_workflow, high).
    classify_severity(spec_repo_full_ci, medium).
    classify_severity(semgrep_language_mismatch, medium).
    classify_severity(excessive_workflow_count, low).
    classify_severity(missing_directory_workflow, low).

    %% Auto-fixable waste patterns
    is_auto_fixable(duplicate_workflow).
    is_auto_fixable(unused_publish_workflows).
    is_auto_fixable(mirror_missing_secrets).
    is_auto_fixable(npm_in_workflow).
    is_auto_fixable(spec_repo_full_ci).
    is_auto_fixable(semgrep_language_mismatch).
    is_auto_fixable(missing_directory_workflow).
    %% excessive_workflow_count NOT auto-fixable (requires judgment)

    %% Fix suggestions for waste patterns
    suggest_fix(duplicate_workflow,
        'Delete the less comprehensive duplicate workflow; keep the more complete one').
    suggest_fix(unused_publish_workflows,
        'Delete unused publish-*.yml files or consolidate into single workflow with matrix').
    suggest_fix(mirror_missing_secrets,
        'Configure GITLAB_SSH_KEY/BITBUCKET_SSH_KEY org secrets or delete mirror.yml').
    suggest_fix(npm_in_workflow,
        'Replace npm install/npx with Deno or pinned binary; remove pnpm/action-setup').
    suggest_fix(spec_repo_full_ci,
        'Reduce to minimal CI: policy checks only; remove CodeQL/Semgrep/build workflows').
    suggest_fix(semgrep_language_mismatch,
        'Remove semgrep.yml from repos without Python/Go/JS source files').
    suggest_fix(excessive_workflow_count,
        'Consolidate workflows; remove unused templates; use matrix builds').
    suggest_fix(missing_directory_workflow,
        'Delete workflow or create the expected directory structure').

    %% Detect duplicate workflows
    detect_workflow_waste(Repo, duplicate_workflow) :-
        repo_has_workflow(Repo, 'rust.yml'),
        repo_has_workflow(Repo, 'rust-ci.yml').
    detect_workflow_waste(Repo, duplicate_workflow) :-
        repo_has_workflow(Repo, 'codeql.yml'),
        repo_has_workflow(Repo, 'codeql-analysis.yml').
    detect_workflow_waste(Repo, duplicate_workflow) :-
        repo_has_workflow(Repo, 'ci.yml'),
        repo_has_workflow(Repo, 'build.yml').

    %% Detect unused publish workflows (>5 platform-specific publish workflows)
    detect_workflow_waste(Repo, unused_publish_workflows) :-
        findall(W, (repo_has_workflow(Repo, W), atom_concat('publish-', _, W)), PublishWorkflows),
        length(PublishWorkflows, Count),
        Count >= 5.

    %% Detect mirror without secrets
    detect_workflow_waste(Repo, mirror_missing_secrets) :-
        repo_has_workflow(Repo, 'mirror.yml'),
        \+ repo_has_secret(Repo, 'GITLAB_SSH_KEY'),
        \+ repo_has_secret(Repo, 'BITBUCKET_SSH_KEY').

    %% Detect npm usage in workflow despite blocker
    detect_workflow_waste(Repo, npm_in_workflow) :-
        repo_has_workflow(Repo, Workflow),
        workflow_contains(Workflow, 'npm install'),
        repo_has_workflow(Repo, 'npm-bun-blocker.yml').
    detect_workflow_waste(Repo, npm_in_workflow) :-
        repo_has_workflow(Repo, Workflow),
        workflow_contains(Workflow, 'pnpm/action-setup'),
        repo_has_workflow(Repo, 'npm-bun-blocker.yml').

    %% Detect spec-only repos running full CI
    detect_workflow_waste(Repo, spec_repo_full_ci) :-
        repo_is_spec_only(Repo),
        repo_workflow_count(Repo, Count),
        Count >= 10.

    %% Detect Semgrep on wrong languages
    detect_workflow_waste(Repo, semgrep_language_mismatch) :-
        repo_has_workflow(Repo, 'semgrep.yml'),
        repo_languages(Repo, Languages),
        \+ member(python, Languages),
        \+ member(javascript, Languages),
        \+ member(typescript, Languages),
        \+ member(go, Languages).

    %% Detect excessive workflow count (>15)
    detect_workflow_waste(Repo, excessive_workflow_count) :-
        repo_workflow_count(Repo, Count),
        Count > 15.

    %% Detect workflows checking for missing directories
    detect_workflow_waste(Repo, missing_directory_workflow) :-
        repo_has_workflow(Repo, 'zig-ffi.yml'),
        \+ repo_has_directory(Repo, 'zig').

    %% Tool language support facts
    %% repo_language_for_tool(Tool, RepoLang, ToolLang)
    repo_language_for_tool(codeql, javascript, 'javascript-typescript').
    repo_language_for_tool(codeql, typescript, 'javascript-typescript').
    repo_language_for_tool(codeql, python, 'python').
    repo_language_for_tool(codeql, go, 'go').
    repo_language_for_tool(codeql, java, 'java-kotlin').
    repo_language_for_tool(codeql, kotlin, 'java-kotlin').
    repo_language_for_tool(codeql, ruby, 'ruby').
    repo_language_for_tool(codeql, csharp, 'csharp').
    repo_language_for_tool(codeql, cpp, 'cpp').
    repo_language_for_tool(codeql, c, 'cpp').
    repo_language_for_tool(codeql, swift, 'swift').
    %% Rust, OCaml, Haskell, etc. -> use 'actions' only
    repo_language_for_tool(codeql, rust, 'actions').
    repo_language_for_tool(codeql, ocaml, 'actions').
    repo_language_for_tool(codeql, haskell, 'actions').
    repo_language_for_tool(codeql, ada, 'actions').
    repo_language_for_tool(codeql, rescript, 'actions').
    repo_language_for_tool(codeql, gleam, 'actions').

    repo_language_for_tool(semgrep, python, python).
    repo_language_for_tool(semgrep, javascript, javascript).
    repo_language_for_tool(semgrep, typescript, typescript).
    repo_language_for_tool(semgrep, go, go).
    repo_language_for_tool(semgrep, java, java).
    repo_language_for_tool(semgrep, ruby, ruby).
    %% Semgrep doesn't support these well
    repo_language_for_tool(semgrep, rust, unsupported).
    repo_language_for_tool(semgrep, ocaml, unsupported).
    repo_language_for_tool(semgrep, rescript, unsupported).

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

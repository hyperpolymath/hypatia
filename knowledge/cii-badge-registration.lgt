% SPDX-License-Identifier: PMPL-1.0-or-later
% Hypatia Knowledge Base: CII Best Practices Badge Registration
% Learned from session 2026-02-04
%
% This module encodes the complete process for registering OpenSSF
% Best Practices badges (formerly CII Best Practices).

:- object(cii_badge_registration).

    :- info([
        version is 1:0:0,
        author is 'Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>',
        date is 2026-02-04,
        comment is 'Knowledge about CII Best Practices badge registration and automation'
    ]).

    % Repository Readiness Assessment
    :- public(assess_readiness/2).
    :- mode(assess_readiness(+atom, -list), one).
    :- info(assess_readiness/2, [
        comment is 'Assess if a repository is ready for CII badge registration',
        argnames is ['RepoName', 'ReadinessReport']
    ]).

    assess_readiness(Repo, Report) :-
        check_required_files(Repo, FilesStatus),
        check_workflows(Repo, WorkflowsStatus),
        check_documentation(Repo, DocsStatus),
        check_security_practices(Repo, SecurityStatus),
        overall_readiness(FilesStatus, WorkflowsStatus, DocsStatus, SecurityStatus, Report).

    % Required Files Check
    :- public(check_required_files/2).
    :- mode(check_required_files(+atom, -list), one).

    check_required_files(Repo, [
        license-Status,
        security_policy-SecurityStatus,
        contributing_guide-ContribStatus,
        readme-ReadmeStatus
    ]) :-
        (file_exists(Repo, 'LICENSE') ; file_exists(Repo, 'LICENSE.txt') ->
            Status = present ; Status = missing),
        (file_exists(Repo, 'SECURITY.md') ->
            SecurityStatus = present ; SecurityStatus = missing),
        (file_exists(Repo, 'CONTRIBUTING.md') ->
            ContribStatus = present ; ContribStatus = missing),
        (file_exists(Repo, 'README.adoc') ; file_exists(Repo, 'README.md') ->
            ReadmeStatus = present ; ReadmeStatus = missing).

    % Workflow Requirements
    :- public(required_workflows/1).
    :- mode(required_workflows(-list), one).

    required_workflows([
        'codeql.yml',           % Static analysis
        'scorecard.yml',         % OpenSSF Scorecard
        'quality.yml'            % TruffleHog, EditorConfig
    ]).

    % Additional workflows for Rust projects
    :- public(rust_workflows/1).
    :- mode(rust_workflows(-list), one).

    rust_workflows([
        'cflite_pr.yml',         % Fuzzing on PR
        'cflite_batch.yml'       % Weekly batch fuzzing
    ]).

    % CII Badge Registration Form Fields
    :- public(registration_field/3).
    :- mode(registration_field(+atom, +atom, -atom), one).
    :- info(registration_field/3, [
        comment is 'Standard answer for a registration field for a given repo',
        argnames is ['FieldName', 'RepoName', 'Answer']
    ]).

    % Universal fields (same for all repos)
    registration_field(license, _, 'PMPL-1.0-or-later').
    registration_field(security_contact, _, 'security@hyperpolymath.org').
    registration_field(version_control, _, 'Git (GitHub)').
    registration_field(vulnerability_response_time, _, 'Within 14 days').
    registration_field(https_website, _, 'Yes - GitHub provides HTTPS').
    registration_field(developer_guidelines, _, 'Yes - CONTRIBUTING.md').
    registration_field(code_review_required, _, 'Yes - Pull request required (branch protection)').
    registration_field(ci_system, _, 'Yes - GitHub Actions').
    registration_field(static_analysis_tool, _, 'CodeQL').
    registration_field(vulnerability_tracking, _, 'Yes - GitHub Dependabot + Security Advisories').

    % Repo-specific fields
    registration_field(homepage_url, Repo, URL) :-
        atom_concat('https://github.com/hyperpolymath/', Repo, URL).

    registration_field(repository_url, Repo, URL) :-
        atom_concat('https://github.com/hyperpolymath/', Repo, URL).

    registration_field(bug_tracker_url, Repo, URL) :-
        atom_concat('https://github.com/hyperpolymath/', Repo, '/issues', URL).

    registration_field(description, bunsenite, 'Nickel-based configuration management and validation').
    registration_field(description, proven, 'Formally verified Idris2 libraries with dependent types').
    registration_field(description, hypatia, 'Neurosymbolic CI/CD intelligence platform').
    registration_field(description, 'robot-repo-automaton', 'Automated repository management').
    registration_field(description, gitvisor, 'Git repository supervisor and compliance tool').
    registration_field(description, scaffoldia, 'Project scaffolding and templating system').
    registration_field(description, 'dicti0nary-attack', 'Password generation and testing utilities').
    registration_field(description, eclexia, 'Data extraction and processing toolkit').
    registration_field(description, ephapax, 'Formally verified systems programming').
    registration_field(description, 'llm-unify', 'LLM integration framework').
    registration_field(description, 'proven-tui', 'Terminal UI framework with formal verification').
    registration_field(description, neurophone, 'Neural network audio processing').
    registration_field(description, 'protocol-squisher', 'Network protocol compression').
    registration_field(description, 'file-soup', 'File processing and parsing library').
    registration_field(description, 'rescript-openapi', 'OpenAPI specification tools for ReScript').
    registration_field(description, 'language-interop-compiler', 'Multi-language interoperability compiler').
    registration_field(description, betlang, 'Experimental programming language').

    % Language-specific answers
    registration_field(primary_language, bunsenite, 'Rust').
    registration_field(primary_language, proven, 'Idris2').
    registration_field(primary_language, hypatia, 'Rust + Logtalk').
    registration_field(primary_language, 'robot-repo-automaton', 'Rust').
    registration_field(primary_language, gitvisor, 'Rust').
    registration_field(primary_language, scaffoldia, 'Haskell').
    registration_field(primary_language, 'dicti0nary-attack', 'Rust').
    registration_field(primary_language, eclexia, 'Rust').
    registration_field(primary_language, ephapax, 'Idris2').
    registration_field(primary_language, 'llm-unify', 'Rust').
    registration_field(primary_language, 'proven-tui', 'Rust').
    registration_field(primary_language, neurophone, 'Rust').
    registration_field(primary_language, 'protocol-squisher', 'Rust').
    registration_field(primary_language, 'file-soup', 'Rust').
    registration_field(primary_language, 'rescript-openapi', 'ReScript').
    registration_field(primary_language, 'language-interop-compiler', 'Rust').
    registration_field(primary_language, betlang, 'Rust').

    % Memory safety mechanisms
    registration_field(memory_safety, Repo, Answer) :-
        registration_field(primary_language, Repo, Lang),
        memory_safety_for_language(Lang, Answer).

    memory_safety_for_language('Rust', 'Yes - Rust language guarantees + AddressSanitizer + UndefinedBehaviorSanitizer (fuzzing)').
    memory_safety_for_language('Idris2', 'Yes - Dependent types provide mathematical proofs of correctness').
    memory_safety_for_language('Haskell', 'Yes - Type safety + managed runtime').
    memory_safety_for_language('ReScript', 'Yes - Type safety + managed JavaScript runtime').
    memory_safety_for_language('Rust + Logtalk', 'Yes - Rust memory safety + formal logic verification').

    % Fuzzing availability
    registration_field(dynamic_analysis, Repo, Answer) :-
        registration_field(primary_language, Repo, Lang),
        fuzzing_for_language(Lang, Repo, Answer).

    fuzzing_for_language('Rust', _, 'Yes - ClusterFuzzLite fuzzing (.github/workflows/cflite_pr.yml and cflite_batch.yml)').
    fuzzing_for_language('Idris2', _, 'Yes - Property-based testing and automated test suite').
    fuzzing_for_language(_, _, 'Yes - Automated testing in CI').

    % Registration Priority Tiers
    :- public(registration_tier/2).
    :- mode(registration_tier(+atom, -atom), one).
    :- info(registration_tier/2, [
        comment is 'Priority tier for badge registration (tier1 = highest)',
        argnames is ['RepoName', 'Tier']
    ]).

    registration_tier(bunsenite, tier1).
    registration_tier(proven, tier1).
    registration_tier(hypatia, tier1).
    registration_tier('robot-repo-automaton', tier2).
    registration_tier(gitvisor, tier2).
    registration_tier(scaffoldia, tier2).
    registration_tier('dicti0nary-attack', tier3).
    registration_tier(eclexia, tier3).
    registration_tier(ephapax, tier3).
    registration_tier('llm-unify', tier3).
    registration_tier('proven-tui', tier4).
    registration_tier(neurophone, tier4).
    registration_tier('protocol-squisher', tier4).
    registration_tier('file-soup', tier4).
    registration_tier('rescript-openapi', tier5).
    registration_tier('language-interop-compiler', tier5).
    registration_tier(betlang, tier5).

    % Registration Process Steps
    :- public(registration_steps/1).
    :- mode(registration_steps(-list), one).

    registration_steps([
        'Navigate to https://bestpractices.coreinfrastructure.org/en/projects/new',
        'Enter GitHub URL: https://github.com/hyperpolymath/REPO',
        'Click Create Project Entry',
        'Review auto-filled fields',
        'Complete missing fields using registration_field/3 predicates',
        'Verify all MUST criteria are met',
        'Submit for badge',
        'Copy badge URL',
        'Add badge to README.adoc',
        'Commit and push badge update'
    ]).

    % Badge Markdown Format
    :- public(badge_markdown/2).
    :- mode(badge_markdown(+atom, -atom), one).

    badge_markdown(ProjectID, Markdown) :-
        atom_concat('image:https://bestpractices.coreinfrastructure.org/projects/', ProjectID, Part1),
        atom_concat(Part1, '/badge[CII Best Practices,link=https://bestpractices.coreinfrastructure.org/projects/', Part2),
        atom_concat(Part2, ProjectID, Part3),
        atom_concat(Part3, ']', Markdown).

    % Automation Strategy
    :- public(automation_approach/2).
    :- mode(automation_approach(+atom, -list), one).
    :- info(automation_approach/2, [
        comment is 'Recommended automation approach based on capability',
        argnames is ['BotCapability', 'Steps']
    ]).

    automation_approach(full_browser_control, [
        'Use browser automation to fill forms',
        'Navigate to registration page',
        'Fill all fields using registration_field/3 predicates',
        'Submit form',
        'Capture badge URL',
        'Update README with badge',
        'Commit and push'
    ]).

    automation_approach(api_access, [
        'Check if OpenSSF Best Practices has API',
        'If yes: use API to submit registration',
        'If no: fall back to guided_manual approach'
    ]).

    automation_approach(guided_manual, [
        'Generate registration data file for each repo',
        'Create script that opens registration page',
        'Display pre-filled answers for copy-paste',
        'Wait for human to complete form',
        'Automatically update README when badge obtained'
    ]).

    % Expected Outcomes
    :- public(expected_outcome/2).
    :- mode(expected_outcome(+atom, -list), one).

    expected_outcome(successful_registration, [
        'Badge awarded (passing level)',
        'Badge URL obtained',
        'README updated with badge',
        'OpenSSF Scorecard detects badge (CII-Best-Practices check passes)',
        'Overall Scorecard score increases',
        'Project demonstrates commitment to security best practices'
    ]).

    % Repos Ready for Registration
    :- public(repos_ready_for_registration/1).
    :- mode(repos_ready_for_registration(-list), one).

    repos_ready_for_registration([
        bunsenite, proven, hypatia,
        'robot-repo-automaton', gitvisor, scaffoldia,
        'dicti0nary-attack', eclexia, ephapax, 'llm-unify',
        'proven-tui', neurophone, 'protocol-squisher', 'file-soup',
        'rescript-openapi', 'language-interop-compiler', betlang
    ]).

    % Integration with Hypatia Scanning
    :- public(post_registration_action/2).
    :- mode(post_registration_action(+atom, -list), one).

    post_registration_action(Repo, [
        action('Update README', 'Add CII badge to README.adoc'),
        action('Commit changes', 'Commit with message: docs: add OpenSSF Best Practices badge'),
        action('Push to GitHub', 'Push changes to trigger CI'),
        action('Verify Scorecard', 'Check OpenSSF Scorecard for CII-Best-Practices check'),
        action('Track in Hypatia', 'Update Hypatia knowledge base with badge status'),
        action('Monitor renewal', 'CII badges require annual renewal - add to calendar')
    ]).

:- end_object.

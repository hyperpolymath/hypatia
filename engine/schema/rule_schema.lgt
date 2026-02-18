%% SPDX-License-Identifier: PMPL-1.0-or-later
%% cicd-hyper-a Rule Schema
%%
%% This file defines the formal schema for CI/CD rules in the neurosymbolic
%% pipeline. All rules must conform to this schema for proper execution
%% and integration with the gitbot-fleet.

:- object(rule_schema).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2026-01-18,
        comment is 'Formal schema for CI/CD rules and findings'
    ]).

    :- public([
        %% Schema validation
        valid_rule/1,
        valid_finding/1,
        valid_severity/1,
        valid_category/1,
        valid_action/1,
        valid_bot_id/1,

        %% Type checking
        is_rule_type/1,
        is_finding_type/1,
        is_fix_type/1,

        %% Schema introspection
        rule_fields/1,
        finding_fields/1,
        severity_levels/1,
        category_list/1,
        bot_list/1
    ]).

    %% ============================================================
    %% SEVERITY LEVELS - Ordered from most to least severe
    %% ============================================================

    severity_levels([critical, high, medium, low, info, suggestion]).

    valid_severity(critical).
    valid_severity(high).
    valid_severity(medium).
    valid_severity(low).
    valid_severity(info).
    valid_severity(suggestion).

    %% Severity ordering for comparison
    severity_order(critical, 1).
    severity_order(high, 2).
    severity_order(medium, 3).
    severity_order(low, 4).
    severity_order(info, 5).
    severity_order(suggestion, 6).

    severity_blocks_release(critical).
    severity_blocks_release(high).

    %% ============================================================
    %% RULE CATEGORIES
    %% ============================================================

    category_list([
        security,                   % Security vulnerabilities, secrets, permissions
        structure,                  % Repository structure, required files
        workflow,                   % GitHub Actions, CI/CD configuration
        code_quality,               % Code style, best practices
        licensing,                  % License headers, SPDX compliance
        documentation,              % README, CHANGELOG, docs
        accessibility,              % WCAG, A11Y compliance
        seo,                        % Meta tags, Open Graph, SEO
        release,                    % Release readiness, versioning
        waste,                      % CI/CD waste, redundant workflows
        policy,                     % RSR compliance, language policy
        seam_analysis,              % Architectural seam detection (seambot)
        drift_detection,            % Interface drift across boundaries (seambot)
        hidden_channels,            % Undeclared cross-boundary communication (seambot)
        forge_integration,          % Multi-forge consistency checks (seambot)
        completeness_license,       % License completeness (finishbot)
        completeness_placeholder,   % Placeholder detection (finishbot)
        completeness_claims,        % Claim verification (finishbot)
        completeness_release,       % Release readiness (finishbot)
        completeness_scm,           % SCM file completeness (finishbot)
        completeness_testing,       % Test coverage completeness (finishbot)
        completeness_tooling,       % Tooling completeness (finishbot)
        completeness_v1_readiness,  % V1 readiness assessment (finishbot)
        proof_verification,         % Formal proof checking (echidnabot)
        solver_integrity,           % Solver correctness validation (echidnabot)
        trust_bridge,               % Cross-system trust verification (echidnabot)
        axiom_tracking,             % Axiom provenance and soundness (echidnabot)
        crypto_hashing,             % Hash algorithm strength (cipherbot)
        crypto_symmetric,           % Symmetric cipher selection (cipherbot)
        crypto_key_exchange,        % Key exchange protocols (cipherbot)
        crypto_signatures,          % Signature scheme validity (cipherbot)
        crypto_password,            % Password hashing compliance (cipherbot)
        crypto_pq_readiness         % Post-quantum readiness (cipherbot)
    ]).

    valid_category(security).
    valid_category(structure).
    valid_category(workflow).
    valid_category(code_quality).
    valid_category(licensing).
    valid_category(documentation).
    valid_category(accessibility).
    valid_category(seo).
    valid_category(release).
    valid_category(waste).
    valid_category(policy).
    valid_category(seam_analysis).
    valid_category(drift_detection).
    valid_category(hidden_channels).
    valid_category(forge_integration).
    valid_category(completeness_license).
    valid_category(completeness_placeholder).
    valid_category(completeness_claims).
    valid_category(completeness_release).
    valid_category(completeness_scm).
    valid_category(completeness_testing).
    valid_category(completeness_tooling).
    valid_category(completeness_v1_readiness).
    valid_category(proof_verification).
    valid_category(solver_integrity).
    valid_category(trust_bridge).
    valid_category(axiom_tracking).
    valid_category(crypto_hashing).
    valid_category(crypto_symmetric).
    valid_category(crypto_key_exchange).
    valid_category(crypto_signatures).
    valid_category(crypto_password).
    valid_category(crypto_pq_readiness).

    %% ============================================================
    %% BOT IDENTIFIERS (from gitbot-fleet)
    %% ============================================================

    bot_list([
        %% Tier 1 - Verifiers
        rhodibot,       % RSR structural compliance
        echidnabot,     % Mathematical verification
        oikos,          % Ecological/economic standards

        %% Tier 2 - Finishers
        glambot,            % Presentation quality
        seambot,            % Architectural seam analysis, drift detection, forge integration
        finishing_bot,      % Completeness analysis, release readiness
        accessibilitybot,   % WCAG accessibility compliance

        %% Specialist
        cipherbot,      % Cryptographic hygiene, post-quantum readiness

        %% Special
        cicd_hyper_a,   % This engine
        robot_repo_automaton  % Executor
    ]).

    valid_bot_id(rhodibot).
    valid_bot_id(echidnabot).
    valid_bot_id(oikos).
    valid_bot_id(glambot).
    valid_bot_id(seambot).
    valid_bot_id(finishing_bot).
    valid_bot_id(accessibilitybot).
    valid_bot_id(cipherbot).
    valid_bot_id(cicd_hyper_a).
    valid_bot_id(robot_repo_automaton).

    %% Bot tier classification
    bot_tier(rhodibot, verifier).
    bot_tier(echidnabot, verifier).
    bot_tier(oikos, verifier).
    bot_tier(glambot, finisher).
    bot_tier(seambot, finisher).
    bot_tier(finishing_bot, finisher).
    bot_tier(accessibilitybot, finisher).
    bot_tier(cipherbot, specialist).
    bot_tier(cicd_hyper_a, engine).
    bot_tier(robot_repo_automaton, executor).

    %% ============================================================
    %% RULE TYPES
    %% ============================================================

    is_rule_type(preventive).      % Block bad commits before they happen
    is_rule_type(curative).        % Fix existing issues
    is_rule_type(declarative).     % Declare what repos must have
    is_rule_type(detective).       % Detect issues during analysis
    is_rule_type(learning).        % Rules learned from training data

    %% ============================================================
    %% RULE SCHEMA
    %% ============================================================

    %% A rule has these required fields:
    rule_fields([
        id,             % Unique identifier (atom, e.g., 'RSR-001')
        name,           % Human-readable name (string)
        type,           % Rule type (preventive, curative, etc.)
        severity,       % Severity level (critical..suggestion)
        category,       % Category (security, structure, etc.)
        description,    % Full description (string)
        detection,      % How to detect (predicate or spec)
        fix,            % How to fix (predicate or spec)
        source_bot,     % Which bot provides this rule
        auto_fixable,   % Can be auto-fixed (boolean)
        commit_message, % Suggested commit message
        prevention_workflow, % Workflow that prevents this
        metadata        % Additional key-value pairs
    ]).

    %% Validate a rule term
    %% rule(Id, Name, Type, Severity, Category, Description, Detection, Fix, Bot, AutoFixable, CommitMsg, Workflow, Meta)
    valid_rule(rule(Id, Name, Type, Severity, Category, Description, Detection, Fix, Bot, AutoFixable, CommitMsg, Workflow, Meta)) :-
        atom(Id),
        atom(Name),
        is_rule_type(Type),
        valid_severity(Severity),
        valid_category(Category),
        atom(Description),
        (callable(Detection) ; is_detection_spec(Detection)),
        (callable(Fix) ; is_fix_spec(Fix)),
        valid_bot_id(Bot),
        is_boolean(AutoFixable),
        atom(CommitMsg),
        (atom(Workflow) ; Workflow == none),
        is_list(Meta).

    %% ============================================================
    %% FINDING SCHEMA (emitted by bots)
    %% ============================================================

    finding_fields([
        id,             % Unique UUID
        rule_id,        % Which rule triggered this
        source_bot,     % Which bot found it
        severity,       % Severity level
        category,       % Category
        message,        % Human-readable message
        file,           % File path (optional)
        line,           % Line number (optional)
        column,         % Column number (optional)
        element,        % Code element (optional)
        suggestion,     % Suggested fix (optional)
        fixable,        % Can be auto-fixed
        fixed,          % Has been fixed
        created_at,     % Timestamp
        related,        % Related finding IDs
        metadata        % Additional data
    ]).

    %% Validate a finding term
    valid_finding(finding(Id, RuleId, Bot, Severity, Category, Message, File, Line, Col, Element, Suggestion, Fixable, Fixed, CreatedAt, Related, Meta)) :-
        atom(Id),
        atom(RuleId),
        valid_bot_id(Bot),
        valid_severity(Severity),
        valid_category(Category),
        atom(Message),
        (atom(File) ; File == none),
        (integer(Line) ; Line == none),
        (integer(Col) ; Col == none),
        (atom(Element) ; Element == none),
        (atom(Suggestion) ; Suggestion == none),
        is_boolean(Fixable),
        is_boolean(Fixed),
        (number(CreatedAt) ; atom(CreatedAt)),
        is_list(Related),
        is_list(Meta).

    %% ============================================================
    %% FIX ACTION TYPES
    %% ============================================================

    valid_action(delete).       % Delete file or content
    valid_action(modify).       % Modify file content
    valid_action(create).       % Create new file
    valid_action(disable).      % Disable workflow/feature
    valid_action(pin).          % Pin dependency/action
    valid_action(add_header).   % Add SPDX/license header
    valid_action(replace).      % Replace content
    valid_action(inject).       % Inject new content
    valid_action(configure).    % Configure setting
    valid_action(enable).       % Enable feature

    %% ============================================================
    %% DETECTION SPECIFICATIONS
    %% ============================================================

    %% Detection spec structure
    is_detection_spec(file_existence(Files)) :-
        is_list(Files).
    is_detection_spec(content_match(Pattern, Files)) :-
        atom(Pattern),
        is_list(Files).
    is_detection_spec(language_detection(Languages)) :-
        is_list(Languages).
    is_detection_spec(workflow_check(Predicate)) :-
        callable(Predicate).
    is_detection_spec(custom(Predicate)) :-
        callable(Predicate).

    %% ============================================================
    %% FIX SPECIFICATIONS
    %% ============================================================

    %% Fix spec structure
    is_fix_spec(fix_spec(Action, Target, Options)) :-
        valid_action(Action),
        atom(Target),
        is_list(Options).
    is_fix_spec(multi_fix(Fixes)) :-
        is_list(Fixes),
        forall(member(F, Fixes), is_fix_spec(F)).
    is_fix_spec(conditional_fix(Condition, ThenFix, ElseFix)) :-
        callable(Condition),
        is_fix_spec(ThenFix),
        (is_fix_spec(ElseFix) ; ElseFix == none).

    %% ============================================================
    %% HELPER PREDICATES
    %% ============================================================

    is_boolean(true).
    is_boolean(false).

    %% Generate a new finding from a rule match
    generate_finding(Rule, File, Line, Col, Element, Finding) :-
        valid_rule(Rule),
        Rule = rule(RuleId, _, _, Severity, Category, _, _, _, Bot, AutoFixable, _, _, _),
        uuid_v4(FindingId),
        current_timestamp(Now),
        Finding = finding(
            FindingId, RuleId, Bot, Severity, Category,
            'Rule violation detected', File, Line, Col, Element,
            none, AutoFixable, false, Now, [], []
        ).

    %% ============================================================
    %% RULE TEMPLATES
    %% ============================================================

    %% Template for creating new preventive rules
    preventive_rule_template(
        rule(
            'TEMPLATE-001',          % id
            'Template Rule',          % name
            preventive,              % type
            medium,                  % severity
            security,                % category
            'Description here',      % description
            file_existence(['file']), % detection
            fix_spec(create, 'file', []), % fix
            rhodibot,                % source_bot
            true,                    % auto_fixable
            'fix: add required file', % commit_message
            'workflow-linter.yml',   % prevention_workflow
            []                       % metadata
        )
    ).

    %% Template for curative rules
    curative_rule_template(
        rule(
            'CURE-001',
            'Curative Template',
            curative,
            medium,
            code_quality,
            'Fix existing issue',
            custom(detect_issue/2),
            fix_spec(modify, 'target', []),
            robot_repo_automaton,
            true,
            'fix: resolve issue',
            none,
            []
        )
    ).

    %% ============================================================
    %% SCHEMA VERSION
    %% ============================================================

    schema_version('1.0.0').
    schema_date('2026-01-18').

:- end_object.

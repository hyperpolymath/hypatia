%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Learning Engine - Auto-generates new rules from observed patterns
%% Closes the feedback loop: bots report → hypatia learns → new rules added

:- object(learning_engine).

    :- info([
        version is 1.0,
        author is 'Hypatia Learning System',
        date is 2026-01-25,
        comment is 'Autonomous rule learning from bot findings and fix outcomes'
    ]).

    :- public([
        % Pattern observation
        observe_pattern/4,
        get_pattern_frequency/2,

        % Rule generation
        should_generate_rule/1,
        generate_rule_from_pattern/2,
        auto_add_rule/1,

        % Fix outcome learning
        record_fix_success/3,
        record_fix_failure/3,
        get_best_fix_for_pattern/2,

        % Rule proposal
        propose_new_rules/1,
        get_pending_rule_proposals/1,
        approve_rule_proposal/1,

        % Statistics
        get_learning_stats/1
    ]).

    :- private([
        observed_pattern/4,      % pattern(Type, Pattern, Context, Count)
        fix_outcome/4,           % outcome(Pattern, Fix, Success, Failure)
        rule_proposal/3,         % proposal(Pattern, GeneratedRule, Status)
        pattern_threshold/1      % Threshold for auto-generation
    ]).

    :- dynamic([
        observed_pattern/4,
        fix_outcome/4,
        rule_proposal/3
    ]).

    %% Configuration
    pattern_threshold(5).  % Generate rule after seeing pattern 5 times

    %% ============================================================
    %% PATTERN OBSERVATION
    %% ============================================================

    % Observe a new pattern from bot findings
    % observe_pattern(+PatternType, +Pattern, +Context, +BotId)
    observe_pattern(PatternType, Pattern, Context, BotId) :-
        % Check if we've seen this pattern before
        ( observed_pattern(PatternType, Pattern, Context, OldCount) ->
            % Increment count
            retract(observed_pattern(PatternType, Pattern, Context, OldCount)),
            NewCount is OldCount + 1,
            assertz(observed_pattern(PatternType, Pattern, Context, NewCount)),

            % Log learning event
            format('Learning: Pattern ~w observed ~w times~n', [Pattern, NewCount]),

            % Check if we should generate a rule
            pattern_threshold(Threshold),
            ( NewCount >= Threshold ->
                format('  → Threshold reached! Generating rule...~n', []),
                propose_new_rule(PatternType, Pattern, Context)
            ; true
            )
        ;
            % First observation
            assertz(observed_pattern(PatternType, Pattern, Context, 1)),
            format('Learning: New pattern observed: ~w (from ~w)~n', [Pattern, BotId])
        ).

    % Get frequency of a pattern
    get_pattern_frequency(Pattern, Count) :-
        ( observed_pattern(_, Pattern, _, Count) -> true
        ; Count = 0
        ).

    %% ============================================================
    %% RULE GENERATION
    %% ============================================================

    % Check if pattern should trigger rule generation
    should_generate_rule(Pattern) :-
        get_pattern_frequency(Pattern, Count),
        pattern_threshold(Threshold),
        Count >= Threshold,
        \+ rule_exists_for_pattern(Pattern).

    % Check if rule already exists
    rule_exists_for_pattern(Pattern) :-
        % Check in existing rule objects
        ( code_safety_lessons::has_unsafe_panic(_, Pattern) -> true
        ; code_safety_lessons::has_unsafe_crash(_, Pattern) -> true
        ; code_safety_lessons::has_cors_misconfiguration(_, Pattern) -> true
        ; rule_proposal(Pattern, _, approved) -> true
        ; fail
        ).

    % Generate rule from observed pattern
    generate_rule_from_pattern(Pattern, GeneratedRule) :-
        observed_pattern(PatternType, Pattern, Context, Count),
        Count >= 5,

        % Generate rule based on pattern type
        ( PatternType = unsafe_call ->
            format(atom(RuleName), 'has_~w', [Pattern]),
            format(atom(RuleBody), 'read_code_line(Path, LineNum, Line), atom_concat(_, \'~w\', Line)', [Pattern]),
            format(atom(GeneratedRule), ':- public(~w/2).~n~w(Path, Issue) :- file_extension(Path, _), ~w.',
                [RuleName, RuleName, RuleBody])

        ; PatternType = security_pattern ->
            % Security pattern rule
            extract_security_rule(Pattern, Context, GeneratedRule)

        ; PatternType = type_safety ->
            % Type safety rule
            extract_type_safety_rule(Pattern, Context, GeneratedRule)

        ; fail
        ).

    extract_security_rule(Pattern, Context, Rule) :-
        format(atom(Rule),
            'has_security_issue(Path, ~w_pattern(Line)) :-~n    read_code_line(Path, LineNum, Line),~n    atom_concat(_, \'~w\', Line),~n    % Context: ~w~n    LineNum.',
            [Pattern, Pattern, Context]).

    extract_type_safety_rule(Pattern, Context, Rule) :-
        format(atom(Rule),
            'has_type_safety_issue(Path, ~w_violation(Line)) :-~n    read_code_line(Path, LineNum, Line),~n    atom_concat(_, \'~w\', Line),~n    % Context: ~w~n    LineNum.',
            [Pattern, Pattern, Context]).

    %% ============================================================
    %% RULE PROPOSALS (require human approval)
    %% ============================================================

    % Propose a new rule for approval
    propose_new_rule(PatternType, Pattern, Context) :-
        % Check if already proposed
        ( rule_proposal(Pattern, _, _) ->
            true  % Already proposed
        ;
            % Generate rule
            ( generate_rule_from_pattern(Pattern, GeneratedRule) ->
                % Create proposal
                assertz(rule_proposal(Pattern, GeneratedRule, pending)),
                format('~n=== NEW RULE PROPOSAL ===~n', []),
                format('Pattern: ~w~n', [Pattern]),
                format('Type: ~w~n', [PatternType]),
                format('Context: ~w~n', [Context]),
                format('~nGenerated Rule:~n~w~n', [GeneratedRule]),
                format('=========================~n~n', []),

                % Auto-approve if high confidence
                ( should_auto_approve(Pattern, Context) ->
                    auto_add_rule(Pattern)
                ; format('Awaiting manual approval (run: hypatia approve-rule ~w)~n', [Pattern])
                )
            ; format('Failed to generate rule for pattern: ~w~n', [Pattern])
            )
        ).

    % Auto-approve rules with high confidence
    should_auto_approve(Pattern, Context) :-
        get_pattern_frequency(Pattern, Count),
        Count >= 10,  % Seen 10+ times
        has_successful_fix_record(Pattern).

    has_successful_fix_record(Pattern) :-
        fix_outcome(Pattern, _, SuccessCount, FailCount),
        SuccessCount > FailCount,
        SuccessCount >= 3.

    % Auto-add approved rule to hypatia
    auto_add_rule(Pattern) :-
        rule_proposal(Pattern, GeneratedRule, pending),

        % Update proposal status
        retract(rule_proposal(Pattern, GeneratedRule, pending)),
        assertz(rule_proposal(Pattern, GeneratedRule, approved)),

        % Write to learned-rules file
        open('learned-rules.lgt', append, Stream),
        format(Stream, '~n% Auto-generated rule (pattern observed ~w times)~n', [_]),
        format(Stream, '~w~n', [GeneratedRule]),
        close(Stream),

        format('✓ Rule auto-approved and added to learned-rules.lgt~n', []).

    % Get all pending rule proposals
    get_pending_rule_proposals(Proposals) :-
        findall(proposal(Pattern, Rule),
            rule_proposal(Pattern, Rule, pending),
            Proposals).

    % Manual approval of rule
    approve_rule_proposal(Pattern) :-
        auto_add_rule(Pattern).

    %% ============================================================
    %% FIX OUTCOME LEARNING
    %% ============================================================

    % Record successful fix
    record_fix_success(Pattern, Fix, Context) :-
        ( fix_outcome(Pattern, Fix, OldSuccess, FailCount) ->
            retract(fix_outcome(Pattern, Fix, OldSuccess, FailCount)),
            NewSuccess is OldSuccess + 1,
            assertz(fix_outcome(Pattern, Fix, NewSuccess, FailCount)),
            format('Learning: Fix \'~w\' for pattern \'~w\' succeeded (~w successes)~n',
                [Fix, Pattern, NewSuccess])
        ;
            assertz(fix_outcome(Pattern, Fix, 1, 0)),
            format('Learning: First successful fix for pattern \'~w\'~n', [Pattern])
        ).

    % Record failed fix
    record_fix_failure(Pattern, Fix, Context) :-
        ( fix_outcome(Pattern, Fix, SuccessCount, OldFail) ->
            retract(fix_outcome(Pattern, Fix, SuccessCount, OldFail)),
            NewFail is OldFail + 1,
            assertz(fix_outcome(Pattern, Fix, SuccessCount, NewFail)),
            format('Learning: Fix \'~w\' for pattern \'~w\' failed (~w failures)~n',
                [Fix, Pattern, NewFail])
        ;
            assertz(fix_outcome(Pattern, Fix, 0, 1)),
            format('Learning: First fix attempt for pattern \'~w\' failed~n', [Pattern])
        ).

    % Get best fix based on success rate
    get_best_fix_for_pattern(Pattern, BestFix) :-
        findall(score(SuccessRate, Fix),
            (fix_outcome(Pattern, Fix, Success, Fail),
             Total is Success + Fail,
             Total > 0,
             SuccessRate is Success / Total),
            Scores),
        sort(0, @>=, Scores, [score(_, BestFix)|_]).

    %% ============================================================
    %% STATISTICS & REPORTING
    %% ============================================================

    get_learning_stats(Stats) :-
        findall(_, observed_pattern(_, _, _, _), Patterns),
        length(Patterns, TotalPatterns),

        findall(_, rule_proposal(_, _, pending), PendingProposals),
        length(PendingProposals, PendingCount),

        findall(_, rule_proposal(_, _, approved), ApprovedProposals),
        length(ApprovedProposals, ApprovedCount),

        findall(Count, observed_pattern(_, _, _, Count), Counts),
        ( Counts = [] -> AvgObservations = 0
        ; sumlist(Counts, Sum),
          length(Counts, Len),
          AvgObservations is Sum / Len
        ),

        Stats = stats(
            total_patterns(TotalPatterns),
            pending_proposals(PendingCount),
            approved_rules(ApprovedCount),
            avg_observations(AvgObservations)
        ).

:- end_object.

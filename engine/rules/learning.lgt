%% SPDX-License-Identifier: PLMP-1.0-or-later
%% Learning Module - Continuous learning from audit data and feedback
%% Enhanced with pattern storage, rule distillation, feedback loops, and cross-repo learning

:- object(learning).

    :- info([
        version is 2:'0':'0',
        author is 'cicd-hyper-a',
        date is 2026-01-18,
        comment is 'Continuous learning from audit data, user feedback, fix outcomes, and cross-repo patterns'
    ]).

    :- public([
        %% Core learning operations
        learn_from_fix/3,
        learn_from_alert/4,
        learn_from_feedback/3,
        learn_pattern/4,

        %% Knowledge retrieval
        get_learned_rules/1,
        get_confidence/2,
        get_success_rate/2,
        recommend_fix/2,

        %% Persistence
        save_knowledge/1,
        load_knowledge/1,
        export_as_rules/1,

        %% Learning statistics
        get_learning_stats/1,
        prune_low_confidence/1,

        %% ============================================================
        %% NEW: Pattern Storage and Retrieval (v2.0.0)
        %% ============================================================
        store_pattern/4,              % store_pattern(Pattern, Category, Confidence, Occurrences)
        get_patterns/2,               % get_patterns(Category, Patterns)
        update_pattern_confidence/3,  % update_pattern_confidence(Pattern, Adjustment, Reason)

        %% ============================================================
        %% NEW: Rule Distillation (v2.0.0)
        %% ============================================================
        pattern_to_rule_candidate/2,  % pattern_to_rule_candidate(Pattern, Candidate)
        validate_rule_candidate/2,    % validate_rule_candidate(Candidate, ValidationResult)
        promote_to_rule/2,            % promote_to_rule(Candidate, RuleId)

        %% ============================================================
        %% NEW: Feedback Loop (v2.0.0)
        %% ============================================================
        record_fix_success/2,         % record_fix_success(IssueType, Context)
        record_fix_failure/2,         % record_fix_failure(IssueType, Context)
        record_false_positive/2,      % record_false_positive(IssueType, Context)
        update_rule_confidence/2,     % update_rule_confidence(RuleId, Adjustment)

        %% ============================================================
        %% NEW: Cross-Repo Learning (v2.0.0)
        %% ============================================================
        find_common_patterns/2,       % find_common_patterns(MinRepoCount, Patterns)
        suggest_organization_rules/1, % suggest_organization_rules(Suggestions)
        pattern_similarity/3,         % pattern_similarity(Pattern1, Pattern2, Score)

        %% ============================================================
        %% NEW: Training Data Management (v2.0.0)
        %% ============================================================
        import_training_data/1,       % import_training_data(Source)
        export_training_data/1,       % export_training_data(Destination)
        clean_training_data/0         % clean_training_data
    ]).

    :- private([
        learned_fix/5,           % learned_fix(IssueType, Fix, Confidence, SuccessCount, FailCount)
        learned_pattern/4,       % learned_pattern(Pattern, Category, Severity, Count)
        feedback_record/4,       % feedback_record(IssueType, Fix, Outcome, Timestamp)
        knowledge_version/1,
        %% New private predicates (v2.0.0)
        stored_pattern/5,        % stored_pattern(Pattern, Category, Confidence, Occurrences, Repos)
        rule_candidate/6,        % rule_candidate(Id, Pattern, Confidence, ValidationStatus, CreatedAt, PromotedAt)
        promoted_rule/4,         % promoted_rule(RuleId, Pattern, Confidence, CreatedAt)
        false_positive_record/3, % false_positive_record(IssueType, Context, Timestamp)
        repo_pattern/3,          % repo_pattern(Repo, Pattern, Count)
        training_example/5       % training_example(Source, IssueType, Fix, Outcome, Timestamp)
    ]).

    :- dynamic([
        learned_fix/5,
        learned_pattern/4,
        feedback_record/4,
        knowledge_version/1,
        stored_pattern/5,
        rule_candidate/6,
        promoted_rule/4,
        false_positive_record/3,
        repo_pattern/3,
        training_example/5
    ]).

    %% ============================================================
    %% CORE LEARNING OPERATIONS
    %% ============================================================

    %% Learn from a successful or failed fix attempt
    %% Outcome: success | failure | partial
    learn_from_fix(IssueType, Fix, Outcome) :-
        get_time(Timestamp),
        assertz(feedback_record(IssueType, Fix, Outcome, Timestamp)),
        update_fix_confidence(IssueType, Fix, Outcome).

    update_fix_confidence(IssueType, Fix, success) :-
        ( learned_fix(IssueType, Fix, Conf, Succ, Fail) ->
            retract(learned_fix(IssueType, Fix, Conf, Succ, Fail)),
            Succ1 is Succ + 1,
            Total is Succ1 + Fail,
            NewConf is Succ1 / Total,
            assertz(learned_fix(IssueType, Fix, NewConf, Succ1, Fail))
        ; assertz(learned_fix(IssueType, Fix, 1.0, 1, 0))
        ).

    update_fix_confidence(IssueType, Fix, failure) :-
        ( learned_fix(IssueType, Fix, Conf, Succ, Fail) ->
            retract(learned_fix(IssueType, Fix, Conf, Succ, Fail)),
            Fail1 is Fail + 1,
            Total is Succ + Fail1,
            NewConf is Succ / Total,
            assertz(learned_fix(IssueType, Fix, NewConf, Succ, Fail1))
        ; assertz(learned_fix(IssueType, Fix, 0.0, 0, 1))
        ).

    update_fix_confidence(IssueType, Fix, partial) :-
        ( learned_fix(IssueType, Fix, Conf, Succ, Fail) ->
            retract(learned_fix(IssueType, Fix, Conf, Succ, Fail)),
            % Partial success counts as 0.5
            Succ1 is Succ + 0.5,
            Fail1 is Fail + 0.5,
            Total is Succ1 + Fail1,
            NewConf is Succ1 / Total,
            assertz(learned_fix(IssueType, Fix, NewConf, Succ1, Fail1))
        ; assertz(learned_fix(IssueType, Fix, 0.5, 0.5, 0.5))
        ).

    %% Learn from a new alert type
    learn_from_alert(AlertId, Category, Severity, Description) :-
        ( learned_pattern(AlertId, _, _, Count) ->
            retract(learned_pattern(AlertId, _, _, Count)),
            Count1 is Count + 1,
            assertz(learned_pattern(AlertId, Category, Severity, Count1))
        ; assertz(learned_pattern(AlertId, Category, Severity, 1))
        ),
        % Try to auto-derive fix from cicd_rules
        ( cicd_rules::suggest_fix(AlertId, _) ->
            true
        ; derive_fix_suggestion(AlertId, Category, Description)
        ).

    derive_fix_suggestion(_, _, _) :- true.  % Placeholder for ML-based derivation

    %% Learn from explicit user feedback
    learn_from_feedback(IssueType, Feedback, Context) :-
        get_time(Timestamp),
        assertz(feedback_record(IssueType, Feedback, user_feedback, Timestamp)),
        process_feedback(IssueType, Feedback, Context).

    process_feedback(IssueType, fix_worked, _) :-
        ( learned_fix(IssueType, _, Conf, Succ, Fail), Conf >= 0.5 ->
            true  % Already tracked
        ; true    % No action needed
        ).
    process_feedback(IssueType, fix_failed, Context) :-
        % Record that the suggested fix didn't work in this context
        assertz(feedback_record(IssueType, context(Context), failure, _)).
    process_feedback(_, _, _) :- true.

    %% Learn a new pattern (from external analysis)
    learn_pattern(Pattern, Category, Severity, Fix) :-
        ( learned_pattern(Pattern, _, _, _) ->
            true  % Already known
        ; assertz(learned_pattern(Pattern, Category, Severity, 1)),
          ( nonvar(Fix) ->
              assertz(learned_fix(Pattern, Fix, 0.8, 4, 1))  % Initial confidence from training
          ; true
          )
        ).

    %% ============================================================
    %% KNOWLEDGE RETRIEVAL
    %% ============================================================

    get_learned_rules(Rules) :-
        findall(
            rule(IssueType, Fix, Confidence),
            (learned_fix(IssueType, Fix, Confidence, _, _), Confidence >= 0.6),
            Rules
        ).

    get_confidence(IssueType, Confidence) :-
        learned_fix(IssueType, _, Confidence, _, _).

    get_success_rate(IssueType, Rate) :-
        findall(
            Succ-Fail,
            learned_fix(IssueType, _, _, Succ, Fail),
            Pairs
        ),
        sum_pairs(Pairs, TotalSucc, TotalFail),
        Total is TotalSucc + TotalFail,
        ( Total > 0 ->
            Rate is TotalSucc / Total
        ; Rate is 0.0
        ).

    sum_pairs([], 0, 0).
    sum_pairs([S-F|Rest], TotalS, TotalF) :-
        sum_pairs(Rest, RestS, RestF),
        TotalS is S + RestS,
        TotalF is F + RestF.

    %% Recommend best fix based on learned confidence
    recommend_fix(IssueType, Fix) :-
        findall(
            Conf-Fix,
            learned_fix(IssueType, Fix, Conf, _, _),
            Pairs
        ),
        sort(0, @>=, Pairs, Sorted),
        ( Sorted = [_-BestFix|_] ->
            Fix = BestFix
        ; cicd_rules::suggest_fix(IssueType, Fix)  % Fallback to static rules
        ).

    %% ============================================================
    %% PERSISTENCE
    %% ============================================================

    save_knowledge(FilePath) :-
        open(FilePath, write, Stream),
        write(Stream, '%% SPDX-License-Identifier: PLMP-1.0-or-later\n'),
        write(Stream, '%% cicd-hyper-a Learned Knowledge Base (v2.0.0)\n'),
        get_time(Now),
        format(Stream, '%% Generated: ~w~n~n', [Now]),

        % Save learned fixes
        write(Stream, '%% Learned Fixes (IssueType, Fix, Confidence, SuccessCount, FailCount)\n'),
        forall(
            learned_fix(Issue, Fix, Conf, Succ, Fail),
            format(Stream, 'learned_fix(~q, ~q, ~w, ~w, ~w).~n', [Issue, Fix, Conf, Succ, Fail])
        ),

        % Save learned patterns
        write(Stream, '\n%% Learned Patterns (Pattern, Category, Severity, Count)\n'),
        forall(
            learned_pattern(Pattern, Cat, Sev, Count),
            format(Stream, 'learned_pattern(~q, ~q, ~q, ~w).~n', [Pattern, Cat, Sev, Count])
        ),

        % Save stored patterns (v2.0.0)
        write(Stream, '\n%% Stored Patterns (Pattern, Category, Confidence, Occurrences, Repos)\n'),
        forall(
            stored_pattern(Pattern, Cat, Conf, Occ, Repos),
            format(Stream, 'stored_pattern(~q, ~q, ~w, ~w, ~q).~n', [Pattern, Cat, Conf, Occ, Repos])
        ),

        % Save promoted rules (v2.0.0)
        write(Stream, '\n%% Promoted Rules (RuleId, Pattern, Confidence, CreatedAt)\n'),
        forall(
            promoted_rule(RuleId, Pattern, Conf, Created),
            format(Stream, 'promoted_rule(~q, ~q, ~w, ~w).~n', [RuleId, Pattern, Conf, Created])
        ),

        % Save repo patterns (v2.0.0)
        write(Stream, '\n%% Repo Patterns (Repo, Pattern, Count)\n'),
        forall(
            repo_pattern(Repo, Pattern, Count),
            format(Stream, 'repo_pattern(~q, ~q, ~w).~n', [Repo, Pattern, Count])
        ),

        close(Stream).

    load_knowledge(FilePath) :-
        ( exists_file(FilePath) ->
            open(FilePath, read, Stream),
            read_knowledge(Stream),
            close(Stream)
        ; true  % File doesn't exist yet, start fresh
        ).

    read_knowledge(Stream) :-
        read(Stream, Term),
        ( Term == end_of_file ->
            true
        ; assert_knowledge(Term),
          read_knowledge(Stream)
        ).

    assert_knowledge(learned_fix(A, B, C, D, E)) :- !,
        assertz(learned_fix(A, B, C, D, E)).
    assert_knowledge(learned_pattern(A, B, C, D)) :- !,
        assertz(learned_pattern(A, B, C, D)).
    assert_knowledge(stored_pattern(A, B, C, D, E)) :- !,
        assertz(stored_pattern(A, B, C, D, E)).
    assert_knowledge(promoted_rule(A, B, C, D)) :- !,
        assertz(promoted_rule(A, B, C, D)).
    assert_knowledge(repo_pattern(A, B, C)) :- !,
        assertz(repo_pattern(A, B, C)).
    assert_knowledge(_).

    %% Export learned knowledge as static rules for cicd_rules
    export_as_rules(FilePath) :-
        open(FilePath, write, Stream),
        write(Stream, '%% SPDX-License-Identifier: PLMP-1.0-or-later\n'),
        write(Stream, '%% Auto-generated rules from learning module (v2.0.0)\n'),
        write(Stream, '%% Import these into cicd_rules.lgt\n\n'),

        forall(
            (learned_fix(Issue, Fix, Conf, _, _), Conf >= 0.75),
            format(Stream, 'suggest_fix(~q, ~q).  %% confidence: ~2f~n', [Issue, Fix, Conf])
        ),

        forall(
            (learned_pattern(Pattern, _, Severity, Count), Count >= 5),
            format(Stream, 'classify_severity(~q, ~q).  %% seen ~w times~n', [Pattern, Severity, Count])
        ),

        % Export promoted rules (v2.0.0)
        write(Stream, '\n%% Promoted rules from pattern learning\n'),
        forall(
            promoted_rule(RuleId, Pattern, Conf, _),
            format(Stream, 'promoted_rule(~q, ~q).  %% confidence: ~2f~n', [RuleId, Pattern, Conf])
        ),

        close(Stream).

    %% ============================================================
    %% LEARNING STATISTICS
    %% ============================================================

    get_learning_stats(Stats) :-
        findall(_, learned_fix(_, _, _, _, _), Fixes),
        length(Fixes, FixCount),
        findall(_, learned_pattern(_, _, _, _), Patterns),
        length(Patterns, PatternCount),
        findall(_, feedback_record(_, _, _, _), Feedback),
        length(Feedback, FeedbackCount),
        findall(
            Conf,
            (learned_fix(_, _, Conf, _, _), Conf >= 0.8),
            HighConf
        ),
        length(HighConf, HighConfCount),
        % v2.0.0 additions
        findall(_, stored_pattern(_, _, _, _, _), StoredPatterns),
        length(StoredPatterns, StoredPatternCount),
        findall(_, promoted_rule(_, _, _, _), PromotedRules),
        length(PromotedRules, PromotedRuleCount),
        findall(_, false_positive_record(_, _, _), FalsePositives),
        length(FalsePositives, FalsePositiveCount),
        Stats = stats{
            total_fixes: FixCount,
            total_patterns: PatternCount,
            total_feedback: FeedbackCount,
            high_confidence_rules: HighConfCount,
            stored_patterns: StoredPatternCount,
            promoted_rules: PromotedRuleCount,
            false_positives: FalsePositiveCount
        }.

    %% Remove rules with low confidence (housekeeping)
    prune_low_confidence(Threshold) :-
        findall(
            learned_fix(A, B, C, D, E),
            (learned_fix(A, B, C, D, E), C < Threshold, D + E >= 5),
            ToRemove
        ),
        forall(member(Fact, ToRemove), retract(Fact)).

    %% ============================================================
    %% PATTERN STORAGE AND RETRIEVAL (v2.0.0)
    %% ============================================================

    %% Store a pattern with category, confidence, and occurrence count
    %% store_pattern(Pattern, Category, Confidence, Occurrences)
    store_pattern(Pattern, Category, Confidence, Occurrences) :-
        ( stored_pattern(Pattern, _, OldConf, OldOcc, Repos) ->
            retract(stored_pattern(Pattern, _, OldConf, OldOcc, Repos)),
            % Merge: weighted average of confidence, sum of occurrences
            TotalOcc is OldOcc + Occurrences,
            NewConf is (OldConf * OldOcc + Confidence * Occurrences) / TotalOcc,
            assertz(stored_pattern(Pattern, Category, NewConf, TotalOcc, Repos))
        ; assertz(stored_pattern(Pattern, Category, Confidence, Occurrences, []))
        ).

    %% Get all patterns in a category
    %% get_patterns(Category, Patterns)
    get_patterns(Category, Patterns) :-
        findall(
            pattern(Pattern, Confidence, Occurrences, Repos),
            stored_pattern(Pattern, Category, Confidence, Occurrences, Repos),
            Patterns
        ).

    %% Update pattern confidence based on feedback
    %% update_pattern_confidence(Pattern, Adjustment, Reason)
    %% Adjustment: positive increases confidence, negative decreases
    update_pattern_confidence(Pattern, Adjustment, Reason) :-
        get_time(Timestamp),
        ( stored_pattern(Pattern, Cat, OldConf, Occ, Repos) ->
            retract(stored_pattern(Pattern, Cat, OldConf, Occ, Repos)),
            % Clamp confidence between 0.0 and 1.0
            TempConf is OldConf + Adjustment,
            ( TempConf > 1.0 -> NewConf = 1.0
            ; TempConf < 0.0 -> NewConf = 0.0
            ; NewConf = TempConf
            ),
            assertz(stored_pattern(Pattern, Cat, NewConf, Occ, Repos)),
            assertz(feedback_record(Pattern, confidence_adjustment(Adjustment, Reason), updated, Timestamp))
        ; true  % Pattern not found, no action
        ).

    %% ============================================================
    %% RULE DISTILLATION (v2.0.0)
    %% ============================================================

    %% Convert a high-confidence pattern to a rule candidate
    %% pattern_to_rule_candidate(Pattern, Candidate)
    pattern_to_rule_candidate(Pattern, Candidate) :-
        stored_pattern(Pattern, Category, Confidence, Occurrences, Repos),
        % Minimum thresholds for candidacy
        Confidence >= 0.75,
        Occurrences >= 5,
        get_time(Now),
        generate_candidate_id(Pattern, CandidateId),
        Candidate = candidate(
            CandidateId,
            Pattern,
            Category,
            Confidence,
            Occurrences,
            Repos,
            pending,
            Now
        ).

    generate_candidate_id(Pattern, CandidateId) :-
        term_hash(Pattern, Hash),
        get_time(Now),
        NowInt is round(Now),
        atom_concat('cand_', Hash, Temp),
        atom_concat(Temp, '_', Temp2),
        atom_concat(Temp2, NowInt, CandidateId).

    %% Validate that a candidate meets criteria for promotion
    %% validate_rule_candidate(Candidate, ValidationResult)
    validate_rule_candidate(Candidate, ValidationResult) :-
        Candidate = candidate(_, Pattern, Category, Confidence, Occurrences, Repos, _, _),
        findall(Issue, validate_criterion(Pattern, Category, Confidence, Occurrences, Repos, Issue), Issues),
        ( Issues == [] ->
            ValidationResult = valid
        ; ValidationResult = invalid(Issues)
        ).

    validate_criterion(_, _, Confidence, _, _, low_confidence) :-
        Confidence < 0.75.
    validate_criterion(_, _, _, Occurrences, _, insufficient_data) :-
        Occurrences < 5.
    validate_criterion(Pattern, _, _, _, _, duplicate_rule) :-
        promoted_rule(_, Pattern, _, _).
    validate_criterion(_, _, _, _, Repos, single_repo_only) :-
        length(Repos, Len),
        Len == 1.
    validate_criterion(Pattern, _, _, _, _, high_false_positive_rate) :-
        findall(_, false_positive_record(Pattern, _, _), FPs),
        findall(_, feedback_record(Pattern, _, success, _), Successes),
        length(FPs, FPCount),
        length(Successes, SuccCount),
        Total is FPCount + SuccCount,
        Total > 0,
        FPRate is FPCount / Total,
        FPRate > 0.3.

    %% Promote a validated candidate to an actual rule
    %% promote_to_rule(Candidate, RuleId)
    promote_to_rule(Candidate, RuleId) :-
        Candidate = candidate(CandidateId, Pattern, Category, Confidence, _, _, _, CreatedAt),
        validate_rule_candidate(Candidate, valid),
        get_time(Now),
        % Generate rule ID
        atom_concat('rule_', Category, Temp),
        atom_concat(Temp, '_', Temp2),
        NowInt is round(Now),
        atom_concat(Temp2, NowInt, RuleId),
        % Store promoted rule
        assertz(promoted_rule(RuleId, Pattern, Confidence, Now)),
        % Update candidate status
        ( rule_candidate(CandidateId, _, _, _, _, _) ->
            retract(rule_candidate(CandidateId, _, _, _, _, _)),
            assertz(rule_candidate(CandidateId, Pattern, Confidence, promoted, CreatedAt, Now))
        ; assertz(rule_candidate(CandidateId, Pattern, Confidence, promoted, CreatedAt, Now))
        ).

    %% ============================================================
    %% FEEDBACK LOOP (v2.0.0)
    %% ============================================================

    %% Record when a fix succeeds
    %% record_fix_success(IssueType, Context)
    record_fix_success(IssueType, Context) :-
        get_time(Timestamp),
        assertz(feedback_record(IssueType, Context, success, Timestamp)),
        % Update pattern confidence
        ( stored_pattern(IssueType, _, _, _, _) ->
            update_pattern_confidence(IssueType, 0.05, fix_success)
        ; true
        ),
        % Update learned fix confidence
        learn_from_fix(IssueType, Context, success).

    %% Record when a fix fails or is reverted
    %% record_fix_failure(IssueType, Context)
    record_fix_failure(IssueType, Context) :-
        get_time(Timestamp),
        assertz(feedback_record(IssueType, Context, failure, Timestamp)),
        % Decrease pattern confidence
        ( stored_pattern(IssueType, _, _, _, _) ->
            update_pattern_confidence(IssueType, -0.1, fix_failure)
        ; true
        ),
        % Update learned fix confidence
        learn_from_fix(IssueType, Context, failure).

    %% Record when a finding is dismissed as a false positive
    %% record_false_positive(IssueType, Context)
    record_false_positive(IssueType, Context) :-
        get_time(Timestamp),
        assertz(false_positive_record(IssueType, Context, Timestamp)),
        % Significantly decrease pattern confidence
        ( stored_pattern(IssueType, _, _, _, _) ->
            update_pattern_confidence(IssueType, -0.15, false_positive)
        ; true
        ).

    %% Update rule confidence based on feedback
    %% update_rule_confidence(RuleId, Adjustment)
    update_rule_confidence(RuleId, Adjustment) :-
        ( promoted_rule(RuleId, Pattern, OldConf, Created) ->
            retract(promoted_rule(RuleId, Pattern, OldConf, Created)),
            TempConf is OldConf + Adjustment,
            ( TempConf > 1.0 -> NewConf = 1.0
            ; TempConf < 0.0 -> NewConf = 0.0
            ; NewConf = TempConf
            ),
            assertz(promoted_rule(RuleId, Pattern, NewConf, Created))
        ; true  % Rule not found
        ).

    %% ============================================================
    %% CROSS-REPO LEARNING (v2.0.0)
    %% ============================================================

    %% Find patterns that appear across multiple repos
    %% find_common_patterns(MinRepoCount, Patterns)
    find_common_patterns(MinRepoCount, Patterns) :-
        % Aggregate patterns by their occurrence across repos
        findall(Pattern-Repo, repo_pattern(Repo, Pattern, _), AllPairs),
        group_by_pattern(AllPairs, GroupedPatterns),
        findall(
            common_pattern(Pattern, RepoCount, TotalOccurrences),
            (
                member(Pattern-Repos, GroupedPatterns),
                length(Repos, RepoCount),
                RepoCount >= MinRepoCount,
                sum_pattern_occurrences(Pattern, TotalOccurrences)
            ),
            Patterns
        ).

    group_by_pattern([], []).
    group_by_pattern([Pattern-Repo|Rest], Grouped) :-
        group_by_pattern(Rest, RestGrouped),
        add_to_group(Pattern, Repo, RestGrouped, Grouped).

    add_to_group(Pattern, Repo, [], [Pattern-[Repo]]).
    add_to_group(Pattern, Repo, [Pattern-Repos|Rest], [Pattern-[Repo|Repos]|Rest]) :- !.
    add_to_group(Pattern, Repo, [Other|Rest], [Other|NewRest]) :-
        add_to_group(Pattern, Repo, Rest, NewRest).

    sum_pattern_occurrences(Pattern, Total) :-
        findall(Count, repo_pattern(_, Pattern, Count), Counts),
        sum_list(Counts, Total).

    sum_list([], 0).
    sum_list([H|T], Sum) :-
        sum_list(T, Rest),
        Sum is H + Rest.

    %% Suggest organization-wide rules based on common patterns
    %% suggest_organization_rules(Suggestions)
    suggest_organization_rules(Suggestions) :-
        find_common_patterns(3, CommonPatterns),  % At least 3 repos
        findall(
            suggestion(Pattern, RepoCount, Confidence, Recommendation),
            (
                member(common_pattern(Pattern, RepoCount, TotalOcc), CommonPatterns),
                % Check if pattern has good success rate
                get_pattern_success_rate(Pattern, SuccessRate),
                SuccessRate >= 0.7,
                % Calculate confidence based on data
                Confidence is min(1.0, (SuccessRate * RepoCount * TotalOcc) / 100),
                generate_recommendation(Pattern, RepoCount, Recommendation)
            ),
            Suggestions
        ).

    get_pattern_success_rate(Pattern, Rate) :-
        findall(_, feedback_record(Pattern, _, success, _), Successes),
        findall(_, feedback_record(Pattern, _, failure, _), Failures),
        length(Successes, SuccCount),
        length(Failures, FailCount),
        Total is SuccCount + FailCount,
        ( Total > 0 ->
            Rate is SuccCount / Total
        ; Rate is 0.5  % Default to neutral if no data
        ).

    generate_recommendation(Pattern, RepoCount, Recommendation) :-
        format(atom(Recommendation),
            'Pattern "~w" detected in ~w repos - consider adding as organization-wide rule',
            [Pattern, RepoCount]).

    %% Calculate similarity between two patterns
    %% pattern_similarity(Pattern1, Pattern2, Score)
    pattern_similarity(Pattern1, Pattern2, Score) :-
        pattern_matching::structural_similarity(Pattern1, Pattern2, StructScore),
        pattern_matching::category_similarity(Pattern1, Pattern2, CatScore),
        pattern_matching::context_similarity(Pattern1, Pattern2, CtxScore),
        % Weighted combination
        Score is (StructScore * 0.5) + (CatScore * 0.3) + (CtxScore * 0.2).

    %% ============================================================
    %% TRAINING DATA MANAGEMENT (v2.0.0)
    %% ============================================================

    %% Import training data from an external source
    %% import_training_data(Source)
    %% Source formats: file(Path) | json(Data) | catalog(Path)
    import_training_data(file(Path)) :-
        exists_file(Path),
        open(Path, read, Stream),
        read_training_terms(Stream),
        close(Stream).

    import_training_data(catalog(Path)) :-
        % Import from ERROR-CATALOG.scm format
        exists_file(Path),
        open(Path, read, Stream),
        read_catalog_entries(Stream),
        close(Stream).

    import_training_data(json(JsonPath)) :-
        exists_file(JsonPath),
        % Read JSON and convert to training examples
        open(JsonPath, read, Stream),
        read_json_training(Stream),
        close(Stream).

    read_training_terms(Stream) :-
        read(Stream, Term),
        ( Term == end_of_file ->
            true
        ; process_training_term(Term),
          read_training_terms(Stream)
        ).

    process_training_term(training(IssueType, Fix, Outcome)) :-
        get_time(Now),
        assertz(training_example(imported, IssueType, Fix, Outcome, Now)).
    process_training_term(example(IssueType, Fix, success)) :-
        get_time(Now),
        assertz(training_example(imported, IssueType, Fix, success, Now)).
    process_training_term(_).

    read_catalog_entries(Stream) :-
        read(Stream, Term),
        ( Term == end_of_file ->
            true
        ; process_catalog_entry(Term),
          read_catalog_entries(Stream)
        ).

    process_catalog_entry(error(Id, Category, Severity, AutoFix, _Desc, Fix)) :-
        get_time(Now),
        assertz(training_example(catalog, Id, Fix, inferred, Now)),
        ( AutoFix == true ->
            store_pattern(Id, Category, 0.9, 1)
        ; store_pattern(Id, Category, 0.7, 1)
        ),
        learn_pattern(Id, Category, Severity, Fix).
    process_catalog_entry(_).

    read_json_training(_Stream) :-
        % Placeholder for JSON parsing
        % In practice, would use a JSON library
        true.

    %% Export training data for ML training
    %% export_training_data(Destination)
    export_training_data(Destination) :-
        open(Destination, write, Stream),
        write(Stream, '%% SPDX-License-Identifier: PLMP-1.0-or-later\n'),
        write(Stream, '%% cicd-hyper-a Training Data Export\n'),
        get_time(Now),
        format(Stream, '%% Exported: ~w~n~n', [Now]),

        % Export training examples
        write(Stream, '%% Training Examples (Source, IssueType, Fix, Outcome, Timestamp)\n'),
        forall(
            training_example(Source, Issue, Fix, Outcome, TS),
            format(Stream, 'training(~q, ~q, ~q, ~q, ~w).~n', [Source, Issue, Fix, Outcome, TS])
        ),

        % Export feedback records as training data
        write(Stream, '\n%% Feedback Records (IssueType, Fix, Outcome, Timestamp)\n'),
        forall(
            feedback_record(Issue, Fix, Outcome, TS),
            format(Stream, 'feedback(~q, ~q, ~q, ~w).~n', [Issue, Fix, Outcome, TS])
        ),

        % Export false positives (important for ML training)
        write(Stream, '\n%% False Positives (IssueType, Context, Timestamp)\n'),
        forall(
            false_positive_record(Issue, Ctx, TS),
            format(Stream, 'false_positive(~q, ~q, ~w).~n', [Issue, Ctx, TS])
        ),

        % Export pattern statistics
        write(Stream, '\n%% Pattern Statistics (Pattern, Category, Confidence, Occurrences)\n'),
        forall(
            stored_pattern(Pattern, Cat, Conf, Occ, _),
            format(Stream, 'pattern_stat(~q, ~q, ~w, ~w).~n', [Pattern, Cat, Conf, Occ])
        ),

        close(Stream).

    %% Clean low-quality training data
    %% clean_training_data
    clean_training_data :-
        % Remove patterns with very low confidence
        findall(
            stored_pattern(P, C, Conf, O, R),
            (stored_pattern(P, C, Conf, O, R), Conf < 0.2),
            LowConfPatterns
        ),
        forall(member(Fact, LowConfPatterns), retract(Fact)),

        % Remove patterns with high false positive rates
        findall(
            Pattern,
            (
                stored_pattern(Pattern, _, _, Occ, _),
                Occ > 0,
                findall(_, false_positive_record(Pattern, _, _), FPs),
                length(FPs, FPCount),
                FPRate is FPCount / Occ,
                FPRate > 0.5
            ),
            HighFPPatterns
        ),
        forall(
            member(P, HighFPPatterns),
            ( stored_pattern(P, _, _, _, _) ->
                retract(stored_pattern(P, _, _, _, _))
            ; true
            )
        ),

        % Remove stale training examples (older than 1 year)
        get_time(Now),
        OneYearAgo is Now - 31536000,  % 365 * 24 * 60 * 60
        findall(
            training_example(S, I, F, O, T),
            (training_example(S, I, F, O, T), T < OneYearAgo),
            StaleExamples
        ),
        forall(member(Fact, StaleExamples), retract(Fact)),

        % Report cleanup stats
        length(LowConfPatterns, LowConfCount),
        length(HighFPPatterns, HighFPCount),
        length(StaleExamples, StaleCount),
        format('Cleaned: ~w low-conf patterns, ~w high-FP patterns, ~w stale examples~n',
            [LowConfCount, HighFPCount, StaleCount]).

    %% ============================================================
    %% HELPER PREDICATES
    %% ============================================================

    %% Helper for timestamps (fallback if get_time not available)
    :- if(\+ current_predicate(get_time/1)).
    get_time(0).
    :- endif.

    exists_file(Path) :-
        catch(open(Path, read, S), _, fail),
        close(S).

    %% term_hash fallback if not available
    :- if(\+ current_predicate(term_hash/2)).
    term_hash(Term, Hash) :-
        term_to_atom(Term, Atom),
        atom_codes(Atom, Codes),
        simple_hash(Codes, 0, Hash).

    simple_hash([], Acc, Acc).
    simple_hash([C|Cs], Acc, Hash) :-
        Acc1 is (Acc * 31 + C) mod 1000000007,
        simple_hash(Cs, Acc1, Hash).
    :- endif.

:- end_object.

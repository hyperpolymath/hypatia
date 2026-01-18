%% SPDX-License-Identifier: AGPL-3.0-or-later
%% Pattern Matching Module - Pattern detection utilities for the learning pipeline
%% Provides structural, categorical, and contextual similarity analysis

:- object(pattern_matching).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2026-01-18,
        comment is 'Pattern detection utilities for similarity analysis, matching, and classification'
    ]).

    :- public([
        %% Similarity calculations
        structural_similarity/3,     % structural_similarity(Pattern1, Pattern2, Score)
        category_similarity/3,       % category_similarity(Pattern1, Pattern2, Score)
        context_similarity/3,        % context_similarity(Pattern1, Pattern2, Score)
        overall_similarity/3,        % overall_similarity(Pattern1, Pattern2, Score)

        %% Pattern detection
        detect_pattern/2,            % detect_pattern(Input, PatternMatches)
        classify_pattern/2,          % classify_pattern(Pattern, Category)
        extract_features/2,          % extract_features(Pattern, Features)

        %% Pattern matching
        match_pattern/3,             % match_pattern(Pattern, Template, Bindings)
        find_similar_patterns/3,     % find_similar_patterns(Pattern, Threshold, SimilarPatterns)
        cluster_patterns/2,          % cluster_patterns(Patterns, Clusters)

        %% Pattern normalization
        normalize_pattern/2,         % normalize_pattern(RawPattern, NormalizedPattern)
        canonicalize/2,              % canonicalize(Pattern, CanonicalForm)

        %% Pattern metrics
        pattern_complexity/2,        % pattern_complexity(Pattern, Score)
        pattern_specificity/2,       % pattern_specificity(Pattern, Score)
        pattern_coverage/2,          % pattern_coverage(Pattern, CoveredCases)

        %% Utility predicates
        levenshtein_distance/3,      % levenshtein_distance(Str1, Str2, Distance)
        jaccard_similarity/3,        % jaccard_similarity(Set1, Set2, Score)
        cosine_similarity/3          % cosine_similarity(Vector1, Vector2, Score)
    ]).

    :- private([
        category_hierarchy/2,        % category_hierarchy(Category, ParentCategory)
        pattern_features/2,          % pattern_features(Pattern, FeatureList)
        known_pattern_template/3     % known_pattern_template(TemplateId, Template, Category)
    ]).

    :- dynamic([
        category_hierarchy/2,
        pattern_features/2,
        known_pattern_template/3
    ]).

    %% ============================================================
    %% CATEGORY HIERARCHY (for similarity calculations)
    %% ============================================================

    %% Security-related categories
    category_hierarchy(vulnerability, security).
    category_hierarchy(injection, vulnerability).
    category_hierarchy(authentication, security).
    category_hierarchy(authorization, security).
    category_hierarchy(cryptography, security).
    category_hierarchy(secrets, security).

    %% Code quality categories
    category_hierarchy(syntax_error, code_quality).
    category_hierarchy(unused_code, code_quality).
    category_hierarchy(complexity, code_quality).
    category_hierarchy(style, code_quality).
    category_hierarchy(documentation, code_quality).

    %% CI/CD categories
    category_hierarchy(workflow, cicd).
    category_hierarchy(permissions, workflow).
    category_hierarchy(pinned_dependencies, workflow).
    category_hierarchy(branch_protection, cicd).
    category_hierarchy(code_review, cicd).

    %% Compliance categories
    category_hierarchy(license, compliance).
    category_hierarchy(spdx, license).
    category_hierarchy(policy, compliance).
    category_hierarchy(standards, compliance).

    %% ============================================================
    %% SIMILARITY CALCULATIONS
    %% ============================================================

    %% Calculate structural similarity between two patterns
    %% Based on term structure comparison
    structural_similarity(Pattern1, Pattern2, Score) :-
        ( Pattern1 == Pattern2 ->
            Score = 1.0
        ; compound(Pattern1), compound(Pattern2) ->
            functor(Pattern1, Name1, Arity1),
            functor(Pattern2, Name2, Arity2),
            ( Name1 == Name2, Arity1 == Arity2 ->
                compare_args(Pattern1, Pattern2, Arity1, ArgScores),
                average(ArgScores, ArgAvg),
                Score is 0.5 + (ArgAvg * 0.5)  % Bonus for matching functor
            ; Name1 == Name2 ->
                Score is 0.3 + (0.2 * min(Arity1, Arity2) / max(Arity1, Arity2))
            ; Score is 0.1
            )
        ; atom(Pattern1), atom(Pattern2) ->
            atom_string(Pattern1, Str1),
            atom_string(Pattern2, Str2),
            string_similarity(Str1, Str2, Score)
        ; Score is 0.0
        ).

    compare_args(_, _, 0, []) :- !.
    compare_args(Term1, Term2, N, [Score|Rest]) :-
        N > 0,
        arg(N, Term1, Arg1),
        arg(N, Term2, Arg2),
        structural_similarity(Arg1, Arg2, Score),
        N1 is N - 1,
        compare_args(Term1, Term2, N1, Rest).

    %% Calculate category similarity using hierarchy distance
    category_similarity(Pattern1, Pattern2, Score) :-
        ( classify_pattern(Pattern1, Cat1),
          classify_pattern(Pattern2, Cat2) ->
            category_distance(Cat1, Cat2, Distance),
            Score is 1.0 / (1.0 + Distance)
        ; Score is 0.5  % Unknown categories, neutral score
        ).

    category_distance(Cat, Cat, 0) :- !.
    category_distance(Cat1, Cat2, Distance) :-
        find_common_ancestor(Cat1, Cat2, _, D1, D2),
        Distance is D1 + D2.
    category_distance(_, _, 5) :- !.  % Max distance if no common ancestor

    find_common_ancestor(Cat1, Cat2, Ancestor, D1, D2) :-
        ancestors(Cat1, Anc1),
        ancestors(Cat2, Anc2),
        member(Ancestor-D1, Anc1),
        member(Ancestor-D2, Anc2),
        !.

    ancestors(Cat, [Cat-0|Ancestors]) :-
        findall(
            Parent-D,
            (
                category_hierarchy(Cat, Parent),
                ancestors(Parent, ParentAnc),
                member(Anc-PD, ParentAnc),
                D is PD + 1,
                Anc = Parent
            ),
            Ancestors
        ).
    ancestors(_, []).

    %% Calculate context similarity based on features
    context_similarity(Pattern1, Pattern2, Score) :-
        extract_features(Pattern1, Features1),
        extract_features(Pattern2, Features2),
        jaccard_similarity(Features1, Features2, Score).

    %% Calculate overall similarity (weighted combination)
    overall_similarity(Pattern1, Pattern2, Score) :-
        structural_similarity(Pattern1, Pattern2, StructScore),
        category_similarity(Pattern1, Pattern2, CatScore),
        context_similarity(Pattern1, Pattern2, CtxScore),
        Score is (StructScore * 0.5) + (CatScore * 0.3) + (CtxScore * 0.2).

    %% ============================================================
    %% PATTERN DETECTION
    %% ============================================================

    %% Detect patterns in input
    detect_pattern(Input, PatternMatches) :-
        findall(
            match(Template, Category, Confidence, Bindings),
            (
                known_pattern_template(_, Template, Category),
                match_pattern(Input, Template, Bindings),
                calculate_match_confidence(Input, Template, Bindings, Confidence)
            ),
            PatternMatches
        ).

    calculate_match_confidence(Input, Template, Bindings, Confidence) :-
        length(Bindings, BindCount),
        term_size(Template, TemplateSize),
        term_size(Input, InputSize),
        ( TemplateSize > 0, InputSize > 0 ->
            SizeRatio is min(TemplateSize, InputSize) / max(TemplateSize, InputSize),
            BindRatio is BindCount / max(1, TemplateSize),
            Confidence is (SizeRatio * 0.6) + (BindRatio * 0.4)
        ; Confidence is 0.5
        ).

    term_size(Term, Size) :-
        ( compound(Term) ->
            functor(Term, _, Arity),
            findall(S, (between(1, Arity, N), arg(N, Term, A), term_size(A, S)), Sizes),
            sum_list(Sizes, ArgsSize),
            Size is 1 + ArgsSize
        ; Size is 1
        ).

    %% Classify a pattern into a category
    classify_pattern(Pattern, Category) :-
        ( pattern_features(Pattern, Features) ->
            classify_from_features(Features, Category)
        ; atom(Pattern) ->
            classify_from_name(Pattern, Category)
        ; compound(Pattern) ->
            functor(Pattern, Name, _),
            classify_from_name(Name, Category)
        ; Category = unknown
        ).

    classify_from_features(Features, Category) :-
        ( member(security, Features) -> Category = security
        ; member(workflow, Features) -> Category = workflow
        ; member(code_quality, Features) -> Category = code_quality
        ; member(compliance, Features) -> Category = compliance
        ; Category = general
        ).

    classify_from_name(Name, Category) :-
        atom_string(Name, NameStr),
        ( sub_string(NameStr, _, _, _, "inject") -> Category = injection
        ; sub_string(NameStr, _, _, _, "vuln") -> Category = vulnerability
        ; sub_string(NameStr, _, _, _, "permission") -> Category = permissions
        ; sub_string(NameStr, _, _, _, "secret") -> Category = secrets
        ; sub_string(NameStr, _, _, _, "pin") -> Category = pinned_dependencies
        ; sub_string(NameStr, _, _, _, "workflow") -> Category = workflow
        ; sub_string(NameStr, _, _, _, "spdx") -> Category = spdx
        ; sub_string(NameStr, _, _, _, "license") -> Category = license
        ; sub_string(NameStr, _, _, _, "branch") -> Category = branch_protection
        ; sub_string(NameStr, _, _, _, "review") -> Category = code_review
        ; sub_string(NameStr, _, _, _, "unused") -> Category = unused_code
        ; sub_string(NameStr, _, _, _, "syntax") -> Category = syntax_error
        ; Category = general
        ).

    %% Extract features from a pattern
    extract_features(Pattern, Features) :-
        findall(Feature, extract_single_feature(Pattern, Feature), Features).

    extract_single_feature(Pattern, Feature) :-
        ( atom(Pattern) ->
            atom_string(Pattern, Str),
            extract_string_feature(Str, Feature)
        ; compound(Pattern) ->
            functor(Pattern, Name, Arity),
            ( extract_string_feature(Name, Feature)
            ; between(1, Arity, N),
              arg(N, Pattern, Arg),
              extract_single_feature(Arg, Feature)
            )
        ; true, fail  % No features from numbers, etc.
        ).

    extract_string_feature(Str, Feature) :-
        ( sub_atom(Str, _, _, _, security) -> Feature = security
        ; sub_atom(Str, _, _, _, workflow) -> Feature = workflow
        ; sub_atom(Str, _, _, _, permission) -> Feature = permission
        ; sub_atom(Str, _, _, _, action) -> Feature = action
        ; sub_atom(Str, _, _, _, sha) -> Feature = sha_pin
        ; sub_atom(Str, _, _, _, spdx) -> Feature = spdx
        ; sub_atom(Str, _, _, _, error) -> Feature = error
        ; sub_atom(Str, _, _, _, fix) -> Feature = fix
        ; sub_atom(Str, _, _, _, repo) -> Feature = repository
        ; sub_atom(Str, _, _, _, commit) -> Feature = commit
        ; fail
        ).

    %% ============================================================
    %% PATTERN MATCHING
    %% ============================================================

    %% Match a pattern against a template, returning bindings
    match_pattern(Pattern, Template, Bindings) :-
        match_pattern_acc(Pattern, Template, [], Bindings).

    match_pattern_acc(Pattern, Var, Acc, [Var=Pattern|Acc]) :-
        var(Var), !.
    match_pattern_acc(Pattern, Pattern, Acc, Acc) :-
        atomic(Pattern), !.
    match_pattern_acc(Pattern, Template, Acc, Bindings) :-
        compound(Pattern),
        compound(Template),
        functor(Pattern, Name, Arity),
        functor(Template, Name, Arity),
        match_args(Pattern, Template, Arity, Acc, Bindings).

    match_args(_, _, 0, Acc, Acc) :- !.
    match_args(Pattern, Template, N, Acc, Bindings) :-
        N > 0,
        arg(N, Pattern, PArg),
        arg(N, Template, TArg),
        match_pattern_acc(PArg, TArg, Acc, Acc1),
        N1 is N - 1,
        match_args(Pattern, Template, N1, Acc1, Bindings).

    %% Find patterns similar to a given pattern above a threshold
    find_similar_patterns(Pattern, Threshold, SimilarPatterns) :-
        findall(
            similar(OtherPattern, Similarity),
            (
                known_pattern_template(_, OtherPattern, _),
                OtherPattern \== Pattern,
                overall_similarity(Pattern, OtherPattern, Similarity),
                Similarity >= Threshold
            ),
            SimilarPatterns
        ).

    %% Cluster patterns by similarity
    cluster_patterns(Patterns, Clusters) :-
        cluster_patterns_acc(Patterns, [], Clusters).

    cluster_patterns_acc([], Clusters, Clusters).
    cluster_patterns_acc([P|Rest], Acc, Clusters) :-
        find_cluster(P, Acc, MatchCluster),
        ( MatchCluster = found(ClusterId) ->
            add_to_cluster(P, ClusterId, Acc, NewAcc)
        ; NewAcc = [[P]|Acc]
        ),
        cluster_patterns_acc(Rest, NewAcc, Clusters).

    find_cluster(Pattern, Clusters, Result) :-
        ( member_with_index(Cluster, Index, Clusters),
          member(ClusterMember, Cluster),
          overall_similarity(Pattern, ClusterMember, Sim),
          Sim >= 0.7 ->
            Result = found(Index)
        ; Result = not_found
        ).

    member_with_index(Element, Index, List) :-
        nth0(Index, List, Element).

    add_to_cluster(Pattern, ClusterId, Clusters, NewClusters) :-
        nth0(ClusterId, Clusters, OldCluster, Rest),
        nth0(ClusterId, NewClusters, [Pattern|OldCluster], Rest).

    %% ============================================================
    %% PATTERN NORMALIZATION
    %% ============================================================

    %% Normalize a raw pattern to a standard form
    normalize_pattern(RawPattern, NormalizedPattern) :-
        ( atom(RawPattern) ->
            downcase_atom(RawPattern, LowerCase),
            atom_string(LowerCase, Str),
            normalize_string(Str, NormStr),
            atom_string(NormalizedPattern, NormStr)
        ; compound(RawPattern) ->
            functor(RawPattern, Name, Arity),
            normalize_pattern(Name, NormName),
            normalize_args(RawPattern, Arity, NormArgs),
            NormalizedPattern =.. [NormName|NormArgs]
        ; NormalizedPattern = RawPattern
        ).

    normalize_string(Str, NormStr) :-
        % Remove extra whitespace, normalize separators
        split_string(Str, " -_", " -_", Parts),
        exclude(=(""), Parts, NonEmpty),
        atomics_to_string(NonEmpty, "_", NormStr).

    normalize_args(_, 0, []) :- !.
    normalize_args(Term, N, [NormArg|Rest]) :-
        N > 0,
        arg(N, Term, Arg),
        normalize_pattern(Arg, NormArg),
        N1 is N - 1,
        normalize_args(Term, N1, Rest).

    %% Convert pattern to canonical form for comparison
    canonicalize(Pattern, CanonicalForm) :-
        normalize_pattern(Pattern, Normalized),
        sort_pattern_args(Normalized, CanonicalForm).

    sort_pattern_args(Pattern, Sorted) :-
        ( compound(Pattern) ->
            functor(Pattern, Name, Arity),
            findall(Arg, (between(1, Arity, N), arg(N, Pattern, Arg)), Args),
            maplist(sort_pattern_args, Args, SortedArgs),
            msort(SortedArgs, OrderedArgs),
            Sorted =.. [Name|OrderedArgs]
        ; Sorted = Pattern
        ).

    %% ============================================================
    %% PATTERN METRICS
    %% ============================================================

    %% Calculate pattern complexity score
    pattern_complexity(Pattern, Score) :-
        term_size(Pattern, Size),
        term_depth(Pattern, Depth),
        unique_symbols(Pattern, SymCount),
        Score is (Size * 0.4) + (Depth * 0.3) + (SymCount * 0.3).

    term_depth(Term, Depth) :-
        ( compound(Term) ->
            functor(Term, _, Arity),
            ( Arity > 0 ->
                findall(D, (between(1, Arity, N), arg(N, Term, A), term_depth(A, D)), Depths),
                max_list(Depths, MaxDepth),
                Depth is MaxDepth + 1
            ; Depth is 1
            )
        ; Depth is 0
        ).

    unique_symbols(Term, Count) :-
        findall(Sym, term_symbol(Term, Sym), Symbols),
        sort(Symbols, Unique),
        length(Unique, Count).

    term_symbol(Term, Symbol) :-
        ( atom(Term) -> Symbol = Term
        ; compound(Term) ->
            ( functor(Term, Name, Arity),
              Symbol = Name
            ; between(1, Arity, N),
              arg(N, Term, Arg),
              term_symbol(Arg, Symbol)
            )
        ; fail
        ).

    %% Calculate pattern specificity (how narrow/specific is the pattern)
    pattern_specificity(Pattern, Score) :-
        ( var(Pattern) ->
            Score is 0.0  % Variables are maximally general
        ; atom(Pattern) ->
            atom_length(Pattern, Len),
            Score is min(1.0, Len / 50)  % Longer atoms more specific
        ; compound(Pattern) ->
            functor(Pattern, _, Arity),
            findall(S, (between(1, Arity, N), arg(N, Pattern, A), pattern_specificity(A, S)), Scores),
            ( Scores == [] -> Score is 0.5
            ; average(Scores, AvgSpec),
              Score is 0.3 + (AvgSpec * 0.7)  % Compound terms slightly more specific
            )
        ; Score is 0.5
        ).

    %% Calculate pattern coverage (how many cases does it cover)
    pattern_coverage(Pattern, CoveredCases) :-
        pattern_specificity(Pattern, Specificity),
        ( Specificity > 0 ->
            CoveredCases is round(100 / Specificity)
        ; CoveredCases is 1000  % Very general pattern
        ).

    %% ============================================================
    %% UTILITY PREDICATES
    %% ============================================================

    %% Calculate Levenshtein distance between two strings
    levenshtein_distance(Str1, Str2, Distance) :-
        string_chars(Str1, Chars1),
        string_chars(Str2, Chars2),
        length(Chars1, Len1),
        length(Chars2, Len2),
        levenshtein_matrix(Chars1, Chars2, Len1, Len2, Matrix),
        get_matrix_value(Matrix, Len1, Len2, Distance).

    levenshtein_matrix(Chars1, Chars2, Len1, Len2, Matrix) :-
        findall(
            cell(I, J, Value),
            (
                between(0, Len1, I),
                between(0, Len2, J),
                levenshtein_cell(Chars1, Chars2, I, J, Value)
            ),
            Matrix
        ).

    levenshtein_cell(_, _, 0, J, J) :- !.
    levenshtein_cell(_, _, I, 0, I) :- !.
    levenshtein_cell(Chars1, Chars2, I, J, Value) :-
        I > 0, J > 0,
        nth1(I, Chars1, C1),
        nth1(J, Chars2, C2),
        I1 is I - 1,
        J1 is J - 1,
        ( C1 == C2 ->
            levenshtein_cell(Chars1, Chars2, I1, J1, Value)
        ; levenshtein_cell(Chars1, Chars2, I1, J, V1),
          levenshtein_cell(Chars1, Chars2, I, J1, V2),
          levenshtein_cell(Chars1, Chars2, I1, J1, V3),
          Value is 1 + min(V1, min(V2, V3))
        ).

    get_matrix_value(Matrix, I, J, Value) :-
        member(cell(I, J, Value), Matrix), !.

    %% Calculate Jaccard similarity between two sets
    jaccard_similarity(Set1, Set2, Score) :-
        intersection(Set1, Set2, Intersection),
        union(Set1, Set2, Union),
        length(Intersection, IntLen),
        length(Union, UnionLen),
        ( UnionLen > 0 ->
            Score is IntLen / UnionLen
        ; Score is 1.0  % Both empty sets are identical
        ).

    %% Calculate cosine similarity between two vectors
    cosine_similarity(Vector1, Vector2, Score) :-
        dot_product(Vector1, Vector2, DotProd),
        magnitude(Vector1, Mag1),
        magnitude(Vector2, Mag2),
        ( Mag1 > 0, Mag2 > 0 ->
            Score is DotProd / (Mag1 * Mag2)
        ; Score is 0.0
        ).

    dot_product([], [], 0).
    dot_product([H1|T1], [H2|T2], Result) :-
        dot_product(T1, T2, RestResult),
        Result is (H1 * H2) + RestResult.

    magnitude(Vector, Mag) :-
        findall(Sq, (member(V, Vector), Sq is V * V), Squares),
        sum_list(Squares, SumSq),
        Mag is sqrt(SumSq).

    %% String similarity based on Levenshtein distance
    string_similarity(Str1, Str2, Score) :-
        string_length(Str1, Len1),
        string_length(Str2, Len2),
        MaxLen is max(Len1, Len2),
        ( MaxLen > 0 ->
            levenshtein_distance(Str1, Str2, Dist),
            Score is 1.0 - (Dist / MaxLen)
        ; Score is 1.0  % Both empty strings
        ).

    %% ============================================================
    %% HELPER PREDICATES
    %% ============================================================

    average([], 0) :- !.
    average(List, Avg) :-
        sum_list(List, Sum),
        length(List, Len),
        Avg is Sum / Len.

    sum_list([], 0).
    sum_list([H|T], Sum) :-
        sum_list(T, Rest),
        Sum is H + Rest.

    max_list([X], X) :- !.
    max_list([H|T], Max) :-
        max_list(T, TMax),
        Max is max(H, TMax).

    %% Downcase atom (fallback if not available)
    :- if(\+ current_predicate(downcase_atom/2)).
    downcase_atom(Atom, Lower) :-
        atom_codes(Atom, Codes),
        maplist(to_lower_code, Codes, LowerCodes),
        atom_codes(Lower, LowerCodes).

    to_lower_code(C, L) :-
        ( C >= 65, C =< 90 ->
            L is C + 32
        ; L = C
        ).
    :- endif.

    %% Exclude helper (filter list)
    exclude(_, [], []).
    exclude(Goal, [H|T], Result) :-
        ( call(Goal, H) ->
            exclude(Goal, T, Result)
        ; Result = [H|Rest],
          exclude(Goal, T, Rest)
        ).

:- end_object.

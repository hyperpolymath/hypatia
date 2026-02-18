%% SPDX-License-Identifier: PMPL-1.0-or-later
%% cicd-hyper-a JSON Utilities
%%
%% Provides JSON serialization and deserialization utilities for converting
%% between Logtalk terms and JSON format for ArangoDB interaction.

:- object(json_utils).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2026-01-18,
        comment is 'JSON conversion utilities for ArangoDB graph model'
    ]).

    :- public([
        %% Finding conversions
        finding_to_json/2,
        json_to_finding/2,

        %% Session conversions
        session_to_json/2,
        json_to_session/2,

        %% Rule conversions
        rule_to_json/2,
        json_to_rule/2,

        %% Repo conversions
        repo_to_json/2,
        json_to_repo/2,

        %% Bot status conversions
        bot_status_to_json/2,
        json_to_bot_status/2,

        %% Edge conversions
        edge_to_json/3,
        json_to_edge/2,

        %% Generic conversions
        term_to_json/2,
        json_to_term/2,
        dict_to_json_string/2,
        json_string_to_dict/2,

        %% Helpers
        generate_key/1,
        current_iso_timestamp/1,
        atom_to_json_string/2
    ]).

    %% ============================================================
    %% FINDING CONVERSIONS
    %% Corresponds to definitions.vertex.finding in graph model
    %% ============================================================

    %% Convert finding term to JSON dict
    %% finding(Id, RuleId, Bot, Severity, Category, Message, File, Line, Col, Element, Suggestion, Fixable, Fixed, CreatedAt, Related, Meta)
    finding_to_json(
        finding(Id, RuleId, Bot, Severity, Category, Message, File, Line, Col, Element, Suggestion, Fixable, Fixed, CreatedAt, Related, Meta),
        JsonDict
    ) :-
        optional_field('_key', Id, KeyPair),
        optional_field(ruleId, RuleId, RuleIdPair),
        optional_field(botId, Bot, BotPair),
        optional_field(severity, Severity, SeverityPair),
        optional_field(category, Category, CategoryPair),
        optional_field(message, Message, MessagePair),
        optional_field(file, File, FilePair),
        optional_field(line, Line, LinePair),
        optional_field(column, Col, ColPair),
        optional_field(element, Element, ElementPair),
        optional_field(suggestion, Suggestion, SuggestionPair),
        boolean_field(fixable, Fixable, FixablePair),
        boolean_field(fixed, Fixed, FixedPair),
        optional_field(createdAt, CreatedAt, CreatedAtPair),
        list_field(related, Related, RelatedPair),
        metadata_field(metadata, Meta, MetaPair),
        flatten([KeyPair, RuleIdPair, BotPair, SeverityPair, CategoryPair, MessagePair,
                 FilePair, LinePair, ColPair, ElementPair, SuggestionPair, FixablePair,
                 FixedPair, CreatedAtPair, RelatedPair, MetaPair], Pairs),
        dict_create(JsonDict, json, Pairs).

    %% Convert JSON dict to finding term
    json_to_finding(JsonDict, Finding) :-
        get_dict_field(JsonDict, '_key', Id, ''),
        get_dict_field(JsonDict, ruleId, RuleId, ''),
        get_dict_field(JsonDict, botId, Bot, ''),
        get_dict_field(JsonDict, severity, Severity, info),
        get_dict_field(JsonDict, category, Category, ''),
        get_dict_field(JsonDict, message, Message, ''),
        get_dict_field(JsonDict, file, File, none),
        get_dict_field(JsonDict, line, Line, none),
        get_dict_field(JsonDict, column, Col, none),
        get_dict_field(JsonDict, element, Element, none),
        get_dict_field(JsonDict, suggestion, Suggestion, none),
        get_dict_field(JsonDict, fixable, Fixable, false),
        get_dict_field(JsonDict, fixed, Fixed, false),
        get_dict_field(JsonDict, createdAt, CreatedAt, ''),
        get_dict_field(JsonDict, related, Related, []),
        get_dict_field(JsonDict, metadata, Meta, []),
        Finding = finding(Id, RuleId, Bot, Severity, Category, Message, File, Line, Col, Element, Suggestion, Fixable, Fixed, CreatedAt, Related, Meta).

    %% ============================================================
    %% SESSION CONVERSIONS
    %% Corresponds to definitions.vertex.session in graph model
    %% ============================================================

    %% session(Id, RepoKey, StartedAt, CompletedAt, Status, Trigger, TriggeredBy, CommitSha, Branch, TotalFindings, CriticalFindings, AutoFixesApplied, ReleaseBlocked, ErrorMessage)
    session_to_json(
        session(Id, RepoKey, StartedAt, CompletedAt, Status, Trigger, TriggeredBy, CommitSha, Branch, TotalFindings, CriticalFindings, AutoFixesApplied, ReleaseBlocked, ErrorMessage),
        JsonDict
    ) :-
        optional_field('_key', Id, KeyPair),
        optional_field(repoKey, RepoKey, RepoKeyPair),
        optional_field(startedAt, StartedAt, StartedAtPair),
        optional_field(completedAt, CompletedAt, CompletedAtPair),
        optional_field(status, Status, StatusPair),
        optional_field(trigger, Trigger, TriggerPair),
        optional_field(triggeredBy, TriggeredBy, TriggeredByPair),
        optional_field(commitSha, CommitSha, CommitShaPair),
        optional_field(branch, Branch, BranchPair),
        optional_field(totalFindings, TotalFindings, TotalFindingsPair),
        optional_field(criticalFindings, CriticalFindings, CriticalFindingsPair),
        optional_field(autoFixesApplied, AutoFixesApplied, AutoFixesPair),
        boolean_field(releaseBlocked, ReleaseBlocked, ReleaseBlockedPair),
        optional_field(errorMessage, ErrorMessage, ErrorMessagePair),
        flatten([KeyPair, RepoKeyPair, StartedAtPair, CompletedAtPair, StatusPair,
                 TriggerPair, TriggeredByPair, CommitShaPair, BranchPair,
                 TotalFindingsPair, CriticalFindingsPair, AutoFixesPair,
                 ReleaseBlockedPair, ErrorMessagePair], Pairs),
        dict_create(JsonDict, json, Pairs).

    %% Convert JSON dict to session term
    json_to_session(JsonDict, Session) :-
        get_dict_field(JsonDict, '_key', Id, ''),
        get_dict_field(JsonDict, repoKey, RepoKey, ''),
        get_dict_field(JsonDict, startedAt, StartedAt, ''),
        get_dict_field(JsonDict, completedAt, CompletedAt, none),
        get_dict_field(JsonDict, status, Status, pending),
        get_dict_field(JsonDict, trigger, Trigger, manual),
        get_dict_field(JsonDict, triggeredBy, TriggeredBy, ''),
        get_dict_field(JsonDict, commitSha, CommitSha, ''),
        get_dict_field(JsonDict, branch, Branch, main),
        get_dict_field(JsonDict, totalFindings, TotalFindings, 0),
        get_dict_field(JsonDict, criticalFindings, CriticalFindings, 0),
        get_dict_field(JsonDict, autoFixesApplied, AutoFixesApplied, 0),
        get_dict_field(JsonDict, releaseBlocked, ReleaseBlocked, false),
        get_dict_field(JsonDict, errorMessage, ErrorMessage, none),
        Session = session(Id, RepoKey, StartedAt, CompletedAt, Status, Trigger, TriggeredBy, CommitSha, Branch, TotalFindings, CriticalFindings, AutoFixesApplied, ReleaseBlocked, ErrorMessage).

    %% ============================================================
    %% RULE CONVERSIONS
    %% Corresponds to definitions.vertex.rule in graph model
    %% ============================================================

    %% rule(Id, Name, Type, Severity, Category, Description, Detection, Fix, Bot, AutoFixable, CommitMsg, Workflow, Meta)
    rule_to_json(
        rule(Id, Name, Type, Severity, Category, Description, Detection, Fix, Bot, AutoFixable, CommitMsg, Workflow, Meta),
        JsonDict
    ) :-
        optional_field('_key', Id, KeyPair),
        optional_field(name, Name, NamePair),
        optional_field(type, Type, TypePair),
        optional_field(severity, Severity, SeverityPair),
        optional_field(category, Category, CategoryPair),
        optional_field(description, Description, DescPair),
        term_to_json_field(detection, Detection, DetectionPair),
        term_to_json_field(fix, Fix, FixPair),
        optional_field(botId, Bot, BotPair),
        boolean_field(autoFixable, AutoFixable, AutoFixablePair),
        optional_field(commitMessage, CommitMsg, CommitMsgPair),
        optional_field(preventionWorkflow, Workflow, WorkflowPair),
        metadata_field(metadata, Meta, MetaPair),
        flatten([KeyPair, NamePair, TypePair, SeverityPair, CategoryPair, DescPair,
                 DetectionPair, FixPair, BotPair, AutoFixablePair, CommitMsgPair,
                 WorkflowPair, MetaPair], Pairs),
        dict_create(JsonDict, json, Pairs).

    %% Convert JSON dict to rule term
    json_to_rule(JsonDict, Rule) :-
        get_dict_field(JsonDict, '_key', Id, ''),
        get_dict_field(JsonDict, name, Name, ''),
        get_dict_field(JsonDict, type, Type, detective),
        get_dict_field(JsonDict, severity, Severity, info),
        get_dict_field(JsonDict, category, Category, ''),
        get_dict_field(JsonDict, description, Description, ''),
        get_dict_field(JsonDict, detection, Detection, none),
        get_dict_field(JsonDict, fix, Fix, none),
        get_dict_field(JsonDict, botId, Bot, cicd_hyper_a),
        get_dict_field(JsonDict, autoFixable, AutoFixable, false),
        get_dict_field(JsonDict, commitMessage, CommitMsg, ''),
        get_dict_field(JsonDict, preventionWorkflow, Workflow, none),
        get_dict_field(JsonDict, metadata, Meta, []),
        Rule = rule(Id, Name, Type, Severity, Category, Description, Detection, Fix, Bot, AutoFixable, CommitMsg, Workflow, Meta).

    %% ============================================================
    %% REPO CONVERSIONS
    %% Corresponds to definitions.vertex.repo in graph model
    %% ============================================================

    %% repo(Key, Name, Forge, Owner, Url, Visibility, DefaultBranch, Languages, PrimaryLang, RsrCompliant, ScorecardScore, HealthScore, LastScanned, LastCommit, CreatedAt, Metadata)
    repo_to_json(
        repo(Key, Name, Forge, Owner, Url, Visibility, DefaultBranch, Languages, PrimaryLang, RsrCompliant, ScorecardScore, HealthScore, LastScanned, LastCommit, CreatedAt, Metadata),
        JsonDict
    ) :-
        optional_field('_key', Key, KeyPair),
        optional_field(name, Name, NamePair),
        optional_field(forge, Forge, ForgePair),
        optional_field(owner, Owner, OwnerPair),
        optional_field(url, Url, UrlPair),
        optional_field(visibility, Visibility, VisibilityPair),
        optional_field(defaultBranch, DefaultBranch, DefaultBranchPair),
        list_field(languages, Languages, LanguagesPair),
        optional_field(primaryLanguage, PrimaryLang, PrimaryLangPair),
        boolean_field(rsrCompliant, RsrCompliant, RsrCompliantPair),
        optional_field(scorecardScore, ScorecardScore, ScorecardScorePair),
        optional_field(healthScore, HealthScore, HealthScorePair),
        optional_field(lastScanned, LastScanned, LastScannedPair),
        optional_field(lastCommit, LastCommit, LastCommitPair),
        optional_field(createdAt, CreatedAt, CreatedAtPair),
        metadata_field(metadata, Metadata, MetadataPair),
        flatten([KeyPair, NamePair, ForgePair, OwnerPair, UrlPair, VisibilityPair,
                 DefaultBranchPair, LanguagesPair, PrimaryLangPair, RsrCompliantPair,
                 ScorecardScorePair, HealthScorePair, LastScannedPair, LastCommitPair,
                 CreatedAtPair, MetadataPair], Pairs),
        dict_create(JsonDict, json, Pairs).

    %% Convert JSON dict to repo term
    json_to_repo(JsonDict, Repo) :-
        get_dict_field(JsonDict, '_key', Key, ''),
        get_dict_field(JsonDict, name, Name, ''),
        get_dict_field(JsonDict, forge, Forge, github),
        get_dict_field(JsonDict, owner, Owner, ''),
        get_dict_field(JsonDict, url, Url, ''),
        get_dict_field(JsonDict, visibility, Visibility, public),
        get_dict_field(JsonDict, defaultBranch, DefaultBranch, main),
        get_dict_field(JsonDict, languages, Languages, []),
        get_dict_field(JsonDict, primaryLanguage, PrimaryLang, ''),
        get_dict_field(JsonDict, rsrCompliant, RsrCompliant, false),
        get_dict_field(JsonDict, scorecardScore, ScorecardScore, 0),
        get_dict_field(JsonDict, healthScore, HealthScore, 0),
        get_dict_field(JsonDict, lastScanned, LastScanned, none),
        get_dict_field(JsonDict, lastCommit, LastCommit, none),
        get_dict_field(JsonDict, createdAt, CreatedAt, ''),
        get_dict_field(JsonDict, metadata, Metadata, []),
        Repo = repo(Key, Name, Forge, Owner, Url, Visibility, DefaultBranch, Languages, PrimaryLang, RsrCompliant, ScorecardScore, HealthScore, LastScanned, LastCommit, CreatedAt, Metadata).

    %% ============================================================
    %% BOT STATUS CONVERSIONS
    %% Used for session_ran_bot edge properties
    %% ============================================================

    %% bot_status(SessionId, BotId, Status, StartedAt, CompletedAt, FindingsCount, FixesApplied, Error)
    bot_status_to_json(
        bot_status(SessionId, BotId, Status, StartedAt, CompletedAt, FindingsCount, FixesApplied, Error),
        JsonDict
    ) :-
        format(atom(FromKey), 'sessions/~w', [SessionId]),
        format(atom(ToKey), 'bots/~w', [BotId]),
        optional_field('_from', FromKey, FromPair),
        optional_field('_to', ToKey, ToPair),
        optional_field(status, Status, StatusPair),
        optional_field(startedAt, StartedAt, StartedAtPair),
        optional_field(completedAt, CompletedAt, CompletedAtPair),
        optional_field(findingsCount, FindingsCount, FindingsCountPair),
        optional_field(fixesApplied, FixesApplied, FixesAppliedPair),
        optional_field(error, Error, ErrorPair),
        flatten([FromPair, ToPair, StatusPair, StartedAtPair, CompletedAtPair,
                 FindingsCountPair, FixesAppliedPair, ErrorPair], Pairs),
        dict_create(JsonDict, json, Pairs).

    %% Convert JSON dict to bot_status term
    json_to_bot_status(JsonDict, BotStatus) :-
        get_dict_field(JsonDict, '_from', From, ''),
        get_dict_field(JsonDict, '_to', To, ''),
        extract_key_from_id(From, SessionId),
        extract_key_from_id(To, BotId),
        get_dict_field(JsonDict, status, Status, pending),
        get_dict_field(JsonDict, startedAt, StartedAt, ''),
        get_dict_field(JsonDict, completedAt, CompletedAt, none),
        get_dict_field(JsonDict, findingsCount, FindingsCount, 0),
        get_dict_field(JsonDict, fixesApplied, FixesApplied, 0),
        get_dict_field(JsonDict, error, Error, none),
        BotStatus = bot_status(SessionId, BotId, Status, StartedAt, CompletedAt, FindingsCount, FixesApplied, Error).

    %% ============================================================
    %% EDGE CONVERSIONS
    %% Generic edge document with _from, _to, and properties
    %% ============================================================

    %% Convert edge to JSON
    edge_to_json(FromCollection/FromKey, ToCollection/ToKey, Properties, JsonDict) :-
        format(atom(FromId), '~w/~w', [FromCollection, FromKey]),
        format(atom(ToId), '~w/~w', [ToCollection, ToKey]),
        ( is_list(Properties) ->
            dict_create(PropsDict, json, Properties)
        ; PropsDict = Properties
        ),
        put_dict('_from', PropsDict, FromId, D1),
        put_dict('_to', D1, ToId, JsonDict).

    edge_to_json(FromId, ToId, Properties, JsonDict) :-
        atom(FromId),
        atom(ToId),
        ( is_list(Properties) ->
            dict_create(PropsDict, json, Properties)
        ; PropsDict = Properties
        ),
        put_dict('_from', PropsDict, FromId, D1),
        put_dict('_to', D1, ToId, JsonDict).

    %% Convert JSON dict to edge
    json_to_edge(JsonDict, edge(From, To, Properties)) :-
        get_dict('_from', JsonDict, From),
        get_dict('_to', JsonDict, To),
        dict_pairs(JsonDict, _, AllPairs),
        exclude(is_edge_meta_field, AllPairs, PropPairs),
        dict_create(Properties, json, PropPairs).

    is_edge_meta_field('_from'-_).
    is_edge_meta_field('_to'-_).
    is_edge_meta_field('_key'-_).
    is_edge_meta_field('_id'-_).
    is_edge_meta_field('_rev'-_).

    %% ============================================================
    %% GENERIC CONVERSIONS
    %% ============================================================

    %% Convert Prolog term to JSON-compatible representation
    term_to_json(Term, Json) :-
        ( atom(Term) -> Json = Term
        ; number(Term) -> Json = Term
        ; is_list(Term) -> maplist(term_to_json, Term, Json)
        ; Term = none -> Json = null
        ; compound(Term) ->
            Term =.. [Functor|Args],
            maplist(term_to_json, Args, JsonArgs),
            dict_create(Json, json, [type-Functor, args-JsonArgs])
        ; Json = Term
        ).

    %% Convert JSON to Prolog term
    json_to_term(Json, Term) :-
        ( Json = null -> Term = none
        ; atom(Json) -> Term = Json
        ; number(Json) -> Term = Json
        ; is_list(Json) -> maplist(json_to_term, Json, Term)
        ; is_dict(Json) ->
            ( get_dict(type, Json, Type), get_dict(args, Json, Args) ->
                maplist(json_to_term, Args, TermArgs),
                Term =.. [Type|TermArgs]
            ; Term = Json
            )
        ; Term = Json
        ).

    %% Convert dict to JSON string
    dict_to_json_string(Dict, String) :-
        with_output_to(string(String), json_write_dict(current_output, Dict, [])).

    %% Convert JSON string to dict
    json_string_to_dict(String, Dict) :-
        open_string(String, Stream),
        json_read_dict(Stream, Dict),
        close(Stream).

    %% ============================================================
    %% HELPER PREDICATES
    %% ============================================================

    %% Generate a UUID-style key
    generate_key(Key) :-
        random(R1), random(R2), random(R3), random(R4),
        format(atom(Key), '~36r-~36r-~36r-~36r',
               [round(R1 * 100000000),
                round(R2 * 10000),
                round(R3 * 10000),
                round(R4 * 100000000000)]).

    %% Get current ISO 8601 timestamp
    current_iso_timestamp(Timestamp) :-
        get_time(Time),
        format_time(atom(Timestamp), '%Y-%m-%dT%H:%M:%SZ', Time).

    %% Convert atom to JSON-safe string
    atom_to_json_string(Atom, String) :-
        ( Atom == none -> String = ''
        ; atom_string(Atom, String)
        ).

    %% Optional field - returns empty list if value is none
    optional_field(_, none, []) :- !.
    optional_field(_, '', []) :- !.
    optional_field(Key, Value, [Key-Value]).

    %% Boolean field
    boolean_field(Key, true, [Key-true]) :- !.
    boolean_field(Key, false, [Key-false]) :- !.
    boolean_field(_, _, []).

    %% List field
    list_field(_, [], []) :- !.
    list_field(Key, List, [Key-List]) :- is_list(List).

    %% Metadata field (key-value list to object)
    metadata_field(_, [], []) :- !.
    metadata_field(Key, Meta, [Key-MetaDict]) :-
        is_list(Meta),
        dict_create(MetaDict, json, Meta).

    %% Term to JSON field (for complex terms like detection/fix specs)
    term_to_json_field(_, none, []) :- !.
    term_to_json_field(Key, Term, [Key-Json]) :-
        term_to_json(Term, Json).

    %% Get field from dict with default value
    get_dict_field(Dict, Key, Value, _Default) :-
        get_dict(Key, Dict, Value), !.
    get_dict_field(_Dict, _Key, Default, Default).

    %% Extract key from ArangoDB document ID (collection/key -> key)
    extract_key_from_id(Id, Key) :-
        atom(Id),
        atom_string(Id, IdStr),
        ( split_string(IdStr, "/", "", [_, KeyStr]) ->
            atom_string(Key, KeyStr)
        ; Key = Id
        ).

    %% Flatten list (SWI-Prolog built-in, but define for portability)
    :- if(\+ predicate_property(flatten(_,_), built_in)).
    flatten([], []).
    flatten([H|T], Flat) :-
        is_list(H), !,
        flatten(H, FlatH),
        flatten(T, FlatT),
        append(FlatH, FlatT, Flat).
    flatten([H|T], [H|FlatT]) :-
        flatten(T, FlatT).
    :- endif.

:- end_object.

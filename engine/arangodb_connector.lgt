%% SPDX-License-Identifier: AGPL-3.0-or-later
%% cicd-hyper-a ArangoDB Connector
%%
%% Main connector module for interacting with ArangoDB.
%% Provides high-level operations for storing and querying CI/CD data
%% according to the graph model defined in schemas/arangodb-graph-model.json.

:- object(arangodb_connector).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2026-01-18,
        comment is 'ArangoDB connector for cicd-hyper-a rules engine'
    ]).

    :- public([
        %% Connection management
        connect/3,
        connect/4,
        disconnect/0,
        is_connected/0,
        get_connection_info/1,

        %% Finding operations
        store_finding/1,
        store_finding/2,
        store_findings/1,
        get_finding/2,
        query_findings/2,
        update_finding/2,
        delete_finding/1,

        %% Session operations
        store_session/1,
        store_session/2,
        get_session/2,
        query_sessions/2,
        update_session/2,
        complete_session/2,

        %% Repository operations
        store_repo/1,
        get_repo/2,
        query_repos/2,
        update_repo/2,

        %% Rule operations
        store_rule/1,
        store_rule/2,
        get_rule/2,
        query_rules/2,
        update_rule/2,
        delete_rule/1,

        %% Bot status operations
        update_bot_status/3,
        get_bot_status/3,
        get_session_bot_statuses/2,

        %% Edge operations
        create_edge/4,
        delete_edge/3,
        get_edges/3,

        %% Graph queries (AQL)
        execute_aql/2,
        execute_aql/3,

        %% Collection management
        ensure_collections/0,
        list_collections/1,
        get_collection_count/2
    ]).

    :- private([
        connection_state/4,
        database_name/1,
        build_document_url/3,
        build_collection_url/2,
        build_aql_url/1,
        handle_response/3,
        ensure_key/2
    ]).

    :- dynamic([
        connection_state/4,
        database_name/1
    ]).

    %% ============================================================
    %% COLLECTION NAMES (from graph model)
    %% ============================================================

    %% Document collections
    document_collection(repos).
    document_collection(bots).
    document_collection(sessions).
    document_collection(findings).
    document_collection(rules).
    document_collection(fixes).
    document_collection(workflows).
    document_collection(learningData).
    document_collection(owners).

    %% Edge collections
    edge_collection(repo_has_workflow).
    edge_collection(repo_owned_by).
    edge_collection(session_scans_repo).
    edge_collection(session_ran_bot).
    edge_collection(bot_found_finding).
    edge_collection(finding_in_session).
    edge_collection(finding_triggered_by).
    edge_collection(finding_in_file).
    edge_collection(finding_fixed_by).
    edge_collection(fix_applies_rule).
    edge_collection(bot_owns_rule).
    edge_collection(bot_depends_on).
    edge_collection(rule_learned_from).
    edge_collection(finding_related_to).
    edge_collection(workflow_triggers_rule).
    edge_collection(repo_similar_to).

    %% ============================================================
    %% CONNECTION MANAGEMENT
    %% ============================================================

    %% Connect to ArangoDB
    %% connect(+Host, +Port, +Database)
    connect(Host, Port, Database) :-
        connect(Host, Port, Database, []).

    %% Connect with options (including auth)
    %% connect(+Host, +Port, +Database, +Options)
    %% Options: [username(User), password(Pass)]
    connect(Host, Port, Database, Options) :-
        disconnect,  %% Clean up any existing connection
        format(atom(BaseUrl), 'http://~w:~w/_db/~w', [Host, Port, Database]),
        http_client::set_base_url(BaseUrl),
        ( member(username(User), Options), member(password(Pass), Options) ->
            http_client::set_auth(User, Pass)
        ; true
        ),
        %% Verify connection by checking server version
        ( http_client::get_json('/_api/version', Response),
            http_client::is_success(Response) ->
            assertz(connection_state(Host, Port, Database, connected)),
            assertz(database_name(Database))
        ; throw(connection_failed(Host, Port, Database))
        ).

    %% Disconnect from ArangoDB
    disconnect :-
        retractall(connection_state(_, _, _, _)),
        retractall(database_name(_)),
        http_client::clear_auth.

    %% Check if connected
    is_connected :-
        connection_state(_, _, _, connected).

    %% Get current connection info
    get_connection_info(info(Host, Port, Database, Status)) :-
        ( connection_state(Host, Port, Database, Status) ->
            true
        ; Host = none, Port = none, Database = none, Status = disconnected
        ).

    %% ============================================================
    %% FINDING OPERATIONS
    %% ============================================================

    %% Store a finding
    %% store_finding(+Finding)
    store_finding(Finding) :-
        store_finding(Finding, _Key).

    %% Store a finding and return the key
    %% store_finding(+Finding, -Key)
    store_finding(Finding, Key) :-
        ensure_connected,
        json_utils::finding_to_json(Finding, JsonDict),
        ensure_key(JsonDict, KeyedDict),
        build_collection_url(findings, Url),
        http_client::post_json(Url, KeyedDict, Response),
        handle_response(Response, store, Key).

    %% Store multiple findings
    %% store_findings(+Findings)
    store_findings(Findings) :-
        ensure_connected,
        maplist(store_finding, Findings).

    %% Get a finding by key
    %% get_finding(+Key, -Finding)
    get_finding(Key, Finding) :-
        ensure_connected,
        build_document_url(findings, Key, Url),
        http_client::get_json(Url, Response),
        ( http_client::is_success(Response) ->
            json_utils::json_to_finding(Response, Finding)
        ; throw(finding_not_found(Key))
        ).

    %% Query findings by criteria
    %% query_findings(+Criteria, -Findings)
    %% Criteria: list of Key=Value pairs or AQL filter string
    query_findings(Criteria, Findings) :-
        ensure_connected,
        build_findings_query(Criteria, Aql),
        execute_aql(Aql, Results),
        maplist(json_utils::json_to_finding, Results, Findings).

    %% Update a finding
    %% update_finding(+Key, +Updates)
    update_finding(Key, Updates) :-
        ensure_connected,
        build_document_url(findings, Key, Url),
        ( is_dict(Updates) -> JsonDict = Updates
        ; dict_create(JsonDict, json, Updates)
        ),
        http_client::patch_json(Url, JsonDict, Response),
        handle_response(Response, update, _).

    %% Delete a finding
    %% delete_finding(+Key)
    delete_finding(Key) :-
        ensure_connected,
        build_document_url(findings, Key, Url),
        http_client::delete(Url, Response),
        handle_response(Response, delete, _).

    %% ============================================================
    %% SESSION OPERATIONS
    %% ============================================================

    %% Store a session
    store_session(Session) :-
        store_session(Session, _Key).

    %% Store a session and return the key
    store_session(Session, Key) :-
        ensure_connected,
        json_utils::session_to_json(Session, JsonDict),
        ensure_key(JsonDict, KeyedDict),
        build_collection_url(sessions, Url),
        http_client::post_json(Url, KeyedDict, Response),
        handle_response(Response, store, Key).

    %% Get a session by key
    get_session(Key, Session) :-
        ensure_connected,
        build_document_url(sessions, Key, Url),
        http_client::get_json(Url, Response),
        ( http_client::is_success(Response) ->
            json_utils::json_to_session(Response, Session)
        ; throw(session_not_found(Key))
        ).

    %% Query sessions
    query_sessions(Criteria, Sessions) :-
        ensure_connected,
        build_sessions_query(Criteria, Aql),
        execute_aql(Aql, Results),
        maplist(json_utils::json_to_session, Results, Sessions).

    %% Update a session
    update_session(Key, Updates) :-
        ensure_connected,
        build_document_url(sessions, Key, Url),
        ( is_dict(Updates) -> JsonDict = Updates
        ; dict_create(JsonDict, json, Updates)
        ),
        http_client::patch_json(Url, JsonDict, Response),
        handle_response(Response, update, _).

    %% Complete a session with summary
    complete_session(Key, Summary) :-
        ensure_connected,
        json_utils::current_iso_timestamp(Now),
        Updates = [
            status = completed,
            completedAt = Now,
            totalFindings = Summary.total_findings,
            criticalFindings = Summary.critical_findings,
            autoFixesApplied = Summary.auto_fixes
        ],
        update_session(Key, Updates).

    %% ============================================================
    %% REPOSITORY OPERATIONS
    %% ============================================================

    %% Store a repository
    store_repo(Repo) :-
        ensure_connected,
        json_utils::repo_to_json(Repo, JsonDict),
        ensure_key(JsonDict, KeyedDict),
        build_collection_url(repos, Url),
        http_client::post_json(Url, KeyedDict, Response),
        handle_response(Response, store, _).

    %% Get a repository by key
    get_repo(Key, Repo) :-
        ensure_connected,
        build_document_url(repos, Key, Url),
        http_client::get_json(Url, Response),
        ( http_client::is_success(Response) ->
            json_utils::json_to_repo(Response, Repo)
        ; throw(repo_not_found(Key))
        ).

    %% Query repositories
    query_repos(Criteria, Repos) :-
        ensure_connected,
        build_repos_query(Criteria, Aql),
        execute_aql(Aql, Results),
        maplist(json_utils::json_to_repo, Results, Repos).

    %% Update a repository
    update_repo(Key, Updates) :-
        ensure_connected,
        build_document_url(repos, Key, Url),
        ( is_dict(Updates) -> JsonDict = Updates
        ; dict_create(JsonDict, json, Updates)
        ),
        http_client::patch_json(Url, JsonDict, Response),
        handle_response(Response, update, _).

    %% ============================================================
    %% RULE OPERATIONS
    %% ============================================================

    %% Store a rule
    store_rule(Rule) :-
        store_rule(Rule, _Key).

    %% Store a rule and return the key
    store_rule(Rule, Key) :-
        ensure_connected,
        json_utils::rule_to_json(Rule, JsonDict),
        ensure_key(JsonDict, KeyedDict),
        build_collection_url(rules, Url),
        http_client::post_json(Url, KeyedDict, Response),
        handle_response(Response, store, Key).

    %% Get a rule by key (ID like 'RSR-001')
    get_rule(Key, Rule) :-
        ensure_connected,
        build_document_url(rules, Key, Url),
        http_client::get_json(Url, Response),
        ( http_client::is_success(Response) ->
            json_utils::json_to_rule(Response, Rule)
        ; throw(rule_not_found(Key))
        ).

    %% Query rules
    query_rules(Criteria, Rules) :-
        ensure_connected,
        build_rules_query(Criteria, Aql),
        execute_aql(Aql, Results),
        maplist(json_utils::json_to_rule, Results, Rules).

    %% Update a rule
    update_rule(Key, Updates) :-
        ensure_connected,
        build_document_url(rules, Key, Url),
        ( is_dict(Updates) -> JsonDict = Updates
        ; dict_create(JsonDict, json, Updates)
        ),
        http_client::patch_json(Url, JsonDict, Response),
        handle_response(Response, update, _).

    %% Delete a rule
    delete_rule(Key) :-
        ensure_connected,
        build_document_url(rules, Key, Url),
        http_client::delete(Url, Response),
        handle_response(Response, delete, _).

    %% ============================================================
    %% BOT STATUS OPERATIONS
    %% ============================================================

    %% Update bot execution status in a session
    %% update_bot_status(+SessionId, +BotId, +StatusUpdate)
    %% StatusUpdate: dict or list with status, findingsCount, fixesApplied, error
    update_bot_status(SessionId, BotId, StatusUpdate) :-
        ensure_connected,
        format(atom(FromId), 'sessions/~w', [SessionId]),
        format(atom(ToId), 'bots/~w', [BotId]),
        ( is_dict(StatusUpdate) -> Updates = StatusUpdate
        ; dict_create(Updates, json, StatusUpdate)
        ),
        %% Check if edge exists
        Aql = 'FOR e IN session_ran_bot FILTER e._from == @from AND e._to == @to RETURN e',
        execute_aql(Aql, [from=FromId, to=ToId], Existing),
        ( Existing = [ExistingEdge|_] ->
            %% Update existing edge
            get_dict('_key', ExistingEdge, EdgeKey),
            build_document_url(session_ran_bot, EdgeKey, Url),
            http_client::patch_json(Url, Updates, Response)
        ; %% Create new edge
            json_utils::edge_to_json(FromId, ToId, StatusUpdate, EdgeJson),
            build_collection_url(session_ran_bot, Url),
            http_client::post_json(Url, EdgeJson, Response)
        ),
        handle_response(Response, update, _).

    %% Get bot status for a session
    get_bot_status(SessionId, BotId, Status) :-
        ensure_connected,
        format(atom(FromId), 'sessions/~w', [SessionId]),
        format(atom(ToId), 'bots/~w', [BotId]),
        Aql = 'FOR e IN session_ran_bot FILTER e._from == @from AND e._to == @to RETURN e',
        execute_aql(Aql, [from=FromId, to=ToId], Results),
        ( Results = [StatusJson|_] ->
            json_utils::json_to_bot_status(StatusJson, Status)
        ; throw(bot_status_not_found(SessionId, BotId))
        ).

    %% Get all bot statuses for a session
    get_session_bot_statuses(SessionId, Statuses) :-
        ensure_connected,
        format(atom(FromId), 'sessions/~w', [SessionId]),
        Aql = 'FOR e IN session_ran_bot FILTER e._from == @from RETURN e',
        execute_aql(Aql, [from=FromId], Results),
        maplist(json_utils::json_to_bot_status, Results, Statuses).

    %% ============================================================
    %% EDGE OPERATIONS
    %% ============================================================

    %% Create an edge between two documents
    %% create_edge(+EdgeCollection, +FromId, +ToId, +Properties)
    create_edge(EdgeCollection, FromId, ToId, Properties) :-
        ensure_connected,
        edge_collection(EdgeCollection),
        json_utils::edge_to_json(FromId, ToId, Properties, EdgeJson),
        build_collection_url(EdgeCollection, Url),
        http_client::post_json(Url, EdgeJson, Response),
        handle_response(Response, store, _).

    %% Delete an edge
    delete_edge(EdgeCollection, FromId, ToId) :-
        ensure_connected,
        format(atom(Aql), 'FOR e IN ~w FILTER e._from == @from AND e._to == @to REMOVE e IN ~w', [EdgeCollection, EdgeCollection]),
        execute_aql(Aql, [from=FromId, to=ToId], _).

    %% Get edges from a collection
    get_edges(EdgeCollection, Filter, Edges) :-
        ensure_connected,
        ( Filter = from(FromId) ->
            format(atom(Aql), 'FOR e IN ~w FILTER e._from == @from RETURN e', [EdgeCollection]),
            execute_aql(Aql, [from=FromId], Results)
        ; Filter = to(ToId) ->
            format(atom(Aql), 'FOR e IN ~w FILTER e._to == @to RETURN e', [EdgeCollection]),
            execute_aql(Aql, [to=ToId], Results)
        ; Filter = all ->
            format(atom(Aql), 'FOR e IN ~w RETURN e', [EdgeCollection]),
            execute_aql(Aql, Results)
        ; Results = []
        ),
        maplist(json_utils::json_to_edge, Results, Edges).

    %% ============================================================
    %% AQL QUERY EXECUTION
    %% ============================================================

    %% Execute an AQL query
    %% execute_aql(+Query, -Results)
    execute_aql(Query, Results) :-
        execute_aql(Query, [], Results).

    %% Execute an AQL query with bind variables
    %% execute_aql(+Query, +BindVars, -Results)
    execute_aql(Query, BindVars, Results) :-
        ensure_connected,
        build_aql_url(Url),
        ( is_list(BindVars) ->
            dict_create(BindDict, json, BindVars)
        ; BindDict = BindVars
        ),
        QueryDict = json{query: Query, bindVars: BindDict},
        http_client::post_json(Url, QueryDict, Response),
        ( http_client::is_success(Response) ->
            ( get_dict(result, Response, Results) -> true
            ; Results = []
            )
        ; http_client::get_error_message(Response, Msg),
          throw(aql_error(Query, Msg))
        ).

    %% ============================================================
    %% COLLECTION MANAGEMENT
    %% ============================================================

    %% Ensure all required collections exist
    ensure_collections :-
        ensure_connected,
        forall(document_collection(C), ensure_document_collection(C)),
        forall(edge_collection(C), ensure_edge_collection(C)).

    ensure_document_collection(Name) :-
        build_collection_url('', Url),
        JsonDict = json{name: Name, type: 2},  %% type 2 = document
        http_client::post_json(Url, JsonDict, Response),
        ( http_client::is_success(Response) -> true
        ; get_dict(errorNum, Response, 1207) -> true  %% Already exists
        ; http_client::get_error_message(Response, Msg),
          throw(collection_creation_failed(Name, Msg))
        ).

    ensure_edge_collection(Name) :-
        build_collection_url('', Url),
        JsonDict = json{name: Name, type: 3},  %% type 3 = edge
        http_client::post_json(Url, JsonDict, Response),
        ( http_client::is_success(Response) -> true
        ; get_dict(errorNum, Response, 1207) -> true  %% Already exists
        ; http_client::get_error_message(Response, Msg),
          throw(collection_creation_failed(Name, Msg))
        ).

    %% List all collections
    list_collections(Collections) :-
        ensure_connected,
        http_client::get_json('/_api/collection', Response),
        ( http_client::is_success(Response) ->
            get_dict(result, Response, CollectionList),
            findall(Name, (member(C, CollectionList), get_dict(name, C, Name)), Collections)
        ; Collections = []
        ).

    %% Get document count for a collection
    get_collection_count(Collection, Count) :-
        ensure_connected,
        format(atom(Url), '/_api/collection/~w/count', [Collection]),
        http_client::get_json(Url, Response),
        ( http_client::is_success(Response) ->
            get_dict(count, Response, Count)
        ; Count = 0
        ).

    %% ============================================================
    %% INTERNAL PREDICATES
    %% ============================================================

    %% Ensure connected or throw
    ensure_connected :-
        ( is_connected -> true
        ; throw(not_connected)
        ).

    %% Build document URL
    build_document_url(Collection, Key, Url) :-
        format(atom(Url), '/_api/document/~w/~w', [Collection, Key]).

    %% Build collection URL
    build_collection_url(Collection, Url) :-
        ( Collection = '' ->
            Url = '/_api/collection'
        ; format(atom(Url), '/_api/document/~w', [Collection])
        ).

    %% Build AQL cursor URL
    build_aql_url('/_api/cursor').

    %% Handle HTTP response
    handle_response(Response, Operation, Result) :-
        ( http_client::is_success(Response) ->
            ( Operation = store, get_dict('_key', Response, Result) -> true
            ; Result = ok
            )
        ; http_client::get_error_message(Response, Msg),
          throw(operation_failed(Operation, Msg))
        ).

    %% Ensure document has a _key field
    ensure_key(Dict, KeyedDict) :-
        ( get_dict('_key', Dict, _) ->
            KeyedDict = Dict
        ; json_utils::generate_key(Key),
          put_dict('_key', Dict, Key, KeyedDict)
        ).

    %% ============================================================
    %% QUERY BUILDERS
    %% ============================================================

    %% Build AQL query for findings
    build_findings_query(Criteria, Aql) :-
        ( is_list(Criteria) ->
            build_filter_clauses(Criteria, 'f', Filters),
            format(atom(Aql), 'FOR f IN findings ~w RETURN f', [Filters])
        ; atom(Criteria) ->
            format(atom(Aql), 'FOR f IN findings FILTER ~w RETURN f', [Criteria])
        ; Aql = 'FOR f IN findings RETURN f'
        ).

    %% Build AQL query for sessions
    build_sessions_query(Criteria, Aql) :-
        ( is_list(Criteria) ->
            build_filter_clauses(Criteria, 's', Filters),
            format(atom(Aql), 'FOR s IN sessions ~w RETURN s', [Filters])
        ; atom(Criteria) ->
            format(atom(Aql), 'FOR s IN sessions FILTER ~w RETURN s', [Criteria])
        ; Aql = 'FOR s IN sessions RETURN s'
        ).

    %% Build AQL query for repos
    build_repos_query(Criteria, Aql) :-
        ( is_list(Criteria) ->
            build_filter_clauses(Criteria, 'r', Filters),
            format(atom(Aql), 'FOR r IN repos ~w RETURN r', [Filters])
        ; atom(Criteria) ->
            format(atom(Aql), 'FOR r IN repos FILTER ~w RETURN r', [Criteria])
        ; Aql = 'FOR r IN repos RETURN r'
        ).

    %% Build AQL query for rules
    build_rules_query(Criteria, Aql) :-
        ( is_list(Criteria) ->
            build_filter_clauses(Criteria, 'r', Filters),
            format(atom(Aql), 'FOR r IN rules ~w RETURN r', [Filters])
        ; atom(Criteria) ->
            format(atom(Aql), 'FOR r IN rules FILTER ~w RETURN r', [Criteria])
        ; Aql = 'FOR r IN rules RETURN r'
        ).

    %% Build filter clauses from criteria list
    build_filter_clauses([], _, '').
    build_filter_clauses(Criteria, Var, FilterStr) :-
        Criteria \= [],
        findall(Clause,
            (member(Key=Value, Criteria),
             format(atom(Clause), '~w.~w == "~w"', [Var, Key, Value])),
            Clauses),
        ( Clauses = [] ->
            FilterStr = ''
        ; atomic_list_concat(Clauses, ' AND ', Combined),
          format(atom(FilterStr), 'FILTER ~w', [Combined])
        ).

:- end_object.

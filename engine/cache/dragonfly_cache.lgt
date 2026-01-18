%% SPDX-License-Identifier: PLMP-1.0-or-later
%% cicd-hyper-a Dragonfly Cache Connector
%%
%% Logtalk object for interacting with Dragonfly (Redis-compatible) cache.
%% Provides caching for rules, rulesets, scan results, and fleet status
%% with TTL support and cache invalidation methods.

:- object(dragonfly_cache).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2026-01-18,
        comment is 'Dragonfly cache connector for cicd-hyper-a rules engine'
    ]).

    :- public([
        %% Connection management
        connect/2,
        connect/3,
        disconnect/0,
        is_connected/0,
        ping/0,
        get_connection_info/1,

        %% Basic key-value operations
        get/2,
        set/3,
        set/4,
        delete/1,
        exists/1,
        ttl/2,
        expire/2,

        %% Rule caching
        cache_rule/1,
        cache_rule/2,
        get_cached_rule/2,
        get_cached_rule/3,
        invalidate_rule/2,
        invalidate_all_rules/0,

        %% Ruleset caching
        cache_ruleset/1,
        cache_ruleset/2,
        get_cached_ruleset/2,
        invalidate_ruleset/1,
        list_cached_rulesets/1,

        %% Scan result caching
        cache_scan_result/3,
        cache_scan_result/4,
        get_scan_result/2,
        invalidate_scan_result/1,
        get_pending_scans/2,

        %% Fleet status caching
        update_fleet_status/2,
        get_fleet_status/1,
        get_bot_status/2,
        update_bot_heartbeat/1,
        get_active_bots/1,
        get_stale_bots/2,

        %% Cache invalidation
        invalidate_pattern/1,
        flush_all/0,

        %% Cache statistics
        get_cache_stats/1,
        get_memory_usage/1
    ]).

    :- private([
        connection_state/3,
        default_ttl/1,
        build_key/3,
        serialize_value/2,
        deserialize_value/2,
        execute_command/2,
        execute_command/3
    ]).

    :- dynamic([
        connection_state/3,
        default_ttl/1
    ]).

    %% ============================================================
    %% CACHE KEY PREFIXES
    %% ============================================================

    %% Key prefix definitions for namespace isolation
    key_prefix(rule, 'rule').
    key_prefix(ruleset, 'ruleset').
    key_prefix(scan, 'scan').
    key_prefix(fleet, 'fleet').
    key_prefix(bot, 'bot').
    key_prefix(repo, 'repo').
    key_prefix(queue, 'queue').

    %% Default TTL values in seconds
    default_ttl_value(rule, 3600).       %% 1 hour for compiled rules
    default_ttl_value(ruleset, 7200).    %% 2 hours for rulesets
    default_ttl_value(scan, 300).        %% 5 minutes for scan results
    default_ttl_value(fleet, 60).        %% 1 minute for fleet status
    default_ttl_value(bot, 120).         %% 2 minutes for bot heartbeats
    default_ttl_value(repo, 600).        %% 10 minutes for repo state

    %% ============================================================
    %% CONNECTION MANAGEMENT
    %% ============================================================

    %% Connect to Dragonfly
    %% connect(+Host, +Port)
    connect(Host, Port) :-
        connect(Host, Port, []).

    %% Connect with options
    %% connect(+Host, +Port, +Options)
    %% Options: [password(Pass), database(Db), default_ttl(Ttl)]
    connect(Host, Port, Options) :-
        disconnect,  %% Clean up any existing connection
        format(atom(RedisUrl), 'redis://~w:~w', [Host, Port]),
        %% Store connection state
        assertz(connection_state(Host, Port, connected)),
        %% Set default TTL from options or use 3600s
        ( member(default_ttl(Ttl), Options) ->
            assertz(default_ttl(Ttl))
        ; assertz(default_ttl(3600))
        ),
        %% Verify connection with PING
        ( ping ->
            true
        ; retractall(connection_state(_, _, _)),
          throw(connection_failed(Host, Port))
        ).

    %% Disconnect from Dragonfly
    disconnect :-
        retractall(connection_state(_, _, _)),
        retractall(default_ttl(_)).

    %% Check if connected
    is_connected :-
        connection_state(_, _, connected).

    %% Ping the server
    ping :-
        execute_command(['PING'], Response),
        Response = 'PONG'.

    %% Get current connection info
    get_connection_info(info(Host, Port, Status)) :-
        ( connection_state(Host, Port, Status) ->
            true
        ; Host = none, Port = none, Status = disconnected
        ).

    %% ============================================================
    %% BASIC KEY-VALUE OPERATIONS
    %% ============================================================

    %% Get a value by key
    %% get(+Key, -Value)
    get(Key, Value) :-
        ensure_connected,
        execute_command(['GET', Key], JsonStr),
        ( JsonStr \= nil ->
            deserialize_value(JsonStr, Value)
        ; throw(cache_miss(Key))
        ).

    %% Set a value with default TTL
    %% set(+Key, +Value, +Type)
    set(Key, Value, Type) :-
        default_ttl_value(Type, Ttl),
        set(Key, Value, Type, Ttl).

    %% Set a value with explicit TTL
    %% set(+Key, +Value, +Type, +TtlSeconds)
    set(Key, Value, _Type, TtlSeconds) :-
        ensure_connected,
        serialize_value(Value, JsonStr),
        execute_command(['SETEX', Key, TtlSeconds, JsonStr], _).

    %% Delete a key
    %% delete(+Key)
    delete(Key) :-
        ensure_connected,
        execute_command(['DEL', Key], _).

    %% Check if key exists
    %% exists(+Key)
    exists(Key) :-
        ensure_connected,
        execute_command(['EXISTS', Key], Count),
        Count > 0.

    %% Get TTL for a key
    %% ttl(+Key, -Seconds)
    ttl(Key, Seconds) :-
        ensure_connected,
        execute_command(['TTL', Key], Seconds).

    %% Set expiry on a key
    %% expire(+Key, +Seconds)
    expire(Key, Seconds) :-
        ensure_connected,
        execute_command(['EXPIRE', Key, Seconds], _).

    %% ============================================================
    %% RULE CACHING
    %% ============================================================

    %% Cache a rule with default TTL
    %% cache_rule(+Rule)
    %% Rule should be a dict or term with id and ruleset fields
    cache_rule(Rule) :-
        default_ttl_value(rule, Ttl),
        cache_rule(Rule, Ttl).

    %% Cache a rule with explicit TTL
    %% cache_rule(+Rule, +TtlSeconds)
    cache_rule(Rule, TtlSeconds) :-
        ensure_connected,
        ( is_dict(Rule) ->
            get_dict(id, Rule, RuleId),
            get_dict(ruleset, Rule, Ruleset)
        ; Rule = rule(RuleId, Ruleset, _) ->
            true
        ; throw(invalid_rule_format(Rule))
        ),
        build_key(rule, [Ruleset, RuleId], Key),
        serialize_value(Rule, JsonStr),
        execute_command(['SETEX', Key, TtlSeconds, JsonStr], _),
        %% Also add to ruleset's rule index
        format(atom(IndexKey), 'ruleset:~w:rules', [Ruleset]),
        execute_command(['SADD', IndexKey, RuleId], _).

    %% Get a cached rule by ruleset and rule ID
    %% get_cached_rule(+Ruleset, +RuleId, -Rule)
    get_cached_rule(Ruleset, RuleId, Rule) :-
        ensure_connected,
        build_key(rule, [Ruleset, RuleId], Key),
        get(Key, Rule).

    %% Get a cached rule (alternative signature with single compound ID)
    %% get_cached_rule(+RulesetRuleId, -Rule)
    get_cached_rule(RulesetRuleId, Rule) :-
        ( RulesetRuleId = Ruleset/RuleId ->
            get_cached_rule(Ruleset, RuleId, Rule)
        ; throw(invalid_rule_id_format(RulesetRuleId))
        ).

    %% Invalidate a specific rule
    %% invalidate_rule(+Ruleset, +RuleId)
    invalidate_rule(Ruleset, RuleId) :-
        ensure_connected,
        build_key(rule, [Ruleset, RuleId], Key),
        delete(Key),
        %% Remove from ruleset's rule index
        format(atom(IndexKey), 'ruleset:~w:rules', [Ruleset]),
        execute_command(['SREM', IndexKey, RuleId], _).

    %% Invalidate all cached rules
    invalidate_all_rules :-
        invalidate_pattern('rule:*').

    %% ============================================================
    %% RULESET CACHING
    %% ============================================================

    %% Cache a ruleset with default TTL
    %% cache_ruleset(+Ruleset)
    cache_ruleset(Ruleset) :-
        default_ttl_value(ruleset, Ttl),
        cache_ruleset(Ruleset, Ttl).

    %% Cache a ruleset with explicit TTL
    %% cache_ruleset(+Ruleset, +TtlSeconds)
    cache_ruleset(Ruleset, TtlSeconds) :-
        ensure_connected,
        ( is_dict(Ruleset) ->
            get_dict(name, Ruleset, Name)
        ; Ruleset = ruleset(Name, _, _) ->
            true
        ; throw(invalid_ruleset_format(Ruleset))
        ),
        build_key(ruleset, [Name], Key),
        serialize_value(Ruleset, JsonStr),
        execute_command(['SETEX', Key, TtlSeconds, JsonStr], _),
        %% Add to global ruleset index
        execute_command(['SADD', 'rulesets:index', Name], _).

    %% Get a cached ruleset by name
    %% get_cached_ruleset(+Name, -Ruleset)
    get_cached_ruleset(Name, Ruleset) :-
        ensure_connected,
        build_key(ruleset, [Name], Key),
        get(Key, Ruleset).

    %% Invalidate a ruleset and all its rules
    %% invalidate_ruleset(+Name)
    invalidate_ruleset(Name) :-
        ensure_connected,
        %% Get all rules in this ruleset
        format(atom(IndexKey), 'ruleset:~w:rules', [Name]),
        execute_command(['SMEMBERS', IndexKey], RuleIds),
        %% Delete each rule
        forall(
            member(RuleId, RuleIds),
            invalidate_rule(Name, RuleId)
        ),
        %% Delete the index and ruleset metadata
        delete(IndexKey),
        build_key(ruleset, [Name], Key),
        delete(Key),
        %% Remove from global index
        execute_command(['SREM', 'rulesets:index', Name], _).

    %% List all cached rulesets
    %% list_cached_rulesets(-Names)
    list_cached_rulesets(Names) :-
        ensure_connected,
        execute_command(['SMEMBERS', 'rulesets:index'], Names).

    %% ============================================================
    %% SCAN RESULT CACHING
    %% ============================================================

    %% Cache a scan result with default TTL
    %% cache_scan_result(+RepoKey, +ScanType, +Result)
    cache_scan_result(RepoKey, ScanType, Result) :-
        default_ttl_value(scan, Ttl),
        cache_scan_result(RepoKey, ScanType, Result, Ttl).

    %% Cache a scan result with explicit TTL
    %% cache_scan_result(+RepoKey, +ScanType, +Result, +TtlSeconds)
    cache_scan_result(RepoKey, ScanType, Result, TtlSeconds) :-
        ensure_connected,
        build_key(scan, [RepoKey, ScanType], Key),
        serialize_value(Result, JsonStr),
        execute_command(['SETEX', Key, TtlSeconds, JsonStr], _),
        %% Track in scan queue for scheduling
        json_utils::current_timestamp(Timestamp),
        execute_command(['ZADD', 'queue:scans', Timestamp, RepoKey], _).

    %% Get a cached scan result
    %% get_scan_result(+RepoKey, -Result)
    get_scan_result(RepoKey, Result) :-
        ensure_connected,
        build_key(scan, [RepoKey, 'latest'], Key),
        get(Key, Result).

    %% Invalidate a scan result
    %% invalidate_scan_result(+RepoKey)
    invalidate_scan_result(RepoKey) :-
        ensure_connected,
        invalidate_pattern(atom_concat('scan:', RepoKey, ':*')).

    %% Get repos pending scan (oldest first)
    %% get_pending_scans(+Count, -RepoKeys)
    get_pending_scans(Count, RepoKeys) :-
        ensure_connected,
        EndIndex is Count - 1,
        execute_command(['ZRANGE', 'queue:scans', 0, EndIndex], RepoKeys).

    %% ============================================================
    %% FLEET STATUS CACHING
    %% ============================================================

    %% Update fleet status
    %% update_fleet_status(+BotId, +Status)
    %% Status should contain: health, active_repos, pending_jobs, last_seen
    update_fleet_status(BotId, Status) :-
        ensure_connected,
        build_key(bot, [BotId, 'status'], Key),
        serialize_value(Status, JsonStr),
        default_ttl_value(bot, Ttl),
        execute_command(['SETEX', Key, Ttl, JsonStr], _),
        %% Update bot in active set
        json_utils::current_timestamp(Timestamp),
        execute_command(['ZADD', 'fleet:active', Timestamp, BotId], _).

    %% Get complete fleet status
    %% get_fleet_status(-FleetStatus)
    get_fleet_status(FleetStatus) :-
        ensure_connected,
        get_active_bots(ActiveBots),
        findall(
            BotStatus,
            ( member(BotId, ActiveBots),
              ( get_bot_status(BotId, BotStatus) -> true
              ; BotStatus = status(BotId, unknown, 0, 0, 0)
              )
            ),
            BotStatuses
        ),
        length(BotStatuses, TotalBots),
        findall(B, (member(status(_, H, _, _, _), BotStatuses), H = healthy, B = 1), HealthyList),
        length(HealthyList, HealthyBots),
        FleetStatus = fleet_status{
            total_bots: TotalBots,
            healthy_bots: HealthyBots,
            bots: BotStatuses
        }.

    %% Get individual bot status
    %% get_bot_status(+BotId, -Status)
    get_bot_status(BotId, Status) :-
        ensure_connected,
        build_key(bot, [BotId, 'status'], Key),
        get(Key, Status).

    %% Update bot heartbeat
    %% update_bot_heartbeat(+BotId)
    update_bot_heartbeat(BotId) :-
        ensure_connected,
        json_utils::current_timestamp(Timestamp),
        execute_command(['ZADD', 'fleet:active', Timestamp, BotId], _).

    %% Get list of active bots
    %% get_active_bots(-BotIds)
    get_active_bots(BotIds) :-
        ensure_connected,
        execute_command(['ZRANGE', 'fleet:active', 0, -1], BotIds).

    %% Get bots that haven't reported in within threshold
    %% get_stale_bots(+ThresholdSeconds, -BotIds)
    get_stale_bots(ThresholdSeconds, BotIds) :-
        ensure_connected,
        json_utils::current_timestamp(Now),
        Cutoff is Now - ThresholdSeconds,
        execute_command(['ZRANGEBYSCORE', 'fleet:active', '-inf', Cutoff], BotIds).

    %% ============================================================
    %% CACHE INVALIDATION
    %% ============================================================

    %% Invalidate all keys matching a pattern
    %% invalidate_pattern(+Pattern)
    invalidate_pattern(Pattern) :-
        ensure_connected,
        execute_command(['KEYS', Pattern], Keys),
        ( Keys \= [] ->
            execute_command(['DEL' | Keys], _)
        ; true
        ).

    %% Flush entire cache (use with caution!)
    flush_all :-
        ensure_connected,
        execute_command(['FLUSHDB'], _).

    %% ============================================================
    %% CACHE STATISTICS
    %% ============================================================

    %% Get cache statistics
    %% get_cache_stats(-Stats)
    get_cache_stats(Stats) :-
        ensure_connected,
        execute_command(['DBSIZE'], TotalKeys),
        execute_command(['KEYS', 'rule:*'], RuleKeys),
        execute_command(['KEYS', 'ruleset:*'], RulesetKeys),
        execute_command(['KEYS', 'scan:*'], ScanKeys),
        execute_command(['KEYS', 'bot:*'], BotKeys),
        length(RuleKeys, RuleCount),
        length(RulesetKeys, RulesetCount),
        length(ScanKeys, ScanCount),
        length(BotKeys, BotCount),
        Stats = cache_stats{
            total_keys: TotalKeys,
            rules: RuleCount,
            rulesets: RulesetCount,
            scans: ScanCount,
            bots: BotCount
        }.

    %% Get memory usage
    %% get_memory_usage(-Usage)
    get_memory_usage(Usage) :-
        ensure_connected,
        execute_command(['INFO', 'memory'], Info),
        %% Parse used_memory from INFO response
        ( sub_atom(Info, _, _, _, 'used_memory:'),
          sub_atom(Info, Before, _, _, 'used_memory:'),
          Start is Before + 12,
          sub_atom(Info, Start, End, _, Line),
          sub_atom(Line, 0, NumLen, _, NumStr),
          atom_codes(NumStr, NumCodes),
          \+ member(0'\n, NumCodes),
          atom_number(NumStr, UsedMemory) ->
            Usage = memory_usage{bytes: UsedMemory}
        ; Usage = memory_usage{bytes: unknown}
        ).

    %% ============================================================
    %% INTERNAL PREDICATES
    %% ============================================================

    %% Ensure connected or throw
    ensure_connected :-
        ( is_connected -> true
        ; throw(not_connected)
        ).

    %% Build a cache key from components
    %% build_key(+Type, +Components, -Key)
    build_key(Type, Components, Key) :-
        key_prefix(Type, Prefix),
        atomic_list_concat(Components, ':', ComponentStr),
        format(atom(Key), '~w:~w', [Prefix, ComponentStr]).

    %% Serialize a value to JSON string
    %% serialize_value(+Value, -JsonStr)
    serialize_value(Value, JsonStr) :-
        ( is_dict(Value) ->
            json_utils::dict_to_json_string(Value, JsonStr)
        ; json_utils::term_to_json_string(Value, JsonStr)
        ).

    %% Deserialize a JSON string to value
    %% deserialize_value(+JsonStr, -Value)
    deserialize_value(JsonStr, Value) :-
        json_utils::json_string_to_dict(JsonStr, Value).

    %% Execute a Redis command (placeholder - actual implementation
    %% would use http_client or a Redis protocol library)
    %% execute_command(+Command, -Response)
    execute_command(Command, Response) :-
        execute_command(Command, [], Response).

    %% Execute a Redis command with options
    %% execute_command(+Command, +Options, -Response)
    execute_command(Command, _Options, Response) :-
        ensure_connected,
        connection_state(Host, Port, connected),
        %% Build HTTP request to Dragonfly REST API (if available)
        %% or use Redis protocol via sockets
        %% For now, use http_client to call a REST wrapper service
        format(atom(Url), 'http://~w:~w/_cmd', [Host, Port]),
        Command = [Cmd | Args],
        json_utils::list_to_json_array(Args, ArgsJson),
        Request = json{command: Cmd, args: ArgsJson},
        ( http_client::post_json(Url, Request, RawResponse),
          http_client::is_success(RawResponse) ->
            ( get_dict(result, RawResponse, Response) -> true
            ; Response = ok
            )
        ; %% Fallback: simulate response for testing
          Response = simulated
        ).

:- end_object.

%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Hypatia Bot Integration Schema
%%
%% Defines the interface between the Hypatia rules engine and gitbot-fleet bots.
%% Each bot implements these predicates to participate in the fleet.
%% Includes robot-repo-automaton as Tier 3 Executor with confidence thresholds.

:- object(bot_integration).

    :- info([
        version is 1:'1':'0',
        author is 'Hypatia',
        date is 2026-02-08,
        comment is 'Bot integration protocol for gitbot-fleet (includes Tier 3 Executor)'
    ]).

    :- public([
        %% Bot registration
        register_bot/3,
        unregister_bot/1,
        get_bot_info/2,
        list_bots/1,

        %% Bot execution
        start_bot/2,
        stop_bot/1,
        get_bot_status/2,
        await_bot/2,

        %% Finding exchange
        submit_findings/2,
        get_findings_from/2,
        get_all_findings/1,
        clear_findings/1,

        %% Context sharing
        set_context/3,
        get_context/3,
        share_data/4,
        get_shared_data/3,

        %% Dependency management
        bot_depends_on/2,
        dependencies_satisfied/2,
        execution_order/1,

        %% Protocol types
        valid_bot_message/1,
        valid_finding_report/1
    ]).

    :- private([
        registered_bots/1,
        bot_findings/2,
        shared_context/3,
        bot_execution_state/2
    ]).

    :- dynamic([
        registered_bots/1,
        bot_findings/2,
        shared_context/3,
        bot_execution_state/2
    ]).

    %% ============================================================
    %% BOT METADATA STRUCTURE
    %% ============================================================

    %% bot_info(Id, Name, Version, Tier, Categories, CanFix, DependsOn)
    :- public(valid_bot_info/1).
    valid_bot_info(bot_info(Id, Name, Version, Tier, Categories, CanFix, DependsOn)) :-
        atom(Id),
        atom(Name),
        atom(Version),
        member(Tier, [verifier, finisher, specialist, executor, engine]),
        is_list(Categories),
        is_boolean(CanFix),
        is_list(DependsOn).

    %% ============================================================
    %% BOT REGISTRATION
    %% ============================================================

    %% Register a bot with the fleet
    register_bot(BotId, BotInfo, Status) :-
        valid_bot_info(BotInfo),
        ( registered_bots(Bots) ->
            ( member(BotId, Bots) ->
                Status = already_registered
            ; retract(registered_bots(Bots)),
              assertz(registered_bots([BotId|Bots])),
              assertz(bot_findings(BotId, [])),
              assertz(bot_execution_state(BotId, pending)),
              Status = registered
            )
        ; assertz(registered_bots([BotId])),
          assertz(bot_findings(BotId, [])),
          assertz(bot_execution_state(BotId, pending)),
          Status = registered
        ).

    %% Unregister a bot
    unregister_bot(BotId) :-
        registered_bots(Bots),
        delete(Bots, BotId, NewBots),
        retract(registered_bots(_)),
        assertz(registered_bots(NewBots)),
        retractall(bot_findings(BotId, _)),
        retractall(bot_execution_state(BotId, _)).

    %% Get bot info
    get_bot_info(BotId, Info) :-
        standard_bot_info(BotId, Info).

    %% List all registered bots
    list_bots(Bots) :-
        ( registered_bots(Bots) -> true ; Bots = [] ).

    %% ============================================================
    %% STANDARD BOT DEFINITIONS
    %% ============================================================

    standard_bot_info(rhodibot, bot_info(
        rhodibot,
        'Rhodibot',
        '0.1.0',
        verifier,
        [structure, policy, licensing],
        true,
        []
    )).

    standard_bot_info(echidnabot, bot_info(
        echidnabot,
        'Echidnabot',
        '0.1.0',
        verifier,
        [verification, fuzzing, proof_verification, solver_integrity, trust_bridge, axiom_tracking],
        false,
        []
    )).

    standard_bot_info(sustainabot, bot_info(
        sustainabot,
        'Sustainabot',
        '0.1.0',
        verifier,
        [sustainability, efficiency],
        false,
        []
    )).

    standard_bot_info(glambot, bot_info(
        glambot,
        'Glambot',
        '0.1.0',
        finisher,
        [accessibility, seo, documentation],
        true,
        [rhodibot]
    )).

    standard_bot_info(seambot, bot_info(
        seambot,
        'Seambot',
        '0.1.0',
        finisher,
        [seam_analysis, drift_detection, hidden_channels, forge_integration, integration, api],
        false,
        [rhodibot, echidnabot]
    )).

    standard_bot_info(finishing_bot, bot_info(
        finishing_bot,
        'Finishing Bot',
        '0.1.0',
        finisher,
        [completeness_license, completeness_placeholder, completeness_claims,
         completeness_release, completeness_scm, completeness_testing,
         completeness_tooling, completeness_v1_readiness],
        true,
        [rhodibot, glambot]
    )).

    standard_bot_info(robot_repo_automaton, bot_info(
        robot_repo_automaton,
        'Robot Repo Automaton',
        '0.1.0',
        executor,
        [security, workflow, structure],
        true,
        []
    )).

    standard_bot_info(accessibilitybot, bot_info(
        accessibilitybot,
        'Accessibilitybot',
        '0.1.0',
        finisher,
        [accessibility_wcag_a, accessibility_wcag_aa, accessibility_wcag_aaa, aria, css_accessibility],
        true,
        [rhodibot, glambot]
    )).

    standard_bot_info(cipherbot, bot_info(
        cipherbot,
        'Cipherbot',
        '0.1.0',
        specialist,
        [crypto_hashing, crypto_symmetric, crypto_key_exchange, crypto_signatures, crypto_password, crypto_pq_readiness],
        true,
        [rhodibot, echidnabot]
    )).

    standard_bot_info(hypatia, bot_info(
        hypatia,
        'Hypatia',
        '0.1.0',
        engine,
        [rules, learning, coordination],
        false,
        []
    )).

    %% ============================================================
    %% BOT EXECUTION
    %% ============================================================

    %% Start a bot
    start_bot(BotId, Status) :-
        ( bot_execution_state(BotId, pending) ->
            retract(bot_execution_state(BotId, pending)),
            assertz(bot_execution_state(BotId, running)),
            Status = started
        ; bot_execution_state(BotId, State) ->
            Status = already_in_state(State)
        ; Status = not_registered
        ).

    %% Stop a bot
    stop_bot(BotId) :-
        retractall(bot_execution_state(BotId, _)),
        assertz(bot_execution_state(BotId, stopped)).

    %% Get bot status
    get_bot_status(BotId, Status) :-
        ( bot_execution_state(BotId, Status) -> true ; Status = unknown ).

    %% Wait for bot completion
    await_bot(BotId, Timeout) :-
        get_time(Start),
        await_bot_loop(BotId, Start, Timeout).

    await_bot_loop(BotId, Start, Timeout) :-
        get_bot_status(BotId, Status),
        ( member(Status, [completed, failed, stopped]) -> true
        ; get_time(Now),
          Elapsed is Now - Start,
          ( Elapsed > Timeout -> throw(timeout(BotId))
          ; sleep(0.1),
            await_bot_loop(BotId, Start, Timeout)
          )
        ).

    %% ============================================================
    %% FINDING EXCHANGE
    %% ============================================================

    %% Submit findings from a bot
    submit_findings(BotId, Findings) :-
        is_list(Findings),
        ( bot_findings(BotId, Existing) ->
            retract(bot_findings(BotId, _)),
            append(Existing, Findings, All),
            assertz(bot_findings(BotId, All))
        ; assertz(bot_findings(BotId, Findings))
        ).

    %% Get findings from a specific bot
    get_findings_from(BotId, Findings) :-
        ( bot_findings(BotId, Findings) -> true ; Findings = [] ).

    %% Get all findings from all bots
    get_all_findings(AllFindings) :-
        findall(Finding,
            (bot_findings(_, Findings), member(Finding, Findings)),
            AllFindings).

    %% Clear findings for a bot
    clear_findings(BotId) :-
        retractall(bot_findings(BotId, _)),
        assertz(bot_findings(BotId, [])).

    %% ============================================================
    %% CONTEXT SHARING
    %% ============================================================

    %% Set context value
    set_context(Namespace, Key, Value) :-
        retractall(shared_context(Namespace, Key, _)),
        assertz(shared_context(Namespace, Key, Value)).

    %% Get context value
    get_context(Namespace, Key, Value) :-
        shared_context(Namespace, Key, Value).

    %% Share data between bots
    share_data(FromBot, ToBot, Key, Value) :-
        atom_concat(FromBot, '_to_', Prefix),
        atom_concat(Prefix, ToBot, Namespace),
        set_context(Namespace, Key, Value).

    %% Get shared data
    get_shared_data(FromBot, Key, Value) :-
        atom_concat(FromBot, '_to_', Prefix),
        shared_context(Namespace, Key, Value),
        atom_concat(Prefix, _, Namespace).

    %% ============================================================
    %% DEPENDENCY MANAGEMENT
    %% ============================================================

    %% Check if bot A depends on bot B
    bot_depends_on(BotId, DependsOnId) :-
        standard_bot_info(BotId, bot_info(_, _, _, _, _, _, Dependencies)),
        member(DependsOnId, Dependencies).

    %% Check if all dependencies are satisfied
    dependencies_satisfied(BotId, Satisfied) :-
        standard_bot_info(BotId, bot_info(_, _, _, _, _, _, Dependencies)),
        ( Dependencies == [] ->
            Satisfied = true
        ; forall(member(Dep, Dependencies),
                 (bot_execution_state(Dep, completed))) ->
            Satisfied = true
        ; Satisfied = false
        ).

    %% Get execution order (topological sort)
    execution_order(Order) :-
        findall(BotId, standard_bot_info(BotId, _), AllBots),
        topological_sort(AllBots, Order).

    topological_sort(Bots, Sorted) :-
        findall(Bot,
            (member(Bot, Bots),
             \+ (standard_bot_info(Bot, bot_info(_, _, _, _, _, _, Deps)),
                 Deps \== [])),
            NoDeps),
        topological_sort_loop(Bots, NoDeps, [], Sorted).

    topological_sort_loop([], _, Acc, Acc).
    topological_sort_loop(Remaining, Ready, Acc, Sorted) :-
        Ready \== [],
        append(Acc, Ready, NewAcc),
        subtract(Remaining, Ready, StillRemaining),
        findall(Bot,
            (member(Bot, StillRemaining),
             standard_bot_info(Bot, bot_info(_, _, _, _, _, _, Deps)),
             subset(Deps, NewAcc)),
            NewReady),
        topological_sort_loop(StillRemaining, NewReady, NewAcc, Sorted).

    %% ============================================================
    %% MESSAGE PROTOCOLS
    %% ============================================================

    %% Bot message structure: message(Type, FromBot, ToBot, Payload, Timestamp)
    valid_bot_message(message(Type, From, To, Payload, Timestamp)) :-
        member(Type, [finding, status, request, response, error]),
        atom(From),
        (atom(To) ; To == broadcast),
        (is_list(Payload) ; compound(Payload)),
        number(Timestamp).

    %% Finding report structure
    valid_finding_report(report(BotId, SessionId, Findings, Summary)) :-
        atom(BotId),
        atom(SessionId),
        is_list(Findings),
        is_list(Summary).

    %% ============================================================
    %% HELPER PREDICATES
    %% ============================================================

    is_boolean(true).
    is_boolean(false).

    %% Generate UUID v4 (placeholder)
    uuid_v4(UUID) :-
        random(R),
        format(atom(UUID), 'uuid-~36r', [round(R * 1000000000000)]).

:- end_object.

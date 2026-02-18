%% SPDX-License-Identifier: PMPL-1.0-or-later
%% cicd-hyper-a Engine Loader

:- initialization((
    %% Load schema definitions first
    logtalk_load([
        schema(rule_schema),
        schema(bot_integration)
    ], []),

    %% Load rules engine
    logtalk_load([
        rules(cicd_rules),
        rules(rule_distiller),
        rules(forge_adapters),
        rules(learning)
    ], []),

    %% Load ArangoDB connector and utilities
    logtalk_load([
        json_utils,
        http_client,
        arangodb_connector
    ], []),

    %% Load cache layer
    logtalk_load([
        cache(dragonfly_cache)
    ], []),

    write('cicd-hyper-a rule engine loaded.\n'),
    write('Available objects:\n'),
    write('  Rules:     cicd_rules, rule_distiller, forge_adapters, learning\n'),
    write('  Schema:    rule_schema, bot_integration\n'),
    write('  Database:  arangodb_connector, http_client, json_utils\n'),
    write('  Cache:     dragonfly_cache\n'),

    % Auto-load saved knowledge if available
    ( catch(learning::load_knowledge('knowledge.pl'), _, true) ->
        write('Loaded saved knowledge base.\n')
    ; write('Starting with fresh knowledge base.\n')
    ),

    % Bootstrap from training data if learning is empty
    ( learning::get_learning_stats(Stats),
      Stats.total_patterns =:= 0 ->
        write('Bootstrapping from training data...\n'),
        ( catch(rule_distiller::load_training_data('../.audittraining/training-data.pl'), _, fail) ->
            rule_distiller::distill_rules,
            write('Training data loaded and rules distilled.\n')
        ; write('No training data found, starting empty.\n')
        )
    ; true
    )
)).

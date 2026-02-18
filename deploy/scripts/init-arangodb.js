// SPDX-License-Identifier: PMPL-1.0-or-later
// cicd-hyper-a ArangoDB Initialization Script for arangosh
//
// Run with: arangosh --javascript.execute init-arangodb.js
//
// Environment Variables:
//   ARANGO_DATABASE     - Database name (default: cicd_hyper_a)
//   ARANGO_DROP_EXISTING - Drop existing collections (default: false)
//   ARANGO_SKIP_SEED    - Skip seeding data (default: false)
//   ARANGO_DRY_RUN      - Show what would be done (default: false)

'use strict';

const db = require('@arangodb').db;
const graph_module = require('@arangodb/general-graph');
const analyzers = require('@arangodb/analyzers');

// =============================================================================
// CONFIGURATION
// =============================================================================

const DATABASE = process.env.ARANGO_DATABASE || 'cicd_hyper_a';
const DROP_EXISTING = process.env.ARANGO_DROP_EXISTING === 'true';
const SKIP_SEED = process.env.ARANGO_SKIP_SEED === 'true';
const DRY_RUN = process.env.ARANGO_DRY_RUN === 'true';

const CONFIG = {
    // Document collections with their schemas
    documentCollections: [
        'repos',
        'bots',
        'sessions',
        'findings',
        'rules',
        'fixes',
        'workflows',
        'learningData',
        'owners',
        'rulesets',
        'bot_results',
        'alerts'
    ],

    // Edge collections
    edgeCollections: [
        'repo_has_workflow',
        'repo_owned_by',
        'session_scans_repo',
        'session_ran_bot',
        'bot_found_finding',
        'finding_in_session',
        'finding_triggered_by',
        'finding_in_file',
        'finding_fixed_by',
        'fix_applies_rule',
        'bot_owns_rule',
        'bot_depends_on',
        'rule_learned_from',
        'finding_related_to',
        'workflow_triggers_rule',
        'repo_similar_to',
        'rule_applies_to',
        'alert_in_repo',
        'bot_analyzed',
        'ruleset_contains',
        'repo_uses_ruleset'
    ],

    // Graph definitions
    graphs: {
        cicd_intelligence: {
            edgeDefinitions: [
                { collection: 'repo_has_workflow', from: ['repos'], to: ['workflows'] },
                { collection: 'repo_owned_by', from: ['repos'], to: ['owners'] },
                { collection: 'session_scans_repo', from: ['sessions'], to: ['repos'] },
                { collection: 'session_ran_bot', from: ['sessions'], to: ['bots'] },
                { collection: 'bot_found_finding', from: ['bots'], to: ['findings'] },
                { collection: 'finding_in_session', from: ['findings'], to: ['sessions'] },
                { collection: 'finding_triggered_by', from: ['findings'], to: ['rules'] },
                { collection: 'finding_in_file', from: ['findings'], to: ['workflows'] },
                { collection: 'finding_fixed_by', from: ['findings'], to: ['fixes'] },
                { collection: 'fix_applies_rule', from: ['fixes'], to: ['rules'] },
                { collection: 'bot_owns_rule', from: ['bots'], to: ['rules'] },
                { collection: 'workflow_triggers_rule', from: ['workflows'], to: ['rules'] }
            ]
        },
        bot_fleet: {
            edgeDefinitions: [
                { collection: 'bot_depends_on', from: ['bots'], to: ['bots'] },
                { collection: 'session_ran_bot', from: ['sessions'], to: ['bots'] },
                { collection: 'bot_owns_rule', from: ['bots'], to: ['rules'] }
            ]
        },
        learning_graph: {
            edgeDefinitions: [
                { collection: 'rule_learned_from', from: ['rules'], to: ['learningData'] },
                { collection: 'finding_related_to', from: ['findings'], to: ['findings'] },
                { collection: 'repo_similar_to', from: ['repos'], to: ['repos'] }
            ]
        }
    },

    // Indexes per collection
    indexes: {
        repos: [
            { type: 'persistent', fields: ['forge', 'owner'] },
            { type: 'persistent', fields: ['scorecardScore'] },
            { type: 'persistent', fields: ['lastScanned'] }
        ],
        findings: [
            { type: 'persistent', fields: ['severity'] },
            { type: 'persistent', fields: ['category'] },
            { type: 'persistent', fields: ['sessionId'] },
            { type: 'persistent', fields: ['botId'] },
            { type: 'persistent', fields: ['fixed'] },
            { type: 'persistent', fields: ['createdAt'] }
        ],
        sessions: [
            { type: 'persistent', fields: ['repoKey'] },
            { type: 'persistent', fields: ['status'] },
            { type: 'persistent', fields: ['startedAt'] }
        ],
        rules: [
            { type: 'persistent', fields: ['type'] },
            { type: 'persistent', fields: ['category'] },
            { type: 'persistent', fields: ['enabled'] },
            { type: 'persistent', fields: ['effect'] },
            { type: 'persistent', fields: ['triggerCount'] }
        ],
        workflows: [
            { type: 'persistent', fields: ['repoKey'] },
            { type: 'persistent', fields: ['allActionsPinned'] }
        ],
        rulesets: [
            { type: 'persistent', fields: ['name'], unique: true },
            { type: 'persistent', fields: ['effect'] },
            { type: 'persistent', fields: ['verified'] }
        ],
        bot_results: [
            { type: 'persistent', fields: ['botId', 'repoKey'] },
            { type: 'persistent', fields: ['runAt'] }
        ],
        alerts: [
            { type: 'persistent', fields: ['repoKey'] },
            { type: 'persistent', fields: ['severity'] },
            { type: 'persistent', fields: ['category'] },
            { type: 'persistent', fields: ['fixApplied'] },
            { type: 'persistent', fields: ['createdAt'] }
        ]
    },

    // Seed data - gitbot-fleet bots
    seedBots: [
        {
            _key: 'rhodibot',
            name: 'rhodibot',
            displayName: 'Rhodibot',
            version: '0.1.0',
            tier: 'verifier',
            categories: ['structure', 'policy', 'licensing'],
            canFix: true,
            enabled: true
        },
        {
            _key: 'echidnabot',
            name: 'echidnabot',
            displayName: 'Echidnabot',
            version: '0.1.0',
            tier: 'verifier',
            categories: ['verification', 'fuzzing'],
            canFix: false,
            enabled: true
        },
        {
            _key: 'oikos',
            name: 'oikos',
            displayName: 'Oikos',
            version: '0.1.0',
            tier: 'verifier',
            categories: ['sustainability', 'efficiency'],
            canFix: false,
            enabled: true
        },
        {
            _key: 'glambot',
            name: 'glambot',
            displayName: 'Glambot',
            version: '0.1.0',
            tier: 'finisher',
            categories: ['accessibility', 'seo', 'documentation'],
            canFix: true,
            enabled: true
        },
        {
            _key: 'seambot',
            name: 'seambot',
            displayName: 'Seambot',
            version: '0.1.0',
            tier: 'finisher',
            categories: ['integration', 'api'],
            canFix: false,
            enabled: true
        },
        {
            _key: 'finishing_bot',
            name: 'finishing_bot',
            displayName: 'Finishing Bot',
            version: '0.1.0',
            tier: 'finisher',
            categories: ['release', 'licensing', 'code_quality'],
            canFix: true,
            enabled: true
        },
        {
            _key: 'robot_repo_automaton',
            name: 'robot_repo_automaton',
            displayName: 'Robot Repo Automaton',
            version: '0.1.0',
            tier: 'executor',
            categories: ['security', 'workflow', 'structure'],
            canFix: true,
            enabled: true
        },
        {
            _key: 'cicd_hyper_a',
            name: 'cicd_hyper_a',
            displayName: 'CICD Hyper-A',
            version: '0.1.0',
            tier: 'engine',
            categories: ['all'],
            canFix: false,
            enabled: true
        }
    ],

    // Seed data - bot dependencies
    seedDependencies: [
        { _from: 'bots/glambot', _to: 'bots/rhodibot', dependencyType: 'required' },
        { _from: 'bots/seambot', _to: 'bots/rhodibot', dependencyType: 'required' },
        { _from: 'bots/seambot', _to: 'bots/echidnabot', dependencyType: 'required' },
        { _from: 'bots/finishing_bot', _to: 'bots/rhodibot', dependencyType: 'required' },
        { _from: 'bots/finishing_bot', _to: 'bots/glambot', dependencyType: 'required' }
    ],

    // Seed data - default rulesets
    seedRulesets: [
        {
            _key: 'openssf_scorecard',
            name: 'OpenSSF Scorecard',
            description: 'Rules for OpenSSF Scorecard compliance',
            version: '1.0.0',
            effect: 'diagnostic',
            verified: true,
            enabled: true
        },
        {
            _key: 'rhodium_standard',
            name: 'Rhodium Standard',
            description: 'RSR repository structure and policy rules',
            version: '1.0.0',
            effect: 'preventive',
            verified: true,
            enabled: true
        },
        {
            _key: 'github_security',
            name: 'GitHub Security',
            description: 'GitHub Actions security best practices',
            version: '1.0.0',
            effect: 'curative',
            verified: true,
            enabled: true
        }
    ]
};

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

function log(message) {
    const timestamp = new Date().toISOString();
    console.log(`[${timestamp}] ${message}`);
}

function logDryRun(message) {
    if (DRY_RUN) {
        console.log(`[DRY-RUN] ${message}`);
    }
}

function safeExecute(fn, description) {
    if (DRY_RUN) {
        logDryRun(description);
        return null;
    }
    try {
        return fn();
    } catch (e) {
        log(`Warning: ${description} - ${e.message}`);
        return null;
    }
}

// =============================================================================
// SETUP FUNCTIONS
// =============================================================================

function setupDatabase() {
    log(`Setting up database: ${DATABASE}`);

    const databases = db._databases();
    if (!databases.includes(DATABASE)) {
        safeExecute(() => {
            db._createDatabase(DATABASE);
        }, `Creating database ${DATABASE}`);
        log(`Created database: ${DATABASE}`);
    } else {
        log(`Database already exists: ${DATABASE}`);
    }

    if (!DRY_RUN) {
        db._useDatabase(DATABASE);
    }
    log(`Using database: ${DATABASE}`);
}

function setupCollections() {
    log('Setting up collections...');

    // Create document collections
    for (const name of CONFIG.documentCollections) {
        if (DROP_EXISTING && db._collection(name)) {
            safeExecute(() => db._drop(name), `Dropping collection ${name}`);
            log(`Dropped collection: ${name}`);
        }

        if (!db._collection(name)) {
            safeExecute(() => db._create(name), `Creating document collection ${name}`);
            log(`Created document collection: ${name}`);
        }
    }

    // Create edge collections
    for (const name of CONFIG.edgeCollections) {
        if (DROP_EXISTING && db._collection(name)) {
            safeExecute(() => db._drop(name), `Dropping edge collection ${name}`);
            log(`Dropped edge collection: ${name}`);
        }

        if (!db._collection(name)) {
            safeExecute(() => db._createEdgeCollection(name), `Creating edge collection ${name}`);
            log(`Created edge collection: ${name}`);
        }
    }
}

function setupIndexes() {
    log('Setting up indexes...');

    for (const [collectionName, indexes] of Object.entries(CONFIG.indexes)) {
        const collection = db._collection(collectionName);
        if (!collection) {
            log(`Warning: Collection not found for indexing: ${collectionName}`);
            continue;
        }

        for (const indexDef of indexes) {
            safeExecute(() => {
                collection.ensureIndex(indexDef);
            }, `Creating ${indexDef.type} index on ${collectionName}: ${indexDef.fields.join(', ')}`);
        }
    }

    log('Indexes created');
}

function setupGraphs() {
    log('Setting up graphs...');

    for (const [graphName, graphDef] of Object.entries(CONFIG.graphs)) {
        const existingGraphs = graph_module._list();

        if (existingGraphs.includes(graphName)) {
            if (DROP_EXISTING) {
                safeExecute(() => graph_module._drop(graphName, true), `Dropping graph ${graphName}`);
                log(`Dropped graph: ${graphName}`);
            } else {
                log(`Graph already exists: ${graphName}`);
                continue;
            }
        }

        safeExecute(() => {
            graph_module._create(graphName, graphDef.edgeDefinitions, []);
        }, `Creating graph ${graphName}`);
        log(`Created graph: ${graphName}`);
    }
}

function setupAnalyzers() {
    log('Setting up analyzers...');

    const analyzerConfig = {
        name: 'text_en',
        type: 'text',
        properties: {
            locale: 'en',
            case: 'lower',
            accent: false,
            stemming: true,
            stopwords: []
        },
        features: ['position', 'frequency', 'norm']
    };

    safeExecute(() => {
        analyzers.save(
            analyzerConfig.name,
            analyzerConfig.type,
            analyzerConfig.properties,
            analyzerConfig.features
        );
    }, `Creating analyzer ${analyzerConfig.name}`);

    log('Analyzers created');
}

function setupViews() {
    log('Setting up ArangoSearch views...');

    // Findings search view
    const findingsViewName = 'findings_search';
    if (!db._view(findingsViewName)) {
        safeExecute(() => {
            db._createView(findingsViewName, 'arangosearch', {
                links: {
                    findings: {
                        includeAllFields: false,
                        fields: {
                            message: { analyzers: ['text_en'] },
                            description: { analyzers: ['text_en'] },
                            file: { analyzers: ['identity'] }
                        }
                    }
                }
            });
        }, `Creating view ${findingsViewName}`);
        log(`Created view: ${findingsViewName}`);
    }

    // Rules search view
    const rulesViewName = 'rules_search';
    if (!db._view(rulesViewName)) {
        safeExecute(() => {
            db._createView(rulesViewName, 'arangosearch', {
                links: {
                    rules: {
                        includeAllFields: false,
                        fields: {
                            name: { analyzers: ['text_en'] },
                            description: { analyzers: ['text_en'] }
                        }
                    },
                    rulesets: {
                        includeAllFields: false,
                        fields: {
                            name: { analyzers: ['text_en'] },
                            description: { analyzers: ['text_en'] }
                        }
                    }
                }
            });
        }, `Creating view ${rulesViewName}`);
        log(`Created view: ${rulesViewName}`);
    }

    log('Views created');
}

function seedData() {
    if (SKIP_SEED) {
        log('Skipping seed data (ARANGO_SKIP_SEED=true)');
        return;
    }

    log('Seeding initial data...');

    const botsCollection = db._collection('bots');
    const dependsOnCollection = db._collection('bot_depends_on');
    const rulesetsCollection = db._collection('rulesets');

    // Seed bots
    log('Seeding bots...');
    for (const bot of CONFIG.seedBots) {
        bot.createdAt = new Date().toISOString();
        safeExecute(() => {
            try {
                botsCollection.insert(bot);
                log(`Seeded bot: ${bot._key}`);
            } catch (e) {
                if (e.errorNum === 1210) {
                    log(`Bot already exists: ${bot._key}`);
                } else {
                    throw e;
                }
            }
        }, `Inserting bot ${bot._key}`);
    }

    // Seed dependencies
    log('Seeding bot dependencies...');
    for (const dep of CONFIG.seedDependencies) {
        safeExecute(() => {
            try {
                dependsOnCollection.insert(dep);
                log(`Seeded dependency: ${dep._from} -> ${dep._to}`);
            } catch (e) {
                if (e.errorNum === 1210) {
                    log(`Dependency already exists: ${dep._from} -> ${dep._to}`);
                }
            }
        }, `Inserting dependency ${dep._from} -> ${dep._to}`);
    }

    // Seed rulesets
    log('Seeding rulesets...');
    for (const ruleset of CONFIG.seedRulesets) {
        ruleset.createdAt = new Date().toISOString();
        safeExecute(() => {
            try {
                rulesetsCollection.insert(ruleset);
                log(`Seeded ruleset: ${ruleset._key}`);
            } catch (e) {
                if (e.errorNum === 1210) {
                    log(`Ruleset already exists: ${ruleset._key}`);
                }
            }
        }, `Inserting ruleset ${ruleset._key}`);
    }

    log('Seed data complete');
}

// =============================================================================
// MAIN
// =============================================================================

function main() {
    console.log('='.repeat(60));
    console.log('cicd-hyper-a ArangoDB Initialization');
    console.log('='.repeat(60));
    console.log(`Database: ${DATABASE}`);
    console.log(`Drop existing: ${DROP_EXISTING}`);
    console.log(`Skip seed: ${SKIP_SEED}`);
    console.log(`Dry run: ${DRY_RUN}`);
    console.log('='.repeat(60));

    setupDatabase();
    setupCollections();
    setupIndexes();
    setupGraphs();
    setupAnalyzers();
    setupViews();
    seedData();

    console.log('='.repeat(60));
    console.log('Initialization complete!');
    console.log('='.repeat(60));

    // Print summary
    console.log('\nSummary:');
    console.log(`  Document collections: ${CONFIG.documentCollections.length}`);
    console.log(`  Edge collections: ${CONFIG.edgeCollections.length}`);
    console.log(`  Graphs: ${Object.keys(CONFIG.graphs).length}`);
    console.log(`  Bots seeded: ${CONFIG.seedBots.length}`);
    console.log(`  Dependencies seeded: ${CONFIG.seedDependencies.length}`);
    console.log(`  Rulesets seeded: ${CONFIG.seedRulesets.length}`);
}

// Run if executed directly
if (typeof ArangoServerState !== 'undefined') {
    main();
}

// Export for use as module
module.exports = {
    CONFIG,
    setupDatabase,
    setupCollections,
    setupIndexes,
    setupGraphs,
    setupAnalyzers,
    setupViews,
    seedData,
    main
};

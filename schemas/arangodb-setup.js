// SPDX-License-Identifier: AGPL-3.0-or-later
// cicd-hyper-a ArangoDB Setup Script
//
// Run with: arangosh --javascript.execute arangodb-setup.js
// Or import in Foxx service
//
// Environment variables:
//   ARANGO_DATABASE - Database name (default: cicd_hyper_a)
//   ARANGO_DROP_EXISTING - Drop existing collections (default: false)

'use strict';

const db = require('@arangodb').db;
const graph_module = require('@arangodb/general-graph');
const analyzers = require('@arangodb/analyzers');

const DATABASE = process.env.ARANGO_DATABASE || 'cicd_hyper_a';
const DROP_EXISTING = process.env.ARANGO_DROP_EXISTING === 'true';

// =============================================================================
// CONFIGURATION
// =============================================================================

const CONFIG = {
  // Document collections
  documentCollections: [
    'repos',
    'bots',
    'sessions',
    'findings',
    'rules',
    'fixes',
    'workflows',
    'learningData',
    'owners'
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
    'repo_similar_to'
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

  // Indexes
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
      { type: 'persistent', fields: ['enabled'] }
    ],
    workflows: [
      { type: 'persistent', fields: ['repoKey'] },
      { type: 'persistent', fields: ['allActionsPinned'] }
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
  ]
};

// =============================================================================
// SETUP FUNCTIONS
// =============================================================================

function setupDatabase() {
  // Check if database exists
  const databases = db._databases();
  if (!databases.includes(DATABASE)) {
    db._createDatabase(DATABASE);
    console.log(`Created database: ${DATABASE}`);
  }

  db._useDatabase(DATABASE);
  console.log(`Using database: ${DATABASE}`);
}

function setupCollections() {
  // Create document collections
  for (const name of CONFIG.documentCollections) {
    if (DROP_EXISTING && db._collection(name)) {
      db._drop(name);
      console.log(`Dropped collection: ${name}`);
    }

    if (!db._collection(name)) {
      db._create(name);
      console.log(`Created document collection: ${name}`);
    }
  }

  // Create edge collections
  for (const name of CONFIG.edgeCollections) {
    if (DROP_EXISTING && db._collection(name)) {
      db._drop(name);
      console.log(`Dropped edge collection: ${name}`);
    }

    if (!db._collection(name)) {
      db._createEdgeCollection(name);
      console.log(`Created edge collection: ${name}`);
    }
  }
}

function setupIndexes() {
  for (const [collectionName, indexes] of Object.entries(CONFIG.indexes)) {
    const collection = db._collection(collectionName);
    if (!collection) {
      console.warn(`Collection not found for indexing: ${collectionName}`);
      continue;
    }

    for (const indexDef of indexes) {
      try {
        collection.ensureIndex(indexDef);
        console.log(`Created ${indexDef.type} index on ${collectionName}: ${indexDef.fields.join(', ')}`);
      } catch (e) {
        console.log(`Index already exists on ${collectionName}: ${indexDef.fields.join(', ')}`);
      }
    }
  }
}

function setupGraphs() {
  for (const [graphName, graphDef] of Object.entries(CONFIG.graphs)) {
    // Check if graph exists
    const existingGraphs = graph_module._list();
    if (existingGraphs.includes(graphName)) {
      if (DROP_EXISTING) {
        graph_module._drop(graphName, true);
        console.log(`Dropped graph: ${graphName}`);
      } else {
        console.log(`Graph already exists: ${graphName}`);
        continue;
      }
    }

    // Create graph
    const graph = graph_module._create(graphName, graphDef.edgeDefinitions, []);
    console.log(`Created graph: ${graphName}`);
  }
}

function setupAnalyzers() {
  // Create text analyzer for search
  const analyzerName = 'text_en';
  try {
    analyzers.save(analyzerName, 'text', {
      locale: 'en',
      case: 'lower',
      accent: false,
      stemming: true,
      stopwords: []
    }, ['position', 'frequency']);
    console.log(`Created analyzer: ${analyzerName}`);
  } catch (e) {
    console.log(`Analyzer already exists: ${analyzerName}`);
  }
}

function setupViews() {
  // Create ArangoSearch views
  const findingsViewName = 'findings_search';
  if (!db._view(findingsViewName)) {
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
    console.log(`Created view: ${findingsViewName}`);
  }

  const rulesViewName = 'rules_search';
  if (!db._view(rulesViewName)) {
    db._createView(rulesViewName, 'arangosearch', {
      links: {
        rules: {
          includeAllFields: false,
          fields: {
            name: { analyzers: ['text_en'] },
            description: { analyzers: ['text_en'] }
          }
        }
      }
    });
    console.log(`Created view: ${rulesViewName}`);
  }
}

function seedData() {
  const botsCollection = db._collection('bots');
  const dependsOnCollection = db._collection('bot_depends_on');

  // Seed bots
  for (const bot of CONFIG.seedBots) {
    try {
      botsCollection.insert(bot);
      console.log(`Seeded bot: ${bot._key}`);
    } catch (e) {
      if (e.errorNum === 1210) { // duplicate key
        console.log(`Bot already exists: ${bot._key}`);
      } else {
        throw e;
      }
    }
  }

  // Seed dependencies
  for (const dep of CONFIG.seedDependencies) {
    try {
      dependsOnCollection.insert(dep);
      console.log(`Seeded dependency: ${dep._from} -> ${dep._to}`);
    } catch (e) {
      if (e.errorNum === 1210) { // duplicate key
        console.log(`Dependency already exists: ${dep._from} -> ${dep._to}`);
      } else {
        throw e;
      }
    }
  }
}

// =============================================================================
// MAIN
// =============================================================================

function main() {
  console.log('='.repeat(60));
  console.log('cicd-hyper-a ArangoDB Setup');
  console.log('='.repeat(60));

  setupDatabase();
  setupCollections();
  setupIndexes();
  setupGraphs();
  setupAnalyzers();
  setupViews();
  seedData();

  console.log('='.repeat(60));
  console.log('Setup complete!');
  console.log('='.repeat(60));

  // Print summary
  console.log('\nSummary:');
  console.log(`  Document collections: ${CONFIG.documentCollections.length}`);
  console.log(`  Edge collections: ${CONFIG.edgeCollections.length}`);
  console.log(`  Graphs: ${Object.keys(CONFIG.graphs).length}`);
  console.log(`  Bots seeded: ${CONFIG.seedBots.length}`);
  console.log(`  Dependencies seeded: ${CONFIG.seedDependencies.length}`);
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

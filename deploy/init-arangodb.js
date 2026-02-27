// SPDX-License-Identifier: PMPL-1.0-or-later
// ArangoDB initialization script for cicd-hyper-a
// Updated for Neurosymbolic integration and VeriSimDB federation

const db = require('@arangodb').db;
const graph_module = require('@arangodb/general-graph');

const DATABASE_NAME = 'cicd_hyper_a';

// Create database if not exists
if (!db._databases().includes(DATABASE_NAME)) {
  db._createDatabase(DATABASE_NAME);
  console.log(`Created database: ${DATABASE_NAME}`);
}

db._useDatabase(DATABASE_NAME);

// ============================================================
// DOCUMENT COLLECTIONS
// ============================================================

const documentCollections = [
  'repos',
  'alerts',
  'rules',
  'rulesets',
  'fixes',
  'patterns',
  'workflows',
  'bots',
  'outcomes',
  'contributors',
  'anomalies',
  'neural_states',
  'learning_data',
  'confidence_history',
  'sessions'
];

documentCollections.forEach(name => {
  if (!db._collection(name)) {
    db._createDocumentCollection(name);
    console.log(`Created collection: ${name}`);
  }
});

// ============================================================
// EDGE COLLECTIONS
// ============================================================

const edgeCollections = [
  'repo_has_alert',
  'repo_has_workflow',
  'alert_fixed_by',
  'rule_applies_fix',
  'ruleset_contains',
  'repo_uses_ruleset',
  'rule_derived_from',
  'workflow_triggers_rule',
  'bot_executes_recipe',
  'recipe_produces_outcome',
  'repo_depends_on',
  'verisim_links' // Federated link to VeriSimDB
];

edgeCollections.forEach(name => {
  if (!db._collection(name)) {
    db._createEdgeCollection(name);
    console.log(`Created edge collection: ${name}`);
  }
});

// ============================================================
// INDEXES
// ============================================================

// Repos indexes
db.repos.ensureIndex({ type: 'persistent', fields: ['forge'] });
db.repos.ensureIndex({ type: 'persistent', fields: ['owner'] });
db.repos.ensureIndex({ type: 'persistent', fields: ['health_score'] });

// Alerts indexes
db.alerts.ensureIndex({ type: 'persistent', fields: ['repo_key'] });
db.alerts.ensureIndex({ type: 'persistent', fields: ['severity'] });
db.alerts.ensureIndex({ type: 'persistent', fields: ['category'] });
db.alerts.ensureIndex({ type: 'persistent', fields: ['fix_applied'] });
db.alerts.ensureIndex({ type: 'persistent', fields: ['verisim_id'] });

// Neural indexes
db.neural_states.ensureIndex({ type: 'persistent', fields: ['network'] });
db.anomalies.ensureIndex({ type: 'persistent', fields: ['source_network'] });
db.anomalies.ensureIndex({ type: 'persistent', fields: ['timestamp'] });
db.confidence_history.ensureIndex({ type: 'persistent', fields: ['recipe_id', 'timestamp'] });

// Rules indexes
db.rules.ensureIndex({ type: 'persistent', fields: ['effect'] });
db.rules.ensureIndex({ type: 'persistent', fields: ['enabled'] });
db.rules.ensureIndex({ type: 'persistent', fields: ['trigger_count'] });

// Rulesets indexes
db.rulesets.ensureIndex({ type: 'persistent', fields: ['name'], unique: true });
db.rulesets.ensureIndex({ type: 'persistent', fields: ['effect'] });
db.rulesets.ensureIndex({ type: 'persistent', fields: ['verified'] });

console.log('Created indexes');

// ============================================================
// GRAPH DEFINITION
// ============================================================

const graphName = 'cicd_graph';

if (!graph_module._list().includes(graphName)) {
  const edgeDefinitions = [
    { collection: 'repo_has_alert', from: ['repos'], to: ['alerts'] },
    { collection: 'repo_has_workflow', from: ['repos'], to: ['workflows'] },
    { collection: 'alert_fixed_by', from: ['alerts'], to: ['rules'] },
    { collection: 'rule_applies_fix', from: ['rules'], to: ['fixes'] },
    { collection: 'ruleset_contains', from: ['rulesets'], to: ['rules'] },
    { collection: 'repo_uses_ruleset', from: ['repos'], to: ['rulesets'] },
    { collection: 'rule_derived_from', from: ['rules'], to: ['patterns'] },
    { collection: 'workflow_triggers_rule', from: ['workflows'], to: ['rules'] },
    { collection: 'bot_executes_recipe', from: ['bots'], to: ['recipes'] },
    { collection: 'recipe_produces_outcome', from: ['recipes'], to: ['outcomes'] },
    { collection: 'repo_depends_on', from: ['repos'], to: ['repos'] },
    { collection: 'verisim_links', from: ['alerts', 'neural_states', 'outcomes'], to: ['repos'] }
  ];

  graph_module._create(graphName, edgeDefinitions);
  console.log(`Created graph: ${graphName}`);
}

// ============================================================
// ANALYZERS (for full-text search)
// ============================================================

const analyzers = require('@arangodb/analyzers');

if (!analyzers.analyzer('text_en')) {
  analyzers.save('text_en', 'text', {
    locale: 'en',
    case: 'lower',
    accent: false,
    stemming: true,
    stopwords: []
  }, ['frequency', 'norm', 'position']);
  console.log('Created text analyzer');
}

// ============================================================
// VIEWS (for full-text search)
// ============================================================

if (!db._view('rules_search')) {
  db._createView('rules_search', 'arangosearch', {
    links: {
      rules: { analyzers: ['text_en'], fields: { name: {}, description: {} }, includeAllFields: false },
      rulesets: { analyzers: ['text_en'], fields: { name: {}, description: {} }, includeAllFields: false }
    }
  });
  console.log('Created search view');
}

console.log('ArangoDB initialization complete');

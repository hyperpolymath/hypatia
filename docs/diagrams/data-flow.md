<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# cicd-hyper-a Data Flow

## Overview

This document describes the data flow between components in the cicd-hyper-a platform.

## Scan Data Flow

```mermaid
sequenceDiagram
    participant User
    participant CLI
    participant Scanner
    participant Rules
    participant Learning
    participant ArangoDB

    User->>CLI: hyper scan /path/to/repo
    CLI->>Scanner: scan_repository(path)

    Scanner->>Scanner: walk_directory()
    Scanner->>Scanner: identify_files()

    loop For each file
        Scanner->>Rules: check_file(file)
        Rules->>Rules: apply_preventive_rules()
        Rules->>Learning: get_learned_patterns()
        Learning->>ArangoDB: query patterns
        ArangoDB-->>Learning: patterns
        Learning-->>Rules: patterns
        Rules-->>Scanner: findings
    end

    Scanner->>Scanner: aggregate_findings()
    Scanner->>Scanner: sort_by_severity()
    Scanner-->>CLI: ScanResults
    CLI-->>User: formatted output
```

## Fleet Execution Data Flow

```mermaid
sequenceDiagram
    participant User
    participant CLI
    participant Fleet
    participant Bot
    participant Forge
    participant Rules

    User->>CLI: hyper fleet run
    CLI->>Fleet: execute(config)

    Fleet->>Fleet: load_bot_definitions()
    Fleet->>Fleet: build_dependency_graph()
    Fleet->>Fleet: topological_sort()

    loop For each bot in order
        Fleet->>Bot: run(repo_path)
        Bot->>Forge: get_repository_info()
        Forge-->>Bot: repo_info

        Bot->>Rules: evaluate_checks()
        Rules-->>Bot: findings

        alt has auto_fix enabled
            Bot->>Bot: apply_fixes()
            Bot->>Forge: create_commit()
            Forge-->>Bot: commit_sha
        end

        Bot-->>Fleet: BotResult
    end

    Fleet->>Fleet: aggregate_results()
    Fleet-->>CLI: FleetRunResult
    CLI-->>User: summary
```

## Learning Feedback Loop

```mermaid
sequenceDiagram
    participant Fix
    participant Rules
    participant Learning
    participant ArangoDB
    participant Distiller

    Fix->>Rules: apply_fix(issue_type)
    Rules->>Learning: record_fix_attempt(issue, fix)

    alt fix successful
        Learning->>ArangoDB: store_success(issue, fix)
        Learning->>Learning: increase_confidence(pattern)
    else fix failed
        Learning->>ArangoDB: store_failure(issue, fix, error)
        Learning->>Learning: decrease_confidence(pattern)
    end

    Learning->>Learning: check_promotion_threshold()

    alt confidence >= 0.9
        Learning->>Distiller: pattern_to_rule_candidate()
        Distiller->>Distiller: validate_candidate()

        alt validation passed
            Distiller->>Rules: promote_to_rule()
            Distiller->>ArangoDB: store_new_rule()
        end
    end
```

## Registry Operations

```mermaid
sequenceDiagram
    participant User
    participant CLI
    participant API
    participant Registry
    participant Verify
    participant Storage

    User->>CLI: hyper deposit ruleset.lgt
    CLI->>CLI: parse_ruleset()
    CLI->>API: POST /rulesets

    API->>Verify: verify_ruleset(ruleset)
    Verify->>Verify: check_conflicts()
    Verify->>Verify: check_coverage()
    Verify->>Verify: run_quickcheck()
    Verify-->>API: VerifyResult

    alt verified
        API->>Registry: deposit(ruleset)
        Registry->>Storage: store(ruleset)
        Storage-->>Registry: entry_id
        Registry->>Registry: update_index()
        Registry->>Registry: audit_log()
        Registry-->>API: RegistryEntry
        API-->>CLI: success
        CLI-->>User: "Deposited successfully"
    else verification failed
        API-->>CLI: errors
        CLI-->>User: "Verification failed: ..."
    end
```

## Webhook Event Flow

```mermaid
sequenceDiagram
    participant Forge as GitHub/GitLab
    participant Adapter
    participant Rules
    participant Fleet
    participant Bot
    participant Learning

    Forge->>Adapter: POST /webhook
    Adapter->>Adapter: verify_signature()
    Adapter->>Adapter: parse_payload()

    alt push event
        Adapter->>Rules: evaluate_commit(payload)
        Rules->>Rules: check_block_conditions()

        alt should block
            Rules-->>Adapter: block_reason
            Adapter->>Forge: create_check_run(failed)
        else allow
            Adapter->>Fleet: queue_analysis(repo)
            Fleet->>Bot: async_run()
            Bot-->>Learning: record_findings()
            Fleet-->>Adapter: run_id
            Adapter->>Forge: create_check_run(pending)
        end

    else pull_request event
        Adapter->>Fleet: run_pr_checks(pr)
        Fleet->>Bot: run_all()
        Bot-->>Fleet: results
        Fleet->>Adapter: aggregate_results()
        Adapter->>Forge: create_review(results)
    end
```

## Knowledge Graph Data Model

```mermaid
graph LR
    subgraph "Entities"
        Repo[Repository]
        Alert[Alert]
        Fix[Fix]
        Pattern[Pattern]
        Rule[Rule]
        Ruleset[Ruleset]
    end

    subgraph "Relationships"
        Repo -->|has| Alert
        Alert -->|fixed_by| Fix
        Fix -->|generates| Pattern
        Pattern -->|becomes| Rule
        Rule -->|part_of| Ruleset
        Pattern -->|similar_to| Pattern
    end

    subgraph "Properties"
        Alert -.->|severity| Severity
        Alert -.->|category| Category
        Pattern -.->|confidence| Score
        Fix -.->|success_rate| Rate
    end
```

## Cache Strategy

```mermaid
flowchart TD
    Request[API Request] --> CacheCheck{In Cache?}

    CacheCheck -->|Yes| CacheHit[Return Cached]
    CacheCheck -->|No| Fetch[Fetch from Source]

    Fetch --> Store[Store in DragonflyDB]
    Store --> SetTTL[Set TTL]

    SetTTL --> Return[Return Response]

    subgraph "TTL Strategy"
        RepoInfo[Repo Info: 5min]
        PRList[PR List: 1min]
        Rulesets[Rulesets: 1hour]
        Patterns[Patterns: 10min]
    end

    SetTTL --> RepoInfo
    SetTTL --> PRList
    SetTTL --> Rulesets
    SetTTL --> Patterns
```

## Data Transformation Pipeline

```mermaid
flowchart LR
    subgraph "Input"
        Raw[Raw Alert Data]
        Training[Training Data]
    end

    subgraph "Processing"
        Parse[Parse & Normalize]
        Classify[Classify Severity]
        Extract[Extract Patterns]
        Validate[Validate Rules]
    end

    subgraph "Output"
        Rules[Active Rules]
        Knowledge[Knowledge Base]
        Metrics[Metrics]
    end

    Raw --> Parse
    Training --> Parse
    Parse --> Classify
    Classify --> Extract
    Extract --> Validate
    Validate --> Rules
    Validate --> Knowledge
    Classify --> Metrics
```

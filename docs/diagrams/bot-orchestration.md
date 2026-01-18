<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Bot Fleet Orchestration

## Overview

The cicd-hyper-a bot fleet uses dependency-based topological sorting to execute bots in the correct order.

## Bot Dependency Graph

```mermaid
graph TD
    subgraph "Security Bots"
        echidna[echidnabot<br/>Secret Detection]
    end

    subgraph "Quality Bots"
        rra[robot-repo-automaton<br/>Standards Enforcement]
        seambot[seambot<br/>Code Formatting]
    end

    subgraph "Documentation Bots"
        glambot[glambot<br/>Documentation]
    end

    subgraph "Compliance Bots"
        compliance[compliance-bot<br/>License/Policy]
    end

    subgraph "Release Bots"
        finishing[finishing-bot<br/>Release Prep]
    end

    %% Dependencies
    echidna --> rra
    rra --> seambot
    rra --> glambot
    rra --> compliance
    seambot --> finishing
    glambot --> finishing
    compliance --> finishing

    %% Styling
    classDef security fill:#f66,stroke:#333,stroke-width:2px;
    classDef quality fill:#6f6,stroke:#333,stroke-width:2px;
    classDef docs fill:#66f,stroke:#333,stroke-width:2px;
    classDef compliance fill:#ff6,stroke:#333,stroke-width:2px;
    classDef release fill:#f6f,stroke:#333,stroke-width:2px;

    class echidna security;
    class rra,seambot quality;
    class glambot docs;
    class compliance compliance;
    class finishing release;
```

## Execution Order

```mermaid
gantt
    title Bot Execution Timeline (Serial Mode)
    dateFormat X
    axisFormat %s

    section Security
    echidnabot       :0, 30

    section Quality
    robot-repo-automaton :30, 90
    seambot              :90, 120

    section Documentation
    glambot          :90, 130

    section Compliance
    compliance-bot   :90, 140

    section Release
    finishing-bot    :140, 180
```

## Parallel Execution

When `--parallel` is enabled, independent bots run concurrently:

```mermaid
gantt
    title Bot Execution Timeline (Parallel Mode)
    dateFormat X
    axisFormat %s

    section Layer 1
    echidnabot       :0, 30

    section Layer 2
    robot-repo-automaton :30, 90

    section Layer 3 (Parallel)
    seambot              :90, 120
    glambot              :90, 130
    compliance-bot       :90, 140

    section Layer 4
    finishing-bot    :140, 180
```

## Fleet Run State Machine

```mermaid
stateDiagram-v2
    [*] --> Initializing

    Initializing --> Loading: load bots
    Loading --> Sorting: build graph
    Sorting --> Ready: topological sort

    Ready --> Running: start execution

    Running --> Running: bot completed
    Running --> Failed: bot failed
    Running --> Completed: all bots done

    Failed --> Stopping: continue_on_error=false
    Failed --> Running: continue_on_error=true

    Stopping --> Stopped
    Stopped --> [*]

    Completed --> [*]
```

## Bot Execution Flow

```mermaid
flowchart TD
    Start([Start Bot]) --> Init[Initialize Context]
    Init --> Clone{Already Cloned?}

    Clone -->|No| DoClone[Clone Repository]
    Clone -->|Yes| Fetch[Fetch Updates]

    DoClone --> Checkout
    Fetch --> Checkout

    Checkout[Checkout Branch] --> Analyze[Run Analysis]
    Analyze --> Findings{Findings?}

    Findings -->|None| Success[Report Success]
    Findings -->|Yes| CanFix{Auto-Fix Enabled?}

    CanFix -->|No| Report[Report Findings]
    CanFix -->|Yes| ApplyFix[Apply Fixes]

    ApplyFix --> Verify{Verify Fix}
    Verify -->|Pass| Commit[Create Commit]
    Verify -->|Fail| Report

    Commit --> Push{Push Enabled?}
    Push -->|Yes| DoPush[Push Changes]
    Push -->|No| Report

    DoPush --> CreatePR{Create PR?}
    CreatePR -->|Yes| PR[Open Pull Request]
    CreatePR -->|No| Report

    PR --> Report
    Report --> Success
    Success --> End([End Bot])
```

## Category-Based Filtering

```mermaid
flowchart LR
    subgraph "Input"
        All[All Bots]
    end

    subgraph "Filter: --category security"
        SecFilter{Category?}
    end

    subgraph "Result"
        echidna[echidnabot]
    end

    All --> SecFilter
    SecFilter -->|security| echidna
    SecFilter -->|quality| Excluded1[excluded]
    SecFilter -->|docs| Excluded2[excluded]
    SecFilter -->|compliance| Excluded3[excluded]
    SecFilter -->|release| Excluded4[excluded]

    style Excluded1 fill:#ccc,stroke:#999
    style Excluded2 fill:#ccc,stroke:#999
    style Excluded3 fill:#ccc,stroke:#999
    style Excluded4 fill:#ccc,stroke:#999
```

## Bot Selection Logic

```mermaid
flowchart TD
    Start([User Request]) --> HasBots{--bots specified?}

    HasBots -->|Yes| ParseBots[Parse bot list]
    HasBots -->|No| AllBots[Use all bots]

    ParseBots --> HasExclude
    AllBots --> HasExclude

    HasExclude{--exclude specified?} -->|Yes| FilterExclude[Remove excluded]
    HasExclude -->|No| HasCategory

    FilterExclude --> HasCategory

    HasCategory{--category specified?} -->|Yes| FilterCategory[Filter by category]
    HasCategory -->|No| BuildGraph

    FilterCategory --> BuildGraph[Build Dependency Graph]

    BuildGraph --> ValidateDeps{Dependencies Met?}
    ValidateDeps -->|Yes| Sort[Topological Sort]
    ValidateDeps -->|No| AddMissing[Add required deps]

    AddMissing --> Sort
    Sort --> Execute[Execute Bots]
```

## Error Handling

```mermaid
flowchart TD
    BotRun[Bot Execution] --> Result{Success?}

    Result -->|Yes| NextBot[Continue to Next]
    Result -->|Error| CheckConfig{continue_on_error?}

    CheckConfig -->|true| LogError[Log Error]
    CheckConfig -->|false| Abort[Abort Fleet Run]

    LogError --> MarkFailed[Mark Bot Failed]
    MarkFailed --> CheckDeps{Other bots depend on this?}

    CheckDeps -->|Yes| SkipDependents[Skip dependent bots]
    CheckDeps -->|No| NextBot

    SkipDependents --> NextBot

    Abort --> Cleanup[Cleanup Resources]
    Cleanup --> ReportFailure[Report Failure]
```

## Resource Management

```mermaid
flowchart LR
    subgraph "Resource Pool"
        CPU[CPU Cores]
        Mem[Memory]
        IO[I/O Bandwidth]
    end

    subgraph "Bots (Parallel)"
        B1[Bot 1]
        B2[Bot 2]
        B3[Bot 3]
    end

    subgraph "Limits"
        MaxParallel[max_parallel: 4]
        Timeout[timeout: 300s]
        MemLimit[mem_limit: 2GB]
    end

    CPU --> B1
    CPU --> B2
    CPU --> B3

    Mem --> B1
    Mem --> B2
    Mem --> B3

    B1 --> MaxParallel
    B2 --> MaxParallel
    B3 --> MaxParallel

    B1 --> Timeout
    B2 --> Timeout
    B3 --> Timeout
```

## Bot Configuration Schema

```mermaid
classDiagram
    class Bot {
        +String id
        +String name
        +String description
        +BotCategory category
        +Vec~String~ dependencies
        +Vec~String~ checks
        +bool can_fix
        +u32 estimated_runtime
    }

    class BotCategory {
        <<enumeration>>
        Security
        Quality
        Documentation
        Compliance
        Performance
        Release
    }

    class BotResult {
        +String bot_id
        +BotStatus status
        +Vec~Finding~ findings
        +Vec~Fix~ fixes_applied
        +u64 duration_ms
        +Option~String~ error
    }

    class BotStatus {
        <<enumeration>>
        Pending
        Running
        Completed
        Failed
        Skipped
    }

    Bot --> BotCategory
    BotResult --> BotStatus
    BotResult --> Bot
```

## Fleet Run Summary

```mermaid
pie title Typical Fleet Run Results
    "Fixed Automatically" : 65
    "Reported (No Fix)" : 20
    "Already Compliant" : 10
    "Skipped" : 5
```

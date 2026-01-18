<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Rule Execution Pipeline

## Overview

The cicd-hyper-a rule engine uses a neurosymbolic approach, combining static declarative rules with dynamically learned patterns.

## Rule Evaluation Pipeline

```mermaid
flowchart TB
    subgraph "Input"
        Commit[Commit/PR]
        File[File Content]
        Context[Repository Context]
    end

    subgraph "Rule Loading"
        Static[Static Rules<br/>cicd_rules.lgt]
        Learned[Learned Patterns<br/>learning.lgt]
        Distilled[Distilled Rules<br/>rule_distiller.lgt]
    end

    subgraph "Evaluation"
        Preventive[Preventive Rules]
        Curative[Curative Rules]
        Diagnostic[Diagnostic Rules]
    end

    subgraph "Output"
        Block[Block Commit]
        Fix[Apply Fix]
        Report[Report Finding]
    end

    Commit --> Preventive
    File --> Preventive
    File --> Curative
    File --> Diagnostic
    Context --> Preventive
    Context --> Curative

    Static --> Preventive
    Static --> Curative
    Static --> Diagnostic
    Learned --> Curative
    Distilled --> Curative

    Preventive -->|blocked| Block
    Curative -->|auto_fix| Fix
    Diagnostic -->|finding| Report
```

## Preventive Rule Evaluation

```mermaid
flowchart TD
    Commit[New Commit] --> ExtractFiles[Extract Changed Files]

    ExtractFiles --> ForEach{For Each File}

    ForEach --> CheckLang[Check Language]
    CheckLang --> Banned{Banned Language?}

    Banned -->|TypeScript| BlockTS[Block: typescript_detected]
    Banned -->|Go| BlockGo[Block: golang_detected]
    Banned -->|Python| CheckSalt{SaltStack?}
    Banned -->|Node| BlockNode[Block: nodejs_detected]
    Banned -->|No| NextCheck

    CheckSalt -->|Yes| NextCheck[Continue]
    CheckSalt -->|No| BlockPy[Block: python_detected]

    NextCheck --> CheckSPDX[Check SPDX Header]
    CheckSPDX --> HasSPDX{Has Header?}

    HasSPDX -->|No| BlockSPDX[Block: missing_spdx]
    HasSPDX -->|Yes| CheckLicense[Check License]

    CheckLicense --> CorrectLicense{AGPL-3.0?}
    CorrectLicense -->|No| BlockLicense[Block: wrong_license_header]
    CorrectLicense -->|Yes| CheckSecrets

    CheckSecrets[Check for Secrets] --> HasSecrets{Secrets Found?}
    HasSecrets -->|Yes| BlockSecret[Block: secret_detected]
    HasSecrets -->|No| CheckWorkflow

    CheckWorkflow{Is Workflow?} -->|Yes| CheckActions
    CheckWorkflow -->|No| Pass

    CheckActions[Check Actions] --> Unpinned{Unpinned Actions?}
    Unpinned -->|Yes| BlockAction[Block: unpinned_action]
    Unpinned -->|No| CheckPerms

    CheckPerms[Check Permissions] --> HasPerms{Has Permissions?}
    HasPerms -->|No| BlockPerms[Block: missing_permissions]
    HasPerms -->|Yes| Pass[Allow Commit]
```

## Curative Rule Execution

```mermaid
sequenceDiagram
    participant Scanner
    participant Rules
    participant Learning
    participant Fixer
    participant Verify

    Scanner->>Rules: find_issues(repo)
    Rules->>Rules: evaluate_curative_rules()

    loop For each issue
        Rules->>Learning: get_best_fix(issue_type)

        alt learned fix available (confidence >= 0.75)
            Learning-->>Rules: fix(Fix, learned, 0.85)
        else use static fix
            Learning-->>Rules: fix(Fix, static, 1.0)
        end

        Rules->>Fixer: apply_fix(issue, fix)
        Fixer->>Fixer: execute_fix_action()
        Fixer->>Verify: verify_fix(issue, fix)

        alt fix verified
            Verify-->>Fixer: success
            Fixer->>Learning: record_fix_success()
        else fix failed
            Verify-->>Fixer: failure
            Fixer->>Learning: record_fix_failure()
        end

        Fixer-->>Rules: FixResult
    end

    Rules-->>Scanner: all_fixes
```

## Learning Integration

```mermaid
flowchart LR
    subgraph "Static Rules"
        SR1[TokenPermissionsID]
        SR2[PinnedDependenciesID]
        SR3[SecurityPolicyID]
    end

    subgraph "Learning Layer"
        Confidence[Confidence Scores]
        Patterns[Learned Patterns]
        Feedback[Feedback Loop]
    end

    subgraph "Decision"
        Select{Select Best Fix}
    end

    subgraph "Output"
        StaticFix[Static Fix<br/>Confidence: 1.0]
        LearnedFix[Learned Fix<br/>Confidence: 0.85]
    end

    SR1 --> Select
    SR2 --> Select
    SR3 --> Select

    Patterns --> Select
    Confidence --> Select

    Select -->|confidence >= 0.75| LearnedFix
    Select -->|confidence < 0.75| StaticFix

    LearnedFix --> Feedback
    StaticFix --> Feedback
    Feedback --> Confidence
```

## Severity Classification

```mermaid
flowchart TD
    Alert[Alert Type] --> Classify{classify_severity/2}

    Classify --> Critical[Critical]
    Classify --> High[High]
    Classify --> Medium[Medium]
    Classify --> Low[Low]
    Classify --> Info[Info]

    Critical --> C1[hard-coded-cryptographic-value]
    Critical --> C2[sql-injection]
    Critical --> C3[VulnerabilitiesID]

    High --> H1[remote-property-injection]
    High --> H2[TokenPermissionsID]
    High --> H3[missing-workflow-permissions]
    High --> H4[upload-pages-artifact-transitive-deps]

    Medium --> M1[PinnedDependenciesID]
    Medium --> M2[SecurityPolicyID]
    Medium --> M3[BranchProtectionID]
    Medium --> M4[syntax-error]

    Low --> L1[unused-local-variable]
    Low --> L2[MaintainedID]
    Low --> L3[deno-lint-include-pattern]

    Info --> I1[Default fallback]
```

## Auto-Fix Decision Tree

```mermaid
flowchart TD
    Issue[Issue Detected] --> IsAutoFixable{is_auto_fixable?}

    IsAutoFixable -->|Yes| GetFix[Get Fix Strategy]
    IsAutoFixable -->|No| ManualReport[Report for Manual Fix]

    GetFix --> HasLearned{Learned Fix Available?}

    HasLearned -->|Yes, conf >= 0.75| UseLearned[Use Learned Fix]
    HasLearned -->|No| UseStatic[Use Static Fix]

    UseLearned --> ApplyFix
    UseStatic --> ApplyFix

    ApplyFix[Apply Fix] --> VerifyFix{Fix Verified?}

    VerifyFix -->|Yes| RecordSuccess[Record Success]
    VerifyFix -->|No| RecordFailure[Record Failure]

    RecordSuccess --> UpdateConfidence[Increase Confidence]
    RecordFailure --> UpdateConfidence2[Decrease Confidence]

    UpdateConfidence --> Done[Done]
    UpdateConfidence2 --> Fallback{Fallback Available?}

    Fallback -->|Yes| UseStatic
    Fallback -->|No| ManualReport
```

## Rule Types

```mermaid
classDiagram
    class Rule {
        <<interface>>
        +evaluate(context)
        +get_severity()
        +is_auto_fixable()
    }

    class PreventiveRule {
        +block_commit_if(commit, reason)
        +get_block_message()
    }

    class CurativeRule {
        +auto_fix(repo, issue_type)
        +suggest_fix(issue_type)
        +verify_fix(before, after)
    }

    class DiagnosticRule {
        +detect_waste(repo)
        +classify_severity(alert)
        +report_finding(finding)
    }

    Rule <|-- PreventiveRule
    Rule <|-- CurativeRule
    Rule <|-- DiagnosticRule
```

## CI/CD Waste Detection Pipeline

```mermaid
flowchart TD
    Repo[Repository] --> ScanWorkflows[Scan Workflows]

    ScanWorkflows --> CheckDupes{Duplicate Workflows?}
    CheckDupes -->|Yes| WasteDupe[duplicate_workflow]
    CheckDupes -->|No| CheckPublish

    CheckPublish{5+ Publish Workflows?} -->|Yes| WastePublish[unused_publish_workflows]
    CheckPublish -->|No| CheckMirror

    CheckMirror{Mirror without Secrets?} -->|Yes| WasteMirror[mirror_missing_secrets]
    CheckMirror -->|No| CheckNpm

    CheckNpm{npm despite Blocker?} -->|Yes| WasteNpm[npm_in_workflow]
    CheckNpm -->|No| CheckSpec

    CheckSpec{Spec Repo with 10+ Workflows?} -->|Yes| WasteSpec[spec_repo_full_ci]
    CheckSpec -->|No| CheckSemgrep

    CheckSemgrep{Semgrep on Wrong Lang?} -->|Yes| WasteSemgrep[semgrep_language_mismatch]
    CheckSemgrep -->|No| CheckCount

    CheckCount{More than 15 Workflows?} -->|Yes| WasteCount[excessive_workflow_count]
    CheckCount -->|No| Done[No Waste Detected]

    WasteDupe --> Report
    WastePublish --> Report
    WasteMirror --> Report
    WasteNpm --> Report
    WasteSpec --> Report
    WasteSemgrep --> Report
    WasteCount --> Report

    Report[Report Waste Findings]
```

## CodeQL Language Matrix Validation

```mermaid
flowchart TD
    Repo[Repository] --> DetectLangs[Detect Languages]

    DetectLangs --> ForEach{For Each Language}

    ForEach --> CheckSupport{CodeQL Supported?}

    CheckSupport -->|Rust| UseActions1[Use 'actions']
    CheckSupport -->|OCaml| UseActions2[Use 'actions']
    CheckSupport -->|Haskell| UseActions3[Use 'actions']
    CheckSupport -->|JavaScript| UseJS[Use 'javascript-typescript']
    CheckSupport -->|Python| UsePy[Use 'python']
    CheckSupport -->|Go| UseGo[Use 'go']

    UseActions1 --> BuildMatrix
    UseActions2 --> BuildMatrix
    UseActions3 --> BuildMatrix
    UseJS --> BuildMatrix
    UsePy --> BuildMatrix
    UseGo --> BuildMatrix

    BuildMatrix[Build Valid Matrix] --> Compare

    Compare{Matches Config?} -->|Yes| Valid[Configuration Valid]
    Compare -->|No| Invalid[Report Mismatch]

    Invalid --> SuggestFix[suggest_fix: Update language matrix]
```

## Rule Promotion Pipeline

```mermaid
flowchart LR
    subgraph "Learning"
        Pattern[New Pattern]
        Confidence[Confidence Score]
    end

    subgraph "Validation"
        Threshold{Confidence >= 0.9?}
        Candidate[Rule Candidate]
        Validate[Validate Against Tests]
    end

    subgraph "Promotion"
        Promote[Promote to Active Rule]
        Store[Store in Knowledge Base]
    end

    Pattern --> Confidence
    Confidence --> Threshold

    Threshold -->|Yes| Candidate
    Threshold -->|No| Continue[Continue Learning]

    Candidate --> Validate
    Validate -->|Pass| Promote
    Validate -->|Fail| Adjust[Adjust Pattern]

    Adjust --> Confidence
    Promote --> Store

    Continue --> Pattern
```

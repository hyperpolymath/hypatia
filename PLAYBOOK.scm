;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational playbooks and procedures for hypatia

(define-module (hyperpolymath hypatia playbook)
  :version "1.0.0"
  :description "Operational playbooks for hypatia neurosymbolic orchestrator"
  :export (playbooks procedures))

(define (playbooks)
  '((fleet-validation
     (description . "Validate entire gitbot fleet for v1 readiness")
     (steps
      ("1. Run hypatia validate-v1 --all-repos"
       "2. Hypatia orchestrates finishingbot + glambot on each repo"
       "3. Logtalk rules aggregate results"
       "4. Haskell registry stores validation state"
       "5. Rust CLI generates fleet dashboard"
       "6. Identify repos blocking v1 release"
       "7. Prioritize fixes based on dependency order"
       "8. Re-validate after fixes applied")))

    (single-repo-deep-dive
     (description . "Deep validation of one repository")
     (steps
      ("1. Run hypatia validate-v1 --repo <name>"
       "2. Execute finishingbot audit"
       "3. Execute glambot audit"
       "4. Run custom validation rules from v1_readiness.lgt"
       "5. Check all 6 SCM files present and valid"
       "6. Verify testing infrastructure"
       "7. Check language compliance (no banned languages)"
       "8. Generate detailed v1 readiness report")))

    (template-scaffolding
     (description . "Create new repo from template with v1 compliance built-in")
     (steps
      ("1. Run hypatia scaffold --template rust-cli --name new-project"
       "2. Haskell validator checks template validity"
       "3. Generate all 6 SCM files with project metadata"
       "4. Create .tool-versions, .editorconfig"
       "5. Add comprehensive CI workflows (15+ workflows)"
       "6. Set up testing infrastructure (tests, benches, fuzz, stress)"
       "7. Initialize git with proper .gitignore"
       "8. Validate newly created repo passes all checks")))

    (rule-synthesis
     (description . "Extract validation rules from existing repos")
     (steps
      ("1. Analyze patterns across successful v1 releases"
       "2. Logtalk rule learning from validation history"
       "3. Generate new predicates for common patterns"
       "4. Haskell type system ensures rule soundness"
       "5. Test new rules on existing repos"
       "6. Deploy rules to production v1_readiness.lgt")))

    (fleet-synchronization
     (description . "Apply fixes across multiple repos consistently")
     (steps
      ("1. Identify common issue across fleet (e.g., missing .tool-versions)"
       "2. Define fix in Logtalk rule"
       "3. Generate fix scripts via Rust CLI"
       "4. Execute fixes in parallel across repos"
       "5. Validate all repos after fixes"
       "6. Create PRs with validation reports"
       "7. Track progress in Haskell registry")))))

(define (procedures)
  '((adding-bot-to-fleet
     (description . "Integrate new bot into hypatia orchestration")
     (steps
      ("1. Define bot interface in Haskell (src/Bots/<BotName>.hs)"
       "2. Add Logtalk predicates for bot-specific validation"
       "3. Register bot in Rust CLI (src/bots/mod.rs)"
       "4. Create bot-specific configuration schema"
       "5. Add bot to orchestration workflows"
       "6. Test bot integration on sample repo"
       "7. Deploy to fleet orchestration")))

    (debugging-validation-failures
     (description . "Troubleshoot why validation fails")
     (steps
      ("1. Run hypatia validate-v1 --repo <name> --verbose"
       "2. Check Logtalk trace for rule evaluation"
       "3. Inspect Haskell type errors if template invalid"
       "4. Review Rust CLI logs for execution issues"
       "5. Manually run finishingbot/glambot to isolate issue"
       "6. Check ArangoDB for stored validation state"
       "7. Fix underlying issue and re-validate")))

    (updating-v1-requirements
     (description . "Modify what constitutes v1 readiness")
     (steps
      ("1. Update Logtalk predicates in v1_readiness.lgt"
       "2. Update Haskell types in V1Requirements.hs"
       "3. Update Rust CLI validation logic if needed"
       "4. Test new requirements on all repos"
       "5. Generate migration guide for repos that now fail"
       "6. Deploy new requirements with migration support")))

    (arango-db-maintenance
     (description . "Maintain ArangoDB validation state database")
     (operations
      ((backup . "hypatia db backup --output /backup/hypatia-$(date).dump")
       (restore . "hypatia db restore --input /backup/hypatia-YYYY-MM-DD.dump")
       (purge-old . "hypatia db purge --older-than 90days")
       (reindex . "hypatia db reindex --collection validation_results"))))))

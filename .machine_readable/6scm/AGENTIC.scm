;; SPDX-License-Identifier: PMPL-1.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for hypatia

(define agentic-config
  `((version . "1.0.0")
    (project . "hypatia")

    (patterns
      ((ruleset-development
         (focus . ("logtalk-correctness" "property-based-testing" "formal-verification"))
         (check-for
           ("Verify Logtalk syntax and semantics"
            "Ensure rules terminate (fuel-based proofs)"
            "Test against edge cases"
            "Validate Haskell DSL types align with Logtalk rules")))

       (registry-management
         (workflow
           ("1. Rulesets stored in Git with cryptographic signatures"
            "2. Haskell type system ensures rule validity"
            "3. ArangoDB indexes for fast search"
            "4. Dragonfly caches frequent queries"))
         (constraints
           ("Never bypass signature verification"
            "Always validate rulesets before deployment"
            "Maintain audit trail in ArangoDB")))

       (fleet-coordination
         (description . "Hypatia coordinates gitbot-fleet via rulesets")
         (workflow
           ("1. Hypatia provides verified rulesets to bots"
            "2. Bots report findings back to Hypatia"
            "3. Hypatia analyzes patterns across repos"
            "4. Learning pipeline updates rule priorities")))))

    (constraints
      ((languages
         (primary . "logtalk")
         (verification . "haskell")
         (storage . "arangodb")
         (cache . "dragonfly")
         (adapters . "rust"))

       (banned . ("typescript" "node" "python" "go"))

       (architectural
         ("Rules are immutable once signed"
          "Verification must be reproducible"
          "State machine transitions must be formally proven"
          "No rule can modify another rule at runtime"))))))

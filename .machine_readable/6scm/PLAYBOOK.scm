;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for hypatia

(define playbook
  `((version . "1.0.0")
    (project . "hypatia")

    (procedures
      ((start-registry
         (steps
           ("1. Start ArangoDB: docker-compose up arangodb"
            "2. Start Dragonfly cache: docker-compose up dragonfly"
            "3. Build Haskell registry: cd registry && cabal build"
            "4. Run registry server: cabal run hypatia-registry"))
         (troubleshooting
           ((issue . "ArangoDB connection refused")
            (solution . "Check docker-compose logs, verify port 8529 not in use"))))

       (deploy-ruleset
         (steps
           ("1. Write ruleset in Logtalk (.lgt file)"
            "2. Verify with property tests: hypatia verify ruleset.lgt"
            "3. Push to registry: hypatia push org/ruleset"
            "4. Test on target repo: hypatia apply org/ruleset --dry-run")))

       (debug-rule-engine
         (steps
           ("1. Enable verbose logging: export HYPATIA_LOG=debug"
            "2. Run with trace: logtalk_trace ruleset.lgt"
            "3. Check ArangoDB queries: query profiler in web UI"
            "4. Verify cache hits: dragonfly-cli --stat")))

       (backup-registry
         (steps
           ("1. Export ArangoDB: arangodump --output-directory backup/"
            "2. Backup Git registry: git bundle create backup.bundle --all"
            "3. Store encrypted: gpg -c backup.tar.gz")))

       (respond-to-ruleset-failure
         (steps
           ("1. Check hypatia logs for error details"
            "2. Verify ruleset syntax with hypatia verify"
            "3. Test locally before pushing fix"
            "4. If blocking: temporarily disable ruleset"
            "5. Post-mortem: document root cause in META.scm")))))

    (alerts
      ((high-priority
         (trigger . "Registry unavailable")
         (response
           ("1. Check ArangoDB and Dragonfly health"
            "2. Verify network connectivity"
            "3. Failover to backup registry if configured"))
         (escalation . "Page on-call if >15min downtime"))

       (medium-priority
         (trigger . "Ruleset verification failure rate >10%")
         (response
           ("1. Analyze failing rulesets for patterns"
            "2. Check for Logtalk version incompatibility"
            "3. Review recent ruleset changes"))
         (escalation . "Create tracking issue"))))))

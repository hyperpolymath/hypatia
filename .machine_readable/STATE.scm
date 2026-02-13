;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Project state for hypatia
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.5.0")
    (schema-version "1.0")
    (created "2026-01-03")
    (updated "2026-02-14")
    (project "hypatia")
    (repo "github.com/hyperpolymath/hypatia"))

  (project-context
    (name "hypatia")
    (tagline "Neurosymbolic CI/CD intelligence with safety triangle pipeline")
    (tech-stack
      (primary "Elixir" "Logtalk" "Rust")
      (config "Nickel")
      (state-files "Guile Scheme")
      (database "ArangoDB" "Dragonfly")))

  (current-position
    (phase "operational")
    (overall-completion 97)
    (components
      (verisimdb-connector "complete" "Reads scan data from verisimdb-data repo")
      (pattern-registry "complete" "Deduplicates findings into canonical patterns")
      (recipe-matcher "complete" "Fuzzy matching with language inference")
      (triangle-router "complete" "Eliminate > Substitute > Control hierarchy")
      (dispatch-manifest "complete" "JSONL bridge to execution layer")
      (outcome-tracker "complete" "Records fix results, updates confidence")
      (fleet-dispatcher "complete" "Real file-based + HTTP dispatch with circuit breaker")
      (pattern-analyzer "complete" "Full pipeline: scan → patterns → triangle → dispatch")
      (learning-scheduler "complete" "GenServer polling every 5 min for feedback loop")
      (self-diagnostics "complete" "Health monitoring, circuit breaker, auto-recovery")
      (neural-coordinator "complete" "Orchestrates 5 neural network subsystems")
      (graph-of-trust "complete" "PageRank-style trust over repos/bots/recipes")
      (mixture-of-experts "complete" "Domain-specific confidence with gating network")
      (liquid-state-machine "complete" "Temporal anomaly detection in event streams")
      (echo-state-network "complete" "Confidence trajectory forecasting")
      (radial-neural-network "complete" "Finding similarity and novelty detection")
      (idris2-abi "complete" "Types, GraphQL, gRPC, REST with dependent type proofs")
      (zig-ffi "complete" "C ABI bridge for all Hypatia API operations")
      (elixir-tests "complete" "8 test files covering all pipeline modules")
      (license-compliance "complete" "All SPDX headers updated to PMPL-1.0-or-later")
      (vql-client "complete" "Built-in VQL parser + query cache GenServer")
      (vql-file-executor "complete" "Executes VQL ASTs against verisimdb-data flat files")
      (vql-query "complete" "20+ high-level query functions for pipeline modules")
      (logtalk-rules "active" "Error catalog with 10+ error types")
      (graphql-api "planned" "Fleet coordination API — live HTTP endpoint"))
    (working-features
      ("Safety triangle pipeline: eliminate → substitute → control")
      ("10 fix recipes (6 at 0.99 confidence)")
      ("Fuzzy recipe matching by PA rule prefix + keyword overlap")
      ("Language inference from description text")
      ("Dispatch manifest generation (JSONL)")
      ("Outcome tracking with confidence feedback loop")
      ("Fleet coordinator integration")
      ("Review processor for substitute-tier findings")))

  (route-to-mvp
    (milestones
      (m1 "Safety Triangle Pipeline"
        (status "complete")
        (items
          ("Pattern registry syncs from verisimdb-data scans")
          ("Triangle router: always try eliminate first")
          ("Fuzzy recipe matching bridges fingerprinted IDs to clean recipe IDs")
          ("Language inference detects shell/idris2 from description text")
          ("Dispatch manifest writes JSONL for execution layer")))
      (m2 "Fleet Execution"
        (status "complete")
        (items
          ("385 auto_execute fixes dispatched (0 failures)")
          ("66 promoted recipe fixes (http-to-https, tmp-to-mktemp)")
          ("282 real outcomes recorded")
          ("6 recipes bootstrapped to 0.99 confidence")
          ("86.3% weak point reduction: 3260 → 447")))
      (m3 "Review Path"
        (status "complete")
        (items
          ("481 substitute-tier findings written to pending")
          ("Review processor creates per-repo GitHub issues")
          ("Fleet coordinator routes substitute findings to rhodibot")))
      (m4 "Neural Intelligence"
        (status "complete")
        (items
          ("5 neural networks: Graph of Trust, MoE, LSM, ESN, RBF")
          ("Neural Coordinator GenServer in OTP supervision tree")
          ("Aggregated confidence from MoE + RBF + LSM")
          ("Novelty detection flags unknown finding types")
          ("Temporal anomaly detection in event streams")
          ("Confidence trajectory forecasting and drift detection")))
      (m5 "Formal ABI + FFI"
        (status "complete")
        (items
          ("Idris2 ABI: Types, GraphQL, gRPC, REST definitions")
          ("Dependent type proofs: all ops return ApiResponse-wrapped types")
          ("Zig C ABI bridge: 7 exported functions")
          ("OTP Application with LearningScheduler + SelfDiagnostics + Neural.Coordinator")))
      (m6 "Federated Data Layer"
        (status "complete")
        (items
          ("Elixir ArangoDB client with auto-sync from verisimdb-data every 10 min")
          ("14 document + 9 edge collections in extended E-R model")
          ("New entities: ConfidenceHistory, Contributor, Anomaly, DispatchBatch, NeuralState")
          ("Named graph hypatia_graph for trust traversal and dependency analysis")
          ("Graceful degradation: flat files when ArangoDB unavailable")
          ("Safety: rate limiter (50/min/bot, 200/min global, 10/5s burst)")
          ("Safety: bot quarantine (auto on 5+ failures or >30% FP rate)")
          ("Safety: batch rollback with confidence revert")
          ("Neural persistence: save/load all 5 network states to ArangoDB + flat file")))
      (m7 "VQL Integration"
        (status "complete")
        (items
          ("VQL Client GenServer: built-in Elixir parser (no Deno/Node needed)")
          ("VQL FileExecutor: executes ASTs against verisimdb-data flat files")
          ("VQL Query module: 20+ high-level query functions")
          ("VerisimdbConnector rewritten: VQL-powered with file I/O fallback")
          ("Cross-repo analytics: pattern correlations, outcome timelines, category distribution")
          ("Query caching: 60s TTL, automatic cache eviction")
          ("WHERE clause engine: FIELD conditions, FULLTEXT search, AND/OR combinators")
          ("Modality support: DOCUMENT, TEMPORAL, GRAPH, SEMANTIC sorting")
          ("FEDERATION queries: cross-store pattern matching")
          ("7 GenServers in OTP supervision: VQL, ArangoDB, RateLimiter, Quarantine, Learning, Diag, Neural")))
      (m8 "Production Operations"
        (status "planned")
        (items
          ("Deploy verisim-api server (enables native graph/vector/temporal modalities)")
          ("Deploy ArangoDB + Dragonfly (transitional until verisim-api running)")
          ("Wire GQL-DT Lean types to VQL PROOF clauses")
          ("Train ESN/RBF on accumulated confidence history")
          ("GraphQL API live HTTP endpoint")
          ("SARIF output for IDE integration")
          ("Chapel NIFs for compute-heavy neural operations")
          ("Cross-organization federation with VQL drift policies")))))

  (blockers-and-issues
    (critical ())
    (high
      ("PAT required for automated cross-repo dispatch")
      ("verisim-api not deployed — VQL queries execute against flat files, not native stores")
      ("ArangoDB + Dragonfly need production deployment (transitional until verisim-api)"))
    (medium
      ("447 weak points remaining across 175 repos")
      ("4 recipes below auto_execute threshold")
      ("Neural networks need training data accumulation")
      ("Only 11 recipes for 954 patterns (98.8% without automated fix)")
      ("184 repos in verisimdb-data have NULL summary fields"))
    (low
      ("Codeberg/Bitbucket mirroring blocked")
      ("3 orphaned pattern IDs (954 in registry vs 951 in index)")
      ("GQL-DT isolated — no consumer uses it")))

  (critical-next-actions
    (immediate
      ("Create PAT with repo scope for automated dispatch")
      ("Deploy ArangoDB for graph queries + neural state persistence")
      ("Generate summaries for 184 NULL-summary repos in verisimdb-data"))
    (this-week
      ("Deploy verisim-api server for native VQL store execution")
      ("Train RBF and ESN on collected confidence history")
      ("Develop 3-5 new recipes for high-frequency substitute patterns"))
    (this-month
      ("Wire GQL-DT Lean types to VQL PROOF clauses")
      ("Implement SARIF output for IDE integration")
      ("Build VQL language bindings (ReScript, Rust, Elixir/NIF)")
      ("Implement multi-store VQL federation (currently local file-backed)")
      ("Historical trend tracking across multiple scan cycles")))

  (session-history
    (session "2026-02-13-15"
      (accomplishments
        ("Built VQL Client GenServer with built-in Elixir parser (no Deno/Node dependency)")
        ("Built VQL FileExecutor: executes VQL ASTs against verisimdb-data flat files")
        ("Built VQL Query module: 20+ high-level functions (analytics, trending, health)")
        ("Rewrote VerisimdbConnector: VQL-powered with graceful file I/O fallback")
        ("Added 7 cross-repo analytics: correlations, timelines, coverage, distribution")
        ("WHERE clause engine: FIELD conditions, FULLTEXT search, regex MATCHES")
        ("Query caching with 60s TTL and automatic eviction")
        ("VQL Client added to OTP supervision tree (7 GenServers total)")
        ("Updated CLAUDE.md, STATE.scm, ECOSYSTEM.scm with VQL integration status")))
    (session "2026-02-13-14"
      (accomplishments
        ("Implemented LearningScheduler GenServer — closes feedback loop automatically")
        ("Replaced execute_graphql stub with real file-based + HTTP dispatch")
        ("Added SelfDiagnostics GenServer with circuit breaker pattern")
        ("Created OTP Application supervisor for all GenServers")
        ("Built Idris2 ABI: Types, GraphQL, gRPC, REST with dependent type proofs")
        ("Built Zig FFI C ABI bridge with 7 exported functions")
        ("Implemented 5 neural networks: Graph of Trust, MoE, LSM, ESN, RBF")
        ("Created Neural Coordinator GenServer orchestrating all networks")
        ("Enrolled 298 repos in hypatia scanning pipeline")
        ("Updated all machine-readable documentation")))
    (session "2026-02-13"
      (accomplishments
        ("Fixed AGPL→PMPL across 32 files (29 SPDX headers + 3 SCM files)")
        ("Updated CLAUDE.md: removed stale NOT YET IMPLEMENTED, added pipeline status")
        ("Added .gitignore entries for Elixir (_build/, deps/, erl_crash.dump)")
        ("Created 4 additional test files (fleet_dispatcher, verisimdb_connector, dispatch_manifest, pattern_analyzer)")
        ("Full test coverage: 8 test files for 8 pipeline modules")
        ("Learning loop verified complete: all 6 phases operational")))
    (session "2026-02-12/13"
      (accomplishments
        ("Built safety triangle pipeline end-to-end")
        ("Created 10 fix recipes with proven module mappings")
        ("Implemented fuzzy recipe matching + language inference")
        ("Dispatched 385 auto_execute fixes (0 failures)")
        ("Promoted 2 recipes to 0.99 via 66 additional fixes")
        ("Reduced weak points 86.3%: 3260 → 447")
        ("Wired fleet coordinator to review processor")
        ("Created dispatch-runner.sh and 7 fix scripts")
        ("Recorded 282 outcomes to bootstrap confidence")))
    (session "2026-02-08"
      (accomplishments
        ("Created verisimdb-data connector")
        ("Built pattern detection rules")
        ("Integrated panic-attack scan pipeline")
        ("Tested with 3 pilot repos: echidna, ambientops, verisimdb")))
    (session "2026-01-04"
      (accomplishments
        ("Recovered from crash - assessed current state")
        ("Found existing Logtalk infrastructure")
        ("Identified 3 new error types: ERR-WF-008, 009, 010")))
    (session "2025-12-15"
      (accomplishments
        ("Fixed Dependabot vulnerabilities")
        ("Enabled branch protection on 16 repos")
        ("Added ClusterFuzzLite fuzzing to 4 Rust repos")))
    (session "2025-12-13"
      (accomplishments
        ("Fixed OpenSSF Scorecard issues across 114 workflows")
        ("SHA-pinned all GitHub Actions")))))

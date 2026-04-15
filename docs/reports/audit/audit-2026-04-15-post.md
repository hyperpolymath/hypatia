# Post-audit Status Report: hypatia
- **Date:** 2026-04-15
- **Status:** Complete (M5 Sweep)
- **Repo:** /var/mnt/eclipse/repos/hypatia

## Actions Taken
1. Standard CI/Workflow Sweep: Added blocker workflows (`ts-blocker.yml`, `npm-bun-blocker.yml`) and updated `Justfile`.
2. SCM-to-A2ML Migration: Staged and committed deletions of legacy `.scm` files.
3. Lockfile Sweep: Generated and tracked missing lockfiles where manifests were present.
4. Static Analysis: Verified with `panic-attack assail`.

## Findings Summary
- Possible hardcoded secret in hooks/lib/cache.sh
- 7 unwrap/expect calls in adapters/src/codeberg.rs
- 38 unwrap/expect calls in adapters/tests/adapter_tests.rs
- 86 unwrap/expect calls in integration/tests/ci_simulation_test.rs
- 1 HTTP (non-HTTPS) URLs in integration/tests/forge_test.rs
- 23 unwrap/expect calls in integration/src/ci_simulation/mod.rs
- 103 unwrap/expect calls in integration/src/ci_simulation/scenarios.rs
- Possible hardcoded secret in integration/run-tests.sh
- 6 unwrap/expect calls in cli/src/output.rs
- 21 potentially unquoted variable expansions in scripts/security-check.sh
- Hardcoded /tmp/ path without mktemp in scripts/hypatia-sitrep.sh
- Deno -A (all permissions) in scripts/fix-scripts/fix-deno-permissions.sh
- 3 HTTP (non-HTTPS) URLs in scripts/fix-scripts/fix-http-to-https.sh
- 37 TODO/FIXME/HACK markers in scripts/fix-scripts/fix-todo-markers.sh
- Hardcoded /tmp/ path without mktemp in scripts/ramdisk-orchestrator.sh
- 24 unsafe blocks in clients/rust/hypatia-client/src/ffi.rs
- flake.nix declares inputs without narHash, rev pinning, or sibling flake.lock — dependency revision is unpinned in flake.nix
- System command execution in lib/rules/honest_completion.ex
- System command execution in lib/rules/structural_drift.ex
- System command execution in lib/rules/code_safety.ex
- System command execution in lib/rules/git_state.ex
- System command execution in lib/rules/dependabot_alerts.ex
- System command execution in lib/rules/security_errors.ex
- System command execution in lib/kin/coordinator.ex
- System command execution in lib/scorecard_ingestor.ex
- Dynamic apply/3 in lib/hypatia/web/channels/dispatch_channel.ex
- System command execution in lib/hypatia/cli.ex
- System command execution in lib/outcome_tracker.ex
- System command execution in lib/direct_github_pr.ex
- System command execution in lib/vcl/remote_cache.ex
- 28 potentially unquoted variable expansions in hypatia-cli-bash.sh
- System command execution in test/vcl_remote_cache_test.exs
- Elixir test file test/vcl_remote_cache_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/recipe_matcher_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/triangle_router_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/verisim_connector_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/fleet_dispatcher_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/zig_ffi_smoke_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/dispatch_manifest_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/pattern_analyzer_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- System command execution in test/dependabot_alerts_test.exs
- Elixir test file test/dependabot_alerts_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/e2e_pipeline_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/concurrency_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/recipe_new_recipes_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/recipe_additional_recipes_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/training_pipeline_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/neural_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/root_hygiene_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/honest_completion_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/workflow_audit_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/vcl_file_executor_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/vcl_query_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/blackboard_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/code_safety_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/green_web_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/kin_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/outcome_tracker_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/pattern_registry_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/reviewer_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/safety_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/security_errors_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- System command execution in test/structural_drift_test.exs
- Elixir test file test/structural_drift_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/confidence_annealing_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/cross_repo_learning_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- System command execution in test/direct_github_pr_test.exs
- Elixir test file test/direct_github_pr_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- System command execution in test/git_state_test.exs
- Elixir test file test/git_state_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/rules/proof_strategy_selection_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- Elixir test file test/vcl_client_test.exs uses ExUnit.Case but has no ExUnitProperties/StreamData — add property-based tests to improve mutation coverage
- 33 potentially unquoted variable expansions in hypatia-scanner-v2.sh
- eval usage in tests/e2e.sh
- 1 HTTP (non-HTTPS) URLs in tests/e2e.sh
- Rust project has test infrastructure but no mutation-test configuration (cargo-mutants/.cargo-mutants.toml) — add `cargo mutants` to verify test suite kills mutations

## Final Grade
- **CRG Grade:** D (Promoted from E/X) - CI and lockfiles are in place.

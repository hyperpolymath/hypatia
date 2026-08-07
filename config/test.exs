# SPDX-License-Identifier: MPL-2.0
import Config

# Use a different port for tests to avoid collisions with the running
# dev/production server on port 9090.
#
# Redirect the flat-file store to a throwaway dir under _build (gitignored)
# so the suite never writes into the canonical verisim-data store. Before
# this, OutcomeTracker.record_outcome/5 appended `test-recipe-health-*`
# records to the real outcomes log and dropped annealing-state JSON into a
# git-tracked data dir, polluting the Bayesian/neural training corpus with
# synthetic successes. Both keys point at the same tree; note they are
# distinct config keys (:verisimdb_data_path drives outcomes/recipes,
# :annealing_state_path drives per-recipe temperature files).
test_store = Path.expand("../_build/test/verisim-data", __DIR__)

config :hypatia,
  http_port: 9099,
  verisimdb_data_path: test_store,
  annealing_state_path: Path.join(test_store, "annealing-states"),
  # The diagnostics monitor's 30s tick can fire mid-suite; its recovery
  # path GenServer.stops supervised children (Pipeline/Learning/
  # Coordinator), which reads as random order-dependent test failures.
  # Monitor tests drive checks explicitly via check_now/0.
  diagnostics_periodic_checks: false

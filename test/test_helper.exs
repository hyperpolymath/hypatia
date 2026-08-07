# Start every run from an empty throwaway store.
#
# `config/test.exs` points :verisimdb_data_path and :annealing_state_path at
# `_build/test/verisim-data` (gitignored, generated entirely by the suite).
# Nothing truncated it, so outcome records accumulated across runs — 1400+
# lines by 2026-08-07 — and any test whose synthetic recipe_id collided with
# an earlier run's aggregated both runs' records. Wiping here makes runs
# hermetic and stops the log growing without bound (it is re-read in full on
# every recipe_health/1 call).
for key <- [:verisimdb_data_path, :annealing_state_path] do
  case Application.get_env(:hypatia, key) do
    path when is_binary(path) -> File.rm_rf!(path)
    _ -> :ok
  end
end

ExUnit.start()

# Tests tagged :verisim_data require the verisim-data git-backed flat-file store
# to be cloned and populated by running hypatia-scan across the estate.
# These tests pass in CI (where verisim-data is available) but are excluded
# from local development runs to avoid spurious failures.
# Run with: mix test --include verisim_data
# Pinned seed (#643): two CI runs with zero source changes differed by ±6
# failures from seed alone, making failure-count deltas meaningless as
# evidence. Pass --seed N explicitly to probe order-dependence.
ExUnit.configure(exclude: [:verisim_data], seed: 0)

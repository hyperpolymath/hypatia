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

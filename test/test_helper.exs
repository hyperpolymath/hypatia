ExUnit.start()

# Tests tagged :verisim_data require the verisim-data git-backed flat-file store
# to be cloned and populated by running hypatia-scan across the estate.
# These tests pass in CI (where verisim-data is available) but are excluded
# from local development runs to avoid spurious failures.
# Run with: mix test --include verisim_data
ExUnit.configure(exclude: [:verisim_data])

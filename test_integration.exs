# Test Hypatia integration with verisim-data

IO.puts("=== Hypatia VeriSimDB Integration Test ===\n")

# Fetch all scans
scans = Hypatia.VerisimdbConnector.fetch_all_scans()
IO.puts("✓ Loaded #{length(scans)} scans from verisim-data")

# Generate summary
summary = Hypatia.PatternAnalyzer.generate_summary(scans)
IO.puts("\n=== Summary ===")
IO.puts("Total repos: #{summary.total_repos}")
IO.puts("Total weak points: #{summary.total_weak_points}")

IO.puts("\n=== Per-Repo Details ===")
Enum.each(scans, fn scan ->
  weak_count = length(Map.get(scan.scan, "weak_points", []))
  IO.puts("  #{scan.repo}: #{weak_count} weak points")
end)

# Test pattern analyzer
{:ok, analysis} = Hypatia.PatternAnalyzer.analyze_all_scans()
IO.puts("\n=== Pattern Analysis ===")
IO.puts("Scan count: #{analysis.scan_count}")
IO.puts("Facts file: #{analysis.facts_file}")
IO.puts("\n✓ Integration test complete!")

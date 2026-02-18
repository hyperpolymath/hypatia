# Test Hypatia integration with verisimdb-data

IO.puts("=== Hypatia VeriSimDB Integration Test ===\n")

# Fetch all scans
scans = Hypatia.VerisimdbConnector.fetch_all_scans()
IO.puts("✓ Loaded #{length(scans)} scans from verisimdb-data")

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

# Generate Logtalk facts
echidna_scan = Enum.find(scans, fn s -> s.repo == "echidna" end)
if echidna_scan do
  facts = Hypatia.VerisimdbConnector.to_logtalk_facts(echidna_scan)
  IO.puts("\n=== Logtalk Facts Sample (echidna, first 3 facts) ===")
  facts
  |> String.split("\n")
  |> Enum.take(3)
  |> Enum.each(&IO.puts/1)
end

# Test pattern analyzer
{:ok, analysis} = Hypatia.PatternAnalyzer.analyze_all_scans()
IO.puts("\n=== Pattern Analysis ===")
IO.puts("Scan count: #{analysis.scan_count}")
IO.puts("Facts file: #{analysis.facts_file}")
IO.puts("\n✓ Integration test complete!")

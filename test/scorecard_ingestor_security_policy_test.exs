# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.ScorecardIngestorSecurityPolicyTest do
  # absolute-zero ships SECURITY.adoc; the local Scorecard check demanded the
  # literal SECURITY.md and reported it missing while the CI/CD requirement
  # (fixed in the same sweep) accepted it. Both now share one acceptance set.
  use ExUnit.Case, async: true

  setup do
    dir = Path.join(System.tmp_dir!(), "scorecard-secpol-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    %{dir: dir}
  end

  defp policy_findings(dir) do
    {:ok, findings} = Hypatia.ScorecardIngestor.local_scan(dir, "fixture")
    Enum.filter(findings, &(&1["category"] == "SecurityPolicy"))
  end

  test "no policy document is reported", %{dir: dir} do
    assert [_] = policy_findings(dir)
  end

  for rel <- ["SECURITY.md", "SECURITY.adoc", ".github/SECURITY.rst", "docs/SECURITY.adoc"] do
    test "#{rel} satisfies the check", %{dir: dir} do
      path = Path.join(dir, unquote(rel))
      File.mkdir_p!(Path.dirname(path))
      File.write!(path, "= Security\n")
      assert [] = policy_findings(dir)
    end
  end

  test "an unrelated SECURITY.txt does not", %{dir: dir} do
    File.write!(Path.join(dir, "SECURITY.txt"), "x")
    assert [_] = policy_findings(dir)
  end
end

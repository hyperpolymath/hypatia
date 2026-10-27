# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.ScorecardIngestorLicenseTest do
  # The estate template ships LICENSE.adoc plus REUSE LICENSES/. Root hygiene
  # accepts both; the local Scorecard check still reported SC-010 "No LICENSE
  # file found", so one scan gave contradictory licence results. The REUSE
  # rule is now shared (RootHygiene.reuse_licenses?/1).
  use ExUnit.Case, async: true

  setup do
    dir = Path.join(System.tmp_dir!(), "scorecard-license-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    %{dir: dir}
  end

  defp license_findings(dir) do
    {:ok, findings} = Hypatia.ScorecardIngestor.local_scan(dir, "fixture")
    Enum.filter(findings, &(&1["scorecard_check"] == "License"))
  end

  test "no licence is reported", %{dir: dir} do
    assert [_] = license_findings(dir)
  end

  for name <- ["LICENSE", "LICENSE.md", "LICENSE.adoc", "COPYING"] do
    test "#{name} satisfies the check", %{dir: dir} do
      File.write!(Path.join(dir, unquote(name)), "licence text")
      assert [] = license_findings(dir)
    end
  end

  test "a REUSE LICENSES/ licence text satisfies the check", %{dir: dir} do
    File.mkdir_p!(Path.join(dir, "LICENSES"))
    File.write!(Path.join([dir, "LICENSES", "MPL-2.0.txt"]), "licence text")
    assert [] = license_findings(dir)
  end

  test "a LicenseRef- custom licence text satisfies the check", %{dir: dir} do
    File.mkdir_p!(Path.join(dir, "LICENSES"))
    File.write!(Path.join([dir, "LICENSES", "LicenseRef-Estate-1.0.txt"]), "licence text")
    assert [] = license_findings(dir)
  end

  test "a LICENSES/ file whose name is not an SPDX identifier does not", %{dir: dir} do
    File.mkdir_p!(Path.join(dir, "LICENSES"))
    File.write!(Path.join([dir, "LICENSES", "notes.txt"]), "build notes")
    assert [_] = license_findings(dir)
  end

  test "a LICENSES/ holding only a placeholder does not", %{dir: dir} do
    File.mkdir_p!(Path.join(dir, "LICENSES"))
    File.write!(Path.join([dir, "LICENSES", ".gitkeep"]), "")
    assert [_] = license_findings(dir)
  end
end

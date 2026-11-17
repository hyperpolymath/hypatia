# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.FleetEnrolmentIngestorTest do
  use ExUnit.Case, async: true

  alias Hypatia.FleetEnrolmentIngestor, as: FE

  @caller """
  jobs:
    hypatia:
      uses: metadatastician/standards/.github/workflows/hypatia-scan-reusable.yml@d253de273c5a6f688598fdd241b4c071159029b9
  """

  setup do
    dir = Path.join(System.tmp_dir!(), "fleet-enrol-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, ".github/workflows"))
    on_exit(fn -> File.rm_rf!(dir) end)
    %{dir: dir}
  end

  defp write(dir, rel, body) do
    path = Path.join(dir, rel)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, body)
  end

  test "a grid caller without a manifest is flagged, at review confidence", %{dir: dir} do
    write(dir, ".github/workflows/hypatia-scan.yml", @caller)
    assert {:ok, [p]} = FE.local_scan(dir, "some-repo")
    assert p["category"] == "MissingFleetEnrolment"
    assert p["repos_affected_list"] == ["some-repo"]
    # 0.85-0.94 is the review tier: a pull request, never a direct commit.
    assert p["confidence"] >= 0.85 and p["confidence"] < 0.95
    # A direct caller has no fork-governance.yml for the fixer to put the
    # producing job in, so the fixer cannot finish: not auto-fixable.
    assert p["auto_fixable"] == false
    assert p["remediation"] =~ "Adopt fork governance first"
  end

  test "fork governance with a hypatia job calling the reusable is auto-fixable", %{dir: dir} do
    write(dir, ".github/workflows/fork-governance.yml", "name: Fork governance\n" <> @caller)
    assert {:ok, [p]} = FE.local_scan(dir, "some-repo")
    assert p["auto_fixable"] == true
    assert p["remediation"] =~ "fix-fleet-enrolment.sh"
    refute p["remediation"] =~ "Adopt fork governance"
  end

  # The shape hyperpolymath/mathesis had on 2026-11-17: fork governance with
  # only the DEED and K9 lanes, so no grid is rendered and nothing is flagged.
  test "fork governance without a Hypatia lane is not a grid lane", %{dir: dir} do
    write(
      dir,
      ".github/workflows/fork-governance.yml",
      "name: Fork governance\njobs:\n  deed:\n    name: DEED syntax\n  k9:\n    name: K9-DEED conformance\n"
    )

    assert {:ok, []} = FE.local_scan(dir, "some-repo")
  end

  test "fork governance calling the reusable from a job not named hypatia is not auto-fixable",
       %{dir: dir} do
    write(
      dir,
      ".github/workflows/fork-governance.yml",
      String.replace(@caller, "  hypatia:", "  grid:")
    )

    assert {:ok, [p]} = FE.local_scan(dir, "some-repo")
    assert p["auto_fixable"] == false
  end

  test "an enrolled repository is not flagged, whatever it enrols", %{dir: dir} do
    write(dir, ".github/workflows/hypatia-scan.yml", @caller)
    write(dir, ".gitbot-fleet/enrolment.json", ~s({"reporters":{}}\n))
    assert {:ok, []} = FE.local_scan(dir, "some-repo")
  end

  test "a repository with no grid lane is not flagged", %{dir: dir} do
    write(dir, ".github/workflows/ci.yml", "jobs: {}\n")
    assert {:ok, []} = FE.local_scan(dir, "some-repo")
  end

  test "the reusable named only in an inline comment is not a grid lane", %{dir: dir} do
    write(
      dir,
      ".github/workflows/ci.yml",
      "jobs:\n  a:\n    steps:\n      - uses: actions/checkout@v4 # hypatia-scan-reusable.yml disabled\n"
    )

    assert {:ok, []} = FE.local_scan(dir, "some-repo")
  end

  test "a quoted uses value calling the reusable is a grid lane", %{dir: dir} do
    write(
      dir,
      ".github/workflows/ci.yml",
      ~s(jobs:\n  h:\n    uses: "o/standards/.github/workflows/hypatia-scan-reusable.yml@abc" # pinned\n)
    )

    assert {:ok, [_]} = FE.local_scan(dir, "some-repo")
  end

  test "a commented-out call is not a grid lane", %{dir: dir} do
    write(
      dir,
      ".github/workflows/ci.yml",
      "# uses: x/standards/.github/workflows/hypatia-scan-reusable.yml@abc\n"
    )

    assert {:ok, []} = FE.local_scan(dir, "some-repo")
  end

  test "a .yaml workflow counts as well", %{dir: dir} do
    write(dir, ".github/workflows/grid.yaml", @caller)
    assert {:ok, [_]} = FE.local_scan(dir, "some-repo")
  end

  test "a manifest that is a directory or a symlink is not a manifest", %{dir: dir} do
    write(dir, ".github/workflows/hypatia-scan.yml", @caller)
    File.mkdir_p!(Path.join(dir, ".gitbot-fleet/enrolment.json"))
    assert {:ok, [_]} = FE.local_scan(dir, "some-repo")

    File.rm_rf!(Path.join(dir, ".gitbot-fleet"))
    write(dir, "elsewhere.json", ~s({"reporters":{}}\n))
    File.mkdir_p!(Path.join(dir, ".gitbot-fleet"))
    File.ln_s!("../elsewhere.json", Path.join(dir, ".gitbot-fleet/enrolment.json"))
    assert {:ok, [_]} = FE.local_scan(dir, "some-repo")
  end

  test "the 007 repository is out of scope", %{dir: dir} do
    write(dir, ".github/workflows/hypatia-scan.yml", @caller)
    assert {:ok, []} = FE.local_scan(dir, "007")
  end

  test "a missing path is not flagged" do
    assert {:ok, []} = FE.local_scan("/nonexistent/fleet-enrol", "x")
  end

  test "batch scan aggregates and counts", %{dir: dir} do
    write(dir, ".github/workflows/hypatia-scan.yml", @caller)
    other = dir <> "-enrolled"
    File.mkdir_p!(Path.join(other, ".gitbot-fleet"))
    File.write!(Path.join(other, ".gitbot-fleet/enrolment.json"), "{}\n")
    on_exit(fn -> File.rm_rf!(other) end)

    {patterns, stats} = FE.batch_local_scan([{"a", dir}, {"b", other}])
    assert length(patterns) == 1
    assert stats == %{total_repos: 2, total_findings: 1}
  end

  test "the finding names the path that was inspected", %{dir: dir} do
    write(dir, ".github/workflows/hypatia-scan.yml", @caller)
    assert {:ok, [p]} = FE.local_scan(dir, "some-repo")
    assert p["repo_paths"] == %{"some-repo" => dir}
  end

  test "a manifest that cannot be inspected is skipped, not reported missing", %{dir: dir} do
    write(dir, ".github/workflows/hypatia-scan.yml", @caller)
    # A self-referencing link: lstat of a path under it fails with :eloop,
    # an inspection error that holds even for root (a chmod would not).
    File.ln_s!(".gitbot-fleet", Path.join(dir, ".gitbot-fleet"))
    assert {:unreadable, :eloop} = FE.manifest_state(dir)
    assert {:ok, []} = FE.local_scan(dir, "some-repo")
  end

  test "a missing manifest directory is absent, not unreadable", %{dir: dir} do
    assert FE.manifest_state(dir) == :absent
  end

  test "the category has a fix recipe" do
    assert Hypatia.RecipeGenerator.fix_script_for(FE.category()) == "fix-fleet-enrolment.sh"
  end
end

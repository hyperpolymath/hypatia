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
    assert p["auto_fixable"] == true
  end

  test "the fork governance workflow alone is a grid lane", %{dir: dir} do
    write(dir, ".github/workflows/fork-governance.yml", "name: Fork governance\n")
    assert {:ok, [_]} = FE.local_scan(dir, "some-repo")
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

  test "the category has a fix recipe" do
    assert Hypatia.RecipeGenerator.fix_script_for(FE.category()) == "fix-fleet-enrolment.sh"
  end
end

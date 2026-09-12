# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.ScorecardIngestorActionsLockTest do
  use ExUnit.Case, async: true

  @workflow """
  permissions:
    contents: read
  jobs:
    test:
      steps:
        - uses: actions/checkout@v7.0.1
  """
  @lock """
  version: 'v0.0.2'
  workflows:
      '.github/workflows/ci.yml':
          - 'actions/checkout@v7.0.1'
  dependencies:
      'actions/checkout@v7.0.1':
          ref: 'v7.0.1'
          commit: 'sha1-3d3c42e5aac5ba805825da76410c181273ba90b1'
          owner_id: 44036562
          repo_id: 197814629
  """

  setup do
    dir = Path.join(System.tmp_dir!(), "scorecard-lock-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, ".github/workflows"))
    File.write!(Path.join(dir, ".github/workflows/ci.yml"), @workflow)
    on_exit(fn -> File.rm_rf!(dir) end)
    %{dir: dir}
  end

  defp pinning_findings(dir) do
    {:ok, findings} = Hypatia.ScorecardIngestor.local_scan(dir, "fixture")
    Enum.filter(findings, &(&1["category"] == "DependencyPinning"))
  end

  test "symbolic action with a valid associated lock is pinned", %{dir: dir} do
    File.write!(Path.join(dir, ".github/workflows/actions.lock"), @lock)
    assert pinning_findings(dir) == []
  end

  test "missing lock still reports the unpinned action", %{dir: dir} do
    assert [_] = pinning_findings(dir)
  end

  test "malformed lock fails closed rather than accepting symbolic refs", %{dir: dir} do
    File.write!(Path.join(dir, ".github/workflows/actions.lock"), "version: 'invalid'\n")
    assert [_] = pinning_findings(dir)
  end

  test "a valid lock for a different workflow does not cover this workflow", %{dir: dir} do
    File.write!(
      Path.join(dir, ".github/workflows/actions.lock"),
      String.replace(@lock, "ci.yml", "other.yml")
    )

    assert [_] = pinning_findings(dir)
  end

  test "a valid lock for a different ref does not cover this ref", %{dir: dir} do
    File.write!(
      Path.join(dir, ".github/workflows/actions.lock"),
      String.replace(@lock, "v7.0.1", "v7.0.0")
    )

    assert [_] = pinning_findings(dir)
  end

  test "unpinned sub-actions and branch refs are detected", %{dir: dir} do
    File.write!(
      Path.join(dir, ".github/workflows/ci.yml"),
      String.replace(@workflow, "actions/checkout@v7.0.1", "github/codeql-action/analyze@main")
    )

    assert [_] = pinning_findings(dir)
  end
end

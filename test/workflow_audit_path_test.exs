# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.WorkflowAuditPathTest do
  use ExUnit.Case, async: true

  alias Hypatia.CLI

  # workflow_audit findings used to carry the bare basename (`ci.yml`) as
  # their file, so the uploaded SARIF did not anchor to a real path and
  # `.hypatia-ignore` entries written as `.github/workflows/ci.yml` never
  # matched. Exercised through the real pipeline, not a hand-built finding.

  setup do
    dir = Path.join(System.tmp_dir!(), "hyp-wfpath-#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, ".github/workflows"))
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  test "unpinned_action reports the repo-relative workflow path", %{dir: dir} do
    File.write!(Path.join(dir, ".github/workflows/ci.yml"), """
    name: ci
    on: push
    permissions: {}
    jobs:
      b:
        runs-on: ubuntu-latest
        timeout-minutes: 5
        steps:
          - uses: actions/checkout@v4
    """)

    findings =
      dir
      |> CLI.collect_findings([:workflow_audit])
      |> Enum.filter(&(&1.rule_module == "workflow_audit" and &1.type == "unpinned_action"))

    assert [%{file: ".github/workflows/ci.yml"}] = findings
  end

  test "qualify_workflow_file/1 leaves paths and lists sensible" do
    assert CLI.qualify_workflow_file("ci.yml") == ".github/workflows/ci.yml"
    assert CLI.qualify_workflow_file(".github/workflows/ci.yml") == ".github/workflows/ci.yml"

    assert CLI.qualify_workflow_file(["a.yml", "b.yml"]) ==
             [".github/workflows/a.yml", ".github/workflows/b.yml"]

    assert CLI.qualify_workflow_file("") == ""
  end
end

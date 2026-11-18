# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Wh014WorkflowModelTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowHardening

  defp findings(source) do
    repo = Path.join(System.tmp_dir!(), "wh014-model-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(repo, ".github/workflows"))
    File.write!(Path.join(repo, ".github/workflows/scan.yml"), source)
    on_exit(fn -> File.rm_rf!(repo) end)
    WorkflowHardening.wh014_masked_scanner_upload(repo)
  end

  test "flow jobs and steps use decoded scalar values" do
    assert [%{rule: "WH014", severity: :high, detail: %{jobs: ["scan"]}}] =
             findings(~S"""
             jobs: {scan: {steps: [
               {run: 'hypatia scan . > findings.json || true'},
               {run: 'cp findings.json out.sarif'},
               {uses: github/codeql-action/upload-sarif@v4, with: {sarif_file: out.sarif}}
             ]}}
             """)
  end

  test "folded run value retains a real command separator" do
    assert [%{severity: :high}] =
             findings("""
             jobs:
               scan:
                 steps:
                   - run: >-
                       hypatia scan . > findings.json || true;
                       cp findings.json out.sarif
                   - uses: github/codeql-action/upload-sarif@v4
                     with: {sarif_file: out.sarif}
             """)
  end

  test "folding does not invent a command separator" do
    assert [] =
             findings("""
             jobs:
               scan:
                 steps:
                   - run: >-
                       hypatia scan . > findings.json || true
                       cp findings.json out.sarif
                   - uses: github/codeql-action/upload-sarif@v4
                     with: {sarif_file: out.sarif}
             """)
  end

  test "quoted mapping keys are structural keys" do
    assert [%{severity: :high}] =
             findings("""
             'jobs':
               'scan':
                 'steps':
                   - 'run': hypatia scan . > findings.json || true
                   - 'run': cp findings.json out.sarif
                   - 'uses': github/codeql-action/upload-sarif@v4
                     'with': {'sarif_file': out.sarif}
             """)
  end

  test "aliased jobs are reported at their use sites" do
    assert [%{line: 7, detail: %{jobs: ["scan"]}}] =
             findings("""
             x-job: &scanner
               steps:
                 - {run: 'hypatia scan . > findings.json || true'}
                 - {run: 'cp findings.json out.sarif'}
                 - {uses: github/codeql-action/upload-sarif@v4, with: {sarif_file: out.sarif}}
             jobs:
               scan: *scanner
             """)
  end

  test "a run key in step env is data" do
    assert [] =
             findings("""
             jobs:
               scan:
                 steps:
                   - env:
                       run: hypatia scan . > findings.json || true
                     run: echo ready
                   - run: cp findings.json out.sarif
                   - uses: github/codeql-action/upload-sarif@v4
                     with:
                       sarif_file: out.sarif
             """)
  end

  test "parsed defaults apply at workflow and job scope" do
    for {job_defaults, upload_path} <- [
          {"{}", "scanner/out.sarif"},
          {"{run: {working-directory: reports}}", "reports/out.sarif"}
        ] do
      assert [%{severity: :high}] =
               findings("""
               defaults: {run: {working-directory: scanner}}
               jobs:
                 scan:
                   defaults: #{job_defaults}
                   steps:
                     - run: hypatia scan . > findings.json || true
                     - {run: 'cp findings.json out.sarif'}
                     - uses: github/codeql-action/upload-sarif@v4
                       with: {sarif_file: #{upload_path}}
               """)
    end
  end

  test "an unreadable workflow produces a located incomplete-analysis finding" do
    assert [%{severity: :warn, line: 2, detail: %{analysis_status: :unreadable}}] =
             findings("jobs: {}\njobs: {}\n")
  end
end

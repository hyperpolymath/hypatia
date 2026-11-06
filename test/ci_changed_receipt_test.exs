# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.CiChangedReceiptTest do
  use ExUnit.Case, async: true
  alias Hypatia.CiChanged

  defp repo do
    directory =
      Path.join(
        System.tmp_dir!(),
        "hypatia-ci-receipt-#{System.pid()}-#{System.os_time(:nanosecond)}"
      )

    File.mkdir_p!(Path.join(directory, ".github/workflows"))
    on_exit(fn -> File.rm_rf!(directory) end)
    git(directory, ["init", "-q"])
    directory
  end

  defp git(repo, args) do
    assert {_, 0} = System.cmd("git", args, cd: repo, stderr_to_stdout: true)
  end

  defp commit(repo) do
    git(repo, ["add", "."])

    git(repo, [
      "-c",
      "commit.gpgsign=false",
      "-c",
      "user.name=Test",
      "-c",
      "user.email=test@example.org",
      "commit",
      "--allow-empty",
      "-qm",
      "baseline"
    ])
  end

  defp row(result, extra \\ %{}) do
    Jason.encode!(
      Map.merge(
        %{"jobID" => "build", "jobResult" => result, "dryrun" => false, "matrix" => %{}},
        extra
      )
    )
  end

  test "only observed, non-dry-run results for the selected job certify completion" do
    assert CiChanged.receipt(row("success"), "build", 0) == :completed
    assert CiChanged.receipt(row("skipped"), "build", 0) == :skipped
    assert CiChanged.receipt("", "build", 0) == :unobserved
    assert CiChanged.receipt(row("success", %{"jobID" => "other"}), "build", 0) == :unobserved
    assert CiChanged.receipt(row("success", %{"dryrun" => true}), "build", 0) == :unobserved
    assert CiChanged.receipt(row("success"), "build", 1) == :failed

    assert CiChanged.receipt(
             row("success") <> "\n" <> row("skipped", %{"matrix" => %{"os" => "other"}}),
             "build",
             0
           ) == :skipped

    message = Jason.encode!(%{"jobID" => "build", "msg" => row("success"), "dryrun" => false})
    assert CiChanged.receipt(message, "build", 0) == :unobserved
  end

  test "source-only changes conservatively select existing jobs with their supported event" do
    repo = repo()

    workflow =
      "on: workflow_dispatch\njobs:\n  build:\n    runs-on: ubuntu-latest\n    steps:\n      - run: bash scripts/input.sh\n"

    File.write!(Path.join(repo, ".github/workflows/ci.yml"), workflow)
    File.mkdir_p!(Path.join(repo, "scripts"))
    File.write!(Path.join(repo, "scripts/input.sh"), "exit 0\n")
    commit(repo)
    File.write!(Path.join(repo, "scripts/input.sh"), "exit 1\n")

    assert %{jobs: [%{job: "build", event: "workflow_dispatch", local_blocker: nil}]} =
             CiChanged.plan(repo, "HEAD")

    assert_raise ArgumentError, fn -> CiChanged.plan(repo, "HEAD", "pull_request") end
  end

  test "deleting prerequisites and cyclic needs are rejected even when no surviving job changed" do
    before = "on: push\njobs: {a: {steps: []}, b: {needs: a, steps: []}}\n"
    after_text = "on: push\njobs: {b: {needs: a, steps: []}}\n"
    assert_raise ArgumentError, fn -> CiChanged.changed_jobs(before, after_text) end

    assert_raise ArgumentError, fn ->
      CiChanged.changed_jobs(nil, "on: push\njobs: {a: {needs: b}, b: {needs: a}}\n")
    end
  end

  test "changed checkout inputs require hosted validation rather than a skipped checkout receipt" do
    repo = repo()

    workflow =
      "on: push\njobs:\n  build:\n    steps:\n      - uses: actions/checkout@v7\n        with: {fetch-depth: 1}\n      - run: echo ok\n"

    File.write!(Path.join(repo, ".github/workflows/ci.yml"), workflow)
    commit(repo)

    File.write!(
      Path.join(repo, ".github/workflows/ci.yml"),
      String.replace(workflow, "fetch-depth: 1", "fetch-depth: 0")
    )

    plan = CiChanged.plan(repo, "HEAD")
    assert [%{local_blocker: blocker}] = plan.jobs
    assert blocker =~ "checkout"

    assert [%{outcome: :requires_hosted_receipt}] =
             CiChanged.execute_jobs(repo, plan.jobs, fn _, _, _ ->
               flunk("must not simulate changed checkout")
             end)
  end

  test "workflow-directory absence findings remain in scope" do
    repo = repo()

    File.write!(
      Path.join(repo, ".github/workflows/ci.yml"),
      "on: push\njobs: {build: {steps: [{run: echo ok}]}}\n"
    )

    findings = CiChanged.scan(repo, [".github/workflows/ci.yml"])
    assert Enum.any?(findings, &(&1.type == "missing_secret_history_scan"))
  end

  @tag :baseline_helper
  test "canonical baseline removes acknowledgements, applies overrides and rejects malformed input" do
    repo = repo()

    helper =
      System.get_env("HYPATIA_BASELINE_HELPER") ||
        Path.expand("../.standards/scripts/apply-baseline.sh", __DIR__)

    assert File.regular?(helper), "provision the governance-pinned baseline helper"

    baseline = %{
      severity: "high",
      rule_module: "code_safety",
      type: "elixir_atom_from_user",
      file: "lib/input.ex",
      expires_at: "2099-01-01",
      note: "fixture"
    }

    finding = %{
      severity: "high",
      rule_module: "code_safety",
      type: "elixir_atom_from_user",
      file: "lib/input.ex",
      reason: "fixture"
    }

    path = Path.join(repo, ".hypatia-baseline.json")
    File.write!(path, Jason.encode!([baseline]))
    assert CiChanged.apply_baseline(repo, [finding], helper) == []
    File.write!(path, Jason.encode!([Map.put(baseline, :severity_override, "low")]))
    assert [%{severity: "low"}] = CiChanged.apply_baseline(repo, [finding], helper)
    File.write!(path, "{}")
    assert_raise ArgumentError, fn -> CiChanged.apply_baseline(repo, [finding], helper) end

    assert_raise ArgumentError, fn ->
      CiChanged.apply_baseline(repo, [finding], "/missing/helper")
    end
  end
end

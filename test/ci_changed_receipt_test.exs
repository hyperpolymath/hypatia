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
      "on: workflow_dispatch\npermissions: read-all\njobs:\n  build:\n    runs-on: ubuntu-latest\n    steps:\n      - run: bash scripts/input.sh\n"

    File.write!(Path.join(repo, ".github/workflows/ci.yml"), workflow)
    File.mkdir_p!(Path.join(repo, "scripts"))
    File.write!(Path.join(repo, "scripts/input.sh"), "exit 0\n")
    commit(repo)
    File.write!(Path.join(repo, "scripts/input.sh"), "exit 1\n")

    assert %{jobs: [%{job: "build", event: "workflow_dispatch", local_blocker: nil}]} =
             CiChanged.plan(repo, "HEAD")

    assert_raise ArgumentError, fn -> CiChanged.plan(repo, "HEAD", "pull_request") end
  end

  test "executor refuses every incomplete receipt outcome" do
    jobs = [%{workflow: "ci.yml", job: "build", event: "push", local_blocker: nil}]

    for {output, exit, expected} <- [
          {row("success"), 1, :failed},
          {"", 0, :unobserved},
          {row("failure"), 0, :failed},
          {row("skipped"), 0, :skipped},
          {row("success", %{"dryrun" => true}), 0, :unobserved},
          {row("success"), 0, :completed}
        ] do
      assert [%{outcome: ^expected}] =
               CiChanged.execute_jobs("/repo", jobs, fn _, _, _ -> {output, exit} end)
    end
  end

  test "transitive prerequisites retain hosted validation blockers" do
    repo = repo()

    for ancestor <- [
          "permissions: {contents: write}\n    steps: []",
          "uses: owner/repo/.github/workflows/reuse.yml@main",
          "steps: [{uses: actions/checkout@v7, with: {fetch-depth: 1}}]"
        ] do
      workflow =
        "on: push\npermissions: read-all\njobs:\n  setup:\n    #{ancestor}\n  middle:\n    needs: setup\n    steps: []\n  build:\n    needs: middle\n    steps: [{run: echo before}]\n"

      File.write!(Path.join(repo, ".github/workflows/ci.yml"), workflow)
      commit(repo)

      File.write!(
        Path.join(repo, ".github/workflows/ci.yml"),
        workflow
        |> String.replace("echo before", "echo after")
        |> String.replace("fetch-depth: 1", "fetch-depth: 0")
      )

      plan = CiChanged.plan(repo, "HEAD")
      selected = Enum.filter(plan.jobs, &(&1.job == "build"))
      assert [%{local_blocker: blocker}] = selected
      assert blocker =~ "prerequisite setup"

      assert [%{outcome: :requires_hosted_receipt}] =
               CiChanged.execute_jobs(repo, selected, fn _, _, _ ->
                 flunk("must not run prerequisites")
               end)
    end
  end

  test "root composite action findings retain their actual path" do
    repo = repo()

    File.write!(
      Path.join(repo, "action.yml"),
      "runs:\n  using: composite\n  steps:\n    - uses: owner/action@main\n"
    )

    findings = CiChanged.scan(repo, ["action.yml"])
    assert Enum.any?(findings, &(&1.type == "RE006" and &1.file == "action.yml"))
  end

  test "changed prerequisites select their transitive consumers" do
    workflow =
      "on: push\njobs: {build: {steps: [{run: before}]}, test: {needs: build, steps: []}, deploy: {needs: test, steps: []}, unrelated: {steps: []}}\n"

    assert CiChanged.changed_jobs(workflow, String.replace(workflow, "before", "after")) == [
             "build",
             "deploy",
             "test"
           ]
  end

  test "changed reusable contracts select unchanged local callers" do
    repo = repo()

    reusable =
      "on: workflow_call\npermissions: read-all\njobs: {build: {steps: [{run: before}]}}\n"

    File.write!(Path.join(repo, ".github/workflows/reusable.yml"), reusable)

    File.write!(
      Path.join(repo, ".github/workflows/caller.yml"),
      "on: push\npermissions: read-all\njobs: {call: {uses: './.github/workflows/reusable.yml'}, consumer: {needs: call, steps: []}}\n"
    )

    commit(repo)

    File.write!(
      Path.join(repo, ".github/workflows/reusable.yml"),
      String.replace(reusable, "before", "after")
    )

    jobs = CiChanged.plan(repo, "HEAD").jobs

    assert Enum.any?(
             jobs,
             &(&1.workflow == ".github/workflows/caller.yml" and &1.job == "call" and
                 &1.local_blocker =~ "reusable")
           )

    assert Enum.any?(jobs, &(&1.job == "consumer" and &1.local_blocker =~ "prerequisite call"))
  end

  test "inherited permissions, conditions and secrets cannot earn local receipts" do
    repo = repo()
    commit(repo)

    for {permissions, body, expected} <- [
          {"", "steps: []", "inherited"},
          {"permissions: read-all\n", "steps: [{run: echo ok, if: false}]", "conditional"},
          {"permissions: read-all\n", "if: false\n    steps: []", "conditional"},
          {"permissions: read-all\n", "env: {TOKEN: '${{ secrets.TOKEN }}'}\n    steps: []",
           "credential"},
          {"permissions: read-all\n", "steps: [{run: echo ok, continue-on-error: true}]",
           "tolerated"}
        ] do
      File.write!(
        Path.join(repo, ".github/workflows/ci.yml"),
        "on: push\n#{permissions}jobs:\n  build:\n    #{body}\n"
      )

      assert [%{local_blocker: blocker}] = CiChanged.plan(repo, "HEAD").jobs
      assert blocker =~ expected
    end
  end

  test "removed and repositioned checkouts require hosted validation" do
    repo = repo()

    workflow =
      "on: push\npermissions: read-all\njobs: {build: {steps: [{uses: actions/checkout@v7}, {run: echo ok}]}}\n"

    File.write!(Path.join(repo, ".github/workflows/ci.yml"), workflow)
    commit(repo)

    for replacement <- [
          "steps: [{run: echo ok}]",
          "steps: [{run: echo ok}, {uses: actions/checkout@v7}]"
        ] do
      File.write!(
        Path.join(repo, ".github/workflows/ci.yml"),
        String.replace(
          workflow,
          "steps: [{uses: actions/checkout@v7}, {run: echo ok}]",
          replacement
        )
      )

      assert [%{local_blocker: blocker}] = CiChanged.plan(repo, "HEAD").jobs
      assert blocker =~ "checkout"
    end
  end

  test "malformed current steps fail clearly and a corrected malformed base can be planned" do
    repo = repo()
    base = "on: push\npermissions: read-all\njobs: {build: {steps: null}}\n"
    File.write!(Path.join(repo, ".github/workflows/ci.yml"), base)
    commit(repo)

    assert_raise ArgumentError, ~r/steps must be a sequence/, fn ->
      CiChanged.plan(repo, "HEAD")
    end

    File.write!(Path.join(repo, ".github/workflows/ci.yml"), String.replace(base, "null", "[]"))
    assert [%{local_blocker: blocker}] = CiChanged.plan(repo, "HEAD").jobs
    assert blocker =~ "checkout"
  end

  test "whole secrets contexts and non-string scalar map keys are handled without JSON encoding" do
    repo = repo()
    commit(repo)

    File.write!(
      Path.join(repo, ".github/workflows/ci.yml"),
      "on: push\npermissions: read-all\njobs: {build: {env: {TOKEN: '${{ toJSON(secrets) }}'}, steps: []}}\n"
    )

    assert [%{local_blocker: blocker}] = CiChanged.plan(repo, "HEAD").jobs
    assert blocker =~ "credential"

    File.write!(
      Path.join(repo, ".github/workflows/ci.yml"),
      "on: push\npermissions: read-all\njobs: {build: {env: {3: value}, steps: []}}\n"
    )

    assert [%{local_blocker: nil}] = CiChanged.plan(repo, "HEAD").jobs
  end

  test "deleting prerequisites and cyclic needs are rejected even when no surviving job changed" do
    before = "on: push\npermissions: read-all\njobs: {a: {steps: []}, b: {needs: a, steps: []}}\n"
    after_text = "on: push\npermissions: read-all\njobs: {b: {needs: a, steps: []}}\n"
    assert_raise ArgumentError, fn -> CiChanged.changed_jobs(before, after_text) end

    assert_raise ArgumentError, fn ->
      CiChanged.changed_jobs(
        nil,
        "on: push\npermissions: read-all\njobs: {a: {needs: b}, b: {needs: a}}\n"
      )
    end
  end

  test "changed checkout inputs require hosted validation rather than a skipped checkout receipt" do
    repo = repo()

    workflow =
      "on: push\npermissions: read-all\njobs:\n  build:\n    steps:\n      - uses: actions/checkout@v7\n        with: {fetch-depth: 1}\n      - run: echo ok\n"

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
      "on: push\npermissions: read-all\njobs: {build: {steps: [{run: echo ok}]}}\n"
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

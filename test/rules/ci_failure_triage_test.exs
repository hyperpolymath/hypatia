# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.CiFailureTriageTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CiFailureTriage, as: T
  alias Hypatia.Rules.WorkflowAudit

  @dir Path.expand("../fixtures/ci_failure_triage", __DIR__)

  # The replay corpus: one log per failure seen in the estate, the rule that
  # should be primary, and the action it should lead to. Logs under `real-`
  # are excerpts of hypatia's own failed runs; the others are reconstructed
  # from the reported signature (the line that matters is the reported text).
  @corpus [
    {"rate-limit-setup-bun.txt", "api_rate_limit", :rerun},
    {"rate-limit-then-tests-red.txt", "api_rate_limit", :rerun},
    {"merge-ref-race.txt", "merge_ref_race", :rerun},
    {"invalid-lockfile-pin.txt", "actions_lock_invalid_pin", :fix},
    {"cancelled-deposit.txt", "runner_cancelled", :none},
    {"app-id-empty.txt", "secret_or_variable_missing", :owner},
    {"app-key-openssh.txt", "app_key_wrong_format", :owner},
    {"rsr-score-segfault.txt", "toolchain_segfault", :owner},
    {"srht-outage.txt", "external_host_unreachable", :rerun},
    {"canon-lockstep.txt", "canon_lockstep", :fix},
    {"bench-regression.txt", "bench_regression", :owner},
    {"exunit-failure.txt", "test_failure", :report},
    # Real excerpts from hypatia's failed runs, 2026-11-12/13 (run id in the name).
    {"real-43367835718-dead-pat.txt", "token_invalid", :owner},
    {"real-43358533401-graphql-502.txt", "api_server_error", :rerun},
    {"real-43137798791-lock-unresolvable.txt", "actions_lock_unresolvable_pin", :fix},
    {"real-43158586027-gitleaks-range.txt", "scan_range_missing_commit", :rerun},
    {"real-43357534889-rollup.txt", "rollup_cascade", :upstream},
    {"real-43416333174-orchestrate-raise.txt", "runtime_exception", :report},
    {"real-43150955344-rustfmt.txt", "format_diff", :fix},
    {"real-43357534889-container-build.txt", "compile_error", :report}
  ]

  describe "replay corpus" do
    for {file, rule, action} <- @corpus do
      test "#{file} -> #{rule} (#{action})" do
        %{primary: p} = T.classify(File.read!(Path.join(@dir, unquote(file))))
        assert p.rule == T.id(unquote(rule))
        assert p.action == unquote(action)
        assert is_binary(p.evidence) and p.evidence != ""
      end
    end

    test "every corpus file is listed, so a new fixture cannot go unchecked" do
      listed = MapSet.new(@corpus, &elem(&1, 0))
      on_disk = @dir |> File.ls!() |> Enum.filter(&String.ends_with?(&1, ".txt"))
      unlisted = Enum.reject(on_disk, &(MapSet.member?(listed, &1) or &1 == "clean-pass.txt"))
      assert unlisted == []
    end

    test "a passing log has no finding" do
      assert %{primary: nil, matches: []} =
               T.classify(File.read!(Path.join(@dir, "clean-pass.txt")))
    end
  end

  describe "precedence" do
    test "a platform failure outranks the test failure it caused" do
      %{primary: p, matches: m} =
        T.classify(File.read!(Path.join(@dir, "rate-limit-then-tests-red.txt")))

      assert p.rule == T.id("api_rate_limit")
      assert T.id("test_failure") in Enum.map(m, & &1.rule)
    end

    test "cancellation outranks the missing artifact it caused" do
      %{primary: p} = T.classify(File.read!(Path.join(@dir, "cancelled-deposit.txt")))
      assert p.rule == T.id("runner_cancelled")
    end

    test "rules/0 lists layers in order and every id is <domain>/<name> without wildcards" do
      rules = T.rules()
      layers = Enum.map(rules, & &1.layer) |> Enum.dedup()
      assert layers == [:platform, :config, :toolchain, :code]

      for r <- rules do
        assert r.id =~ ~r{^hypatia/ci_failure_triage/[a-z0-9_]+$}
        refute String.contains?(r.id, "*")
      end
    end

    test "classification is a pure function of its input" do
      log = File.read!(Path.join(@dir, "merge-ref-race.txt"))
      assert T.classify(log) == T.classify(log)
    end
  end

  describe "context" do
    test "a merge-ref race on a merged PR is a stale run, not a rerun" do
      log = File.read!(Path.join(@dir, "merge-ref-race.txt"))
      assert T.classify(log, %{pr_state: :merged}).primary.action == :none
      assert T.classify(log, %{pr_state: :open}).primary.action == :rerun
    end

    test "conclusion cancelled is authoritative with an empty log" do
      assert T.classify("", %{conclusion: "cancelled"}).primary.rule == T.id("runner_cancelled")
    end

    test "exit code 139 with no log line is a segfault" do
      assert T.classify("", %{exit_code: 139}).primary.rule == T.id("toolchain_segfault")
    end
  end

  describe "loop limit" do
    test "a transient rule reruns within budget, then escalates as persistent" do
      %{primary: p} = T.classify(File.read!(Path.join(@dir, "rate-limit-setup-bun.txt")))
      assert %{action: :rerun, persistent: false} = T.decide(p, 0)
      assert %{action: :rerun, persistent: false} = T.decide(p, 1)
      assert %{action: :owner, persistent: true} = T.decide(p, 2)
      assert %{action: :owner, persistent: true} = T.decide(p, 50)
    end

    test "every budget is at most 2, so a rerun loop always ends" do
      assert Enum.all?(T.rules(), &(&1.retries <= 2))
      assert Enum.all?(T.rules(), &(&1.action == :rerun or &1.retries == 0))
    end

    test "a fix or owner decision never becomes a rerun" do
      %{primary: p} = T.classify(File.read!(Path.join(@dir, "invalid-lockfile-pin.txt")))
      assert T.decide(p, 0).action == :fix
    end

    test "nothing in the forbidden list is an action any rule takes" do
      actions = T.rules() |> Enum.map(& &1.action) |> MapSet.new()
      assert MapSet.disjoint?(actions, MapSet.new(T.forbidden_actions()))
    end
  end

  describe "required check never reports" do
    test "a check that never ran goes to the owner" do
      [f] = T.check_gate(["CodeQL", "Tests"], ["Tests"])
      assert f.rule == T.id("required_check_never_reported")
      assert f.check == "CodeQL"
      assert f.near_match == nil
      assert f.action == :owner
    end

    test "case drift between the required name and the job name is named" do
      [f] = T.check_gate(["codeql"], ["CodeQL", "Tests"])
      assert f.near_match == "CodeQL"
    end

    test "all reported, nothing to say" do
      assert T.check_gate(["Tests"], ["Tests", "Lint"]) == []
    end
  end

  describe "scanner pin drift" do
    @a "410a8f821ea8297833162565b50a298f787815da"
    @b "b689a74d2762a1c19839ae3fbd59b7fe5ef920ea"

    test "env pins and reusable input defaults at different commits all report" do
      contents = %{
        "fleet/report-back.yml" => "env:\n  PANIC_ATTACK_SHA: #{@b}\n",
        "standards/hypatia-scan-reusable.yml" =>
          "on:\n  workflow_call:\n    inputs:\n      panic-ref:\n        description: x\n        type: string\n        default: #{@a}\n"
      }

      found = T.check_scanner_pin_drift(contents)
      assert length(found) == 2
      assert Enum.all?(found, &(&1.pinned == Enum.sort([@a, @b])))
      assert Enum.all?(found, &(&1.action == :owner))
    end

    test "one commit everywhere is not drift" do
      contents = %{"a.yml" => "  PA_SHA: #{@a}\n", "b.yml" => "  PANIC_ATTACK_SHA: #{@a}\n"}
      assert T.check_scanner_pin_drift(contents) == []
    end
  end

  describe "WF029 always() after cancel" do
    @deposit """
    on: pull_request
    jobs:
      scan:
        runs-on: ubuntu-latest
        steps:
          - run: echo scan
      deposit:
        runs-on: ubuntu-latest
        needs: [scan]
        if: always()
        steps:
          - uses: actions/download-artifact@v8.0.2
            with:
              name: findings
    """

    test "fires on an always() job that downloads its needs' artifacts" do
      [f] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => @deposit})
      assert f.rule == "WF029"
      assert f.rule_id == "hypatia/ci_failure_triage/always_after_cancel"
      assert f.job == "deposit"
    end

    test "!cancelled() is the fix and does not fire" do
      fixed = String.replace(@deposit, "if: always()", "if: ${{ !cancelled() }}")
      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => fixed}) == []
    end

    test "an always() job with no artifact download (a status rollup) does not fire" do
      rollup =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n        with:\n          name: findings\n",
          "      - run: echo rollup\n"
        )

      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => rollup}) == []
    end

    test "is part of audit/3" do
      %{findings: findings} = WorkflowAudit.audit(["gate.yml"], %{"gate.yml" => @deposit})
      assert Enum.any?(findings, &(&1[:rule] == "WF029"))
    end
  end
end

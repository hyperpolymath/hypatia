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
    {"rulesets-unreadable.txt", "github_state_unreadable", :rerun},
    {"workflow-catalogue-unreadable.txt", "github_state_unreadable", :rerun},
    {"callee-unreadable.txt", "github_state_unreadable", :rerun},
    {"mirror-ssh-key-empty.txt", "ssh_key_secret_empty", :owner},
    {"rate-limit-then-tests-red.txt", "api_rate_limit", :rerun},
    {"merge-ref-race.txt", "merge_ref_race", :report},
    {"invalid-lockfile-pin.txt", "actions_lock_invalid_pin", :fix},
    {"cancelled-deposit.txt", "runner_cancelled", :none},
    {"app-id-empty.txt", "secret_or_variable_missing", :owner},
    {"app-key-openssh.txt", "app_key_wrong_format", :owner},
    {"rsr-score-segfault.txt", "toolchain_segfault", :owner},
    {"srht-outage.txt", "external_host_unreachable", :rerun},
    {"canon-lockstep.txt", "canon_lockstep", :owner},
    {"bench-regression.txt", "bench_regression", :owner},
    {"exunit-failure.txt", "test_failure", :report},
    # Doctrine 16 sweep, 2026-11-13: Pages off (xmlpull-zig, sholes), bench over
    # its limit (unified-api-adapter), findings artifact missing after a
    # scanner SIGSEGV (bactrian, quintessence).
    {"pages-not-enabled.txt", "pages_not_enabled", :owner},
    {"bench-over-limit.txt", "bench_regression", :owner},
    {"findings-artifact-after-segfault.txt", "upstream_artifact_missing", :upstream},
    # Experiment-owner intake, 2026-11-13 (rule 16 sweep cases): scanner
    # segfault (bactrian, quintessence), Hypatia criticals (puggle), lock
    # missing a ref (this-ssg), canon lockstep (EpistemicTypes.jl, dictask),
    # stale REPOSITORY-MAP (dictask), superseded deed (rsr-template-repo#337).
    {"hypatia-cli-segfault.txt", "toolchain_segfault", :owner},
    {"hypatia-critical-findings.txt", "security_findings_reported", :owner},
    {"lock-missing-ref.txt", "actions_lock_missing_ref", :fix},
    {"lock-missing-pin.txt", "actions_lock_missing_ref", :fix},
    # Tsunami-breaker, 2026-11-14: tag-keyed lock records refused by the
    # standards pin-contract gate (ZeroInflatedCounts.jl, ResidualEvidenceTypes.jl).
    {"lock-tag-keyed.txt", "actions_lock_tag_keyed", :owner},
    {"canon-lockstep-broken.txt", "canon_lockstep", :owner},
    {"canon-pin-needs-profile.txt", "canon_lockstep", :owner},
    {"repo-map-stale.txt", "repository_map_stale", :fix},
    # Batch 2 (cases from 8 sessions): CodeQL, Dependabot, cargo audit, bots.
    {"codeql-no-python.txt", "codeql_no_source", :owner},
    # Case-12 excerpts (rule-16 sweep session, all red on the base).
    {"ruleset-blocks-push.txt", "ruleset_blocks_push", :owner},
    {"uuid-non-v8.txt", "uuid_conformance", :owner},
    {"kyaml-format.txt", "kyaml_format", :fix},
    {"sonar-not-authorized.txt", "sonar_not_authorized", :owner},
    {"capnp-generated-e0451.txt", "compile_error", :report},
    {"codeql-config-error.txt", "codeql_setup_conflict", :owner},
    {"dependabot-bun.txt", "dependabot_ecosystem_mismatch", :fix},
    {"real-131888629871-cargo-audit.txt", "dependency_advisory", :report},
    {"coderabbit-review-limit.txt", "review_rate_limited", :none},
    {"codex-credits.txt", "review_quota_exhausted", :none},
    {"deed-cancelled.txt", "runner_cancelled", :none},
    # Real excerpts from hypatia's failed runs, 2026-11-12/13 (run id in the name).
    {"real-43367835718-dead-pat.txt", "token_invalid", :owner},
    {"real-43358533401-graphql-502.txt", "api_server_error", :rerun},
    {"real-43137798791-lock-unresolvable.txt", "actions_lock_unresolvable_pin", :fix},
    {"real-43158586027-gitleaks-range.txt", "scan_range_missing_commit", :rerun},
    {"real-43357534889-rollup.txt", "rollup_cascade", :upstream},
    # hypatia#1091: the CI Status rollup on a superseded head, red only
    # because a newer push cancelled the job it waits on.
    {"real-43601615541-rollup-cancelled.txt", "runner_cancelled", :none},
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

    test "rules/0 is the whole inventory, including the non-log rules" do
      names = T.rules() |> Enum.map(& &1.id) |> MapSet.new()

      for n <- ["required_check_never_reported", "scanner_pin_drift", "always_after_cancel"] do
        assert T.id(n) in names
      end

      assert Enum.all?(
               T.rules(),
               &(&1.route in ["first-line", "referral", "owner-only", "noise"])
             )
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

  describe "signatures are not over-broad" do
    test "an application cancellation exception is not a runner cancellation" do
      log =
        "  1) test x\n     ** (TaskCanceledException) The operation was canceled.\n5 tests, 1 failure\n"

      %{primary: p, matches: m} = T.classify(log)
      # Attributed to the code, not written off as a cancelled run.
      assert p.layer == :code
      assert p.action != :none
      refute T.id("runner_cancelled") in Enum.map(m, & &1.rule)
    end

    test "an app's own API rate-limit message is not GitHub throttling" do
      log =
        "  1) test quota\n     ** (RuntimeError) API rate limit exceeded for tenant 7\n5 tests, 1 failure\n"

      %{primary: p, matches: m} = T.classify(log)
      assert p.layer == :code
      refute T.id("api_rate_limit") in Enum.map(m, & &1.rule)
    end

    test "an app test that names a GitHub read is not the gate refusing" do
      log =
        "  1) test rulesets\n     ** (RuntimeError) could not read branch rulesets\n" <>
          "  2) test catalogue\n     Assertion failed: could not read the workflow catalogue\n" <>
          "5 tests, 2 failures\n"

      %{primary: p, matches: m} = T.classify(log)
      assert p.layer == :code
      refute T.id("github_state_unreadable") in Enum.map(m, & &1.rule)
    end

    test "an unresolved service name under test is not an external host" do
      log =
        "curl: (6) Could not resolve host: backend\n  1) test api\n5 tests, 1 failure\n"

      %{primary: p, matches: m} = T.classify(log)
      assert p.layer == :code
      refute T.id("external_host_unreachable") in Enum.map(m, & &1.rule)
    end

    test "an app's bare HTTP 401 Bad credentials is not a GitHub token" do
      log =
        "  1) test login\n     ** (RuntimeError) HTTP 401: Bad credentials\n5 tests, 1 failure\n"

      %{matches: m} = T.classify(log)
      refute T.id("token_invalid") in Enum.map(m, & &1.rule)
    end

    test "a repository URL elsewhere in the log is not GitHub API context" do
      log =
        "Cloning https://github.com/org/repo\n  1) test quota\n     ** (RuntimeError) API rate limit exceeded\n5 tests, 1 failure\n"

      %{matches: m} = T.classify(log)
      refute T.id("api_rate_limit") in Enum.map(m, & &1.rule)
    end

    test "an advisory id in a failing test is not an audit failure" do
      log = "  1) test advisory parser reads RUSTSEC-2026-0538\n5 tests, 1 failure\n"
      %{primary: p, matches: m} = T.classify(log)
      assert p.rule == T.id("test_failure")
      refute T.id("dependency_advisory") in Enum.map(m, & &1.rule)
    end

    test "an app's own empty ssh-private-key message is not the agent secret" do
      log =
        "  1) test cli\n     ** (ArgumentError) The ssh-private-key argument is empty\n5 tests, 1 failure\n"

      %{matches: m} = T.classify(log)
      refute T.id("ssh_key_secret_empty") in Enum.map(m, & &1.rule)
    end

    test "a dotted unresolved host is still external" do
      %{primary: p} = T.classify("curl: (6) Could not resolve host: registry.npmjs.org\n")
      assert p.rule == T.id("external_host_unreachable")
    end

    test "Codex quota outside a review stays actionable" do
      log =
        "agent step: You've reached your Codex usage limits\nError: Process completed with exit code 1.\n"

      %{matches: m} = T.classify(log)
      refute T.id("review_quota_exhausted") in Enum.map(m, & &1.rule)
    end

    test "a missing client-id input without an App token action is not the App secret rule" do
      log = "Error: Input required and not supplied: client-id\n"
      %{matches: m} = T.classify(log)
      refute T.id("secret_or_variable_missing") in Enum.map(m, & &1.rule)
    end

    test "a bare HTTP 502 from an application is not a GitHub API outage" do
      log = "  1) test proxy\n     ** (RuntimeError) HTTP 502: Bad Gateway\n5 tests, 1 failure\n"
      %{primary: p, matches: m} = T.classify(log)
      assert p.layer == :code
      refute T.id("api_server_error") in Enum.map(m, & &1.rule)
    end

    test "git bad object from a test is not a scan-range miss" do
      log =
        "fatal: bad object 41901363fcd775bb9628acf571bbba93cca38420\n  1) test x\n5 tests, 1 failure\n"

      %{matches: m} = T.classify(log)
      refute T.id("scan_range_missing_commit") in Enum.map(m, & &1.rule)
    end

    test "gitleaks bad object is a scan-range miss" do
      log = "ERR [git] fatal: bad object 41901363fcd775bb9628acf571bbba93cca38420\n"
      assert T.classify(log).primary.rule == T.id("scan_range_missing_commit")
    end

    test "an app's own failure text does not trip platform/config rules" do
      for line <- [
            "checkout failed (state=cancelled)",
            "Server Error (HTTP 502)",
            "fatal: ambiguous argument 'abc1234..def5678': unknown revision",
            "login test: Bad credentials"
          ] do
        log = line <> "\n  1) test x\n5 tests, 1 failure\n"
        %{primary: p} = T.classify(log)
        assert p.layer == :code, "#{line} -> #{inspect(p.rule)}"
      end
    end

    test "a namespaced Elixir exception is a runtime exception" do
      %{primary: p} = T.classify("** (Jason.DecodeError) unexpected byte\n")
      assert p.rule == T.id("runtime_exception")
    end

    test "key-decoder errors need a GitHub App token step in the log" do
      log =
        "##[error]error:1E08010C:DECODER routines::unsupported\n  1) test x\n5 tests, 1 failure\n"

      refute T.id("app_key_wrong_format") in Enum.map(T.classify(log).matches, & &1.rule)
    end

    test "an app's own 'Review limit reached' is not CodeRabbit" do
      log = "Review limit reached for user 7\n  1) test x\n5 tests, 1 failure\n"
      assert T.classify(log).primary.layer == :code
    end

    test "gate and pins findings carry a route" do
      [g] = T.check_gate(["lint"], [])
      assert g.route == "owner-only"
      a = String.duplicate("a", 40)
      b = String.duplicate("b", 40)

      [p | _] =
        T.check_scanner_pin_drift(%{"x.yml" => "  PA_SHA: #{a}\n", "y.yml" => "  PA_SHA: #{b}\n"})

      assert p.route == "owner-only"
    end

    test "a loopback connection failure is the app under test, not a host outage" do
      log =
        "curl: (7) Failed to connect to localhost port 3000\n  1) test x\n5 tests, 1 failure\n"

      assert T.classify(log).primary.layer == :code

      assert T.classify("curl: (7) Failed to connect to git.sr.ht port 443\n").primary.rule ==
               T.id("external_host_unreachable")
    end

    test "a doctest-only failure is a test failure" do
      assert T.classify("Finished in 0.1 seconds\n1 doctest, 1 failure\n").primary.rule ==
               T.id("test_failure")
    end

    test "'Not authorized' without a Sonar run is not a Sonar setting" do
      log = "Not authorized or organization not found\n  1) test x\n5 tests, 1 failure\n"
      assert T.classify(log).primary.layer == :code
    end

    test "gh api with HTTP 502 is a GitHub API outage" do
      %{primary: p} = T.classify("gh api graphql: HTTP 502: Bad Gateway\n")
      assert p.rule == T.id("api_server_error")
    end

    test "git 401/403 is a refused credential, not an unreachable host" do
      log =
        "fatal: unable to access 'https://github.com/acme/private/': The requested URL returned error: 403\n"

      %{primary: p} = T.classify(log)
      assert p.rule == T.id("token_invalid")
      assert p.action == :owner
    end

    test "a 5xx from the same host is still an outage" do
      log =
        "fatal: unable to access 'https://git.sr.ht/~x/y/': The requested URL returned error: 502\n"

      assert T.classify(log).primary.rule == T.id("external_host_unreachable")
    end

    test "a segfault naming no known tool is the program's own crash" do
      log =
        "/tmp/run.sh: line 3: 99 Segmentation fault      ./target/release/my_app\n##[error]Process completed with exit code 139.\n"

      %{primary: p} = T.classify(log)
      assert p.rule == T.id("process_segfault")
      assert p.layer == :code
      assert p.action == :report
    end

    test "a segfault naming the runtime is a toolchain fault" do
      log =
        "/tmp/run.sh: line 1: 22 Segmentation fault      (core dumped) mix hypatia.rsr_score .\n"

      assert T.classify(log).primary.rule == T.id("toolchain_segfault")
    end

    test "a rate-limit header on a successful response is not a rate-limit failure" do
      log = "< x-ratelimit-remaining: 0\n200 OK\n5 tests, 1 failure\n"
      assert T.classify(log).primary.rule == T.id("test_failure")
    end

    test "a 5xx from the application under test is not an external outage" do
      log = "curl: (22) The requested URL returned error: 500\n5 tests, 1 failure\n"
      refute T.id("external_host_unreachable") in Enum.map(T.classify(log).matches, & &1.rule)
    end

    test "a crash of the hypatia binary itself is the program's, not the toolchain's" do
      log = "/tmp/run.sh: line 2: 7 Segmentation fault      ./hypatia scan .\n"
      assert T.classify(log).primary.rule == T.id("process_segfault")
    end

    test "a panic input with no default does not borrow a sibling's default" do
      contents = %{
        "r.yml" =>
          "on:\n  workflow_call:\n    inputs:\n      panic-ref:\n        type: string\n" <>
            "      pons-ref:\n        type: string\n        default: bca1ea289a4f8a86bac6212731e6f8ec15c67ad4\n",
        "s.yml" => "  PA_SHA: 410a8f821ea8297833162565b50a298f787815da\n"
      }

      assert T.check_scanner_pin_drift(contents) == []
    end

    test "scanner pins differing only in case are the same commit" do
      sha = "410a8f821ea8297833162565b50a298f787815da"

      contents = %{
        "a.yml" => "  PA_SHA: #{sha}\n",
        "b.yml" => "  PA_SHA: #{String.upcase(sha)}\n"
      }

      assert T.check_scanner_pin_drift(contents) == []
    end
  end

  describe "context" do
    test "a merge-ref race is never a rerun: noise when merged, a referral when open" do
      log = File.read!(Path.join(@dir, "merge-ref-race.txt"))
      merged = T.classify(log, %{pr_state: :merged}).primary
      assert merged.action == :none
      assert merged.route == "noise"
      open = T.classify(log, %{pr_state: :open}).primary
      assert open.action == :report
      assert open.route == "referral"
    end

    test "conclusion cancelled is authoritative with an empty log" do
      assert T.classify("", %{conclusion: "cancelled"}).primary.rule == T.id("runner_cancelled")
    end

    test "a rollup naming a failed job is not read as a cancellation" do
      failed = "##[error]kyaml-conformance did not succeed (result: failure)\n"
      %{matches: ms} = T.classify(failed, %{})
      refute Enum.any?(ms, &(&1.rule == T.id("runner_cancelled")))
    end

    test "a program printing the rollup phrase is not read as a cancellation" do
      log =
        "worker did not succeed (result: cancelled)\n" <>
          "  1) test frobnicates (FooTest)\n     Assertion with == failed\n"

      %{matches: ms} = T.classify(log, %{})
      refute Enum.any?(ms, &(&1.rule == T.id("runner_cancelled")))
    end

    test "a quoted runner annotation mid-line does not outrank a test failure" do
      log =
        "  expected \"##[error]worker did not succeed (result: cancelled)\"\n" <>
          "  1) test frobnicates (FooTest)\n     Assertion with == failed\n" <>
          "5 tests, 1 failure\n"

      %{primary: p, matches: ms} = T.classify(log, %{})
      refute Enum.any?(ms, &(&1.rule == T.id("runner_cancelled")))
      refute p.rule == T.id("runner_cancelled")
      assert p.layer == :code
    end

    test "a timestamped runner annotation is still read as a cancellation" do
      log = "2026-11-14T13:52:24.3285804Z ##[error]lint did not succeed (result: cancelled)\n"
      %{primary: p} = T.classify(log, %{})
      assert p.rule == T.id("runner_cancelled")
    end

    test "startup_failure with no log is a referral, never a rerun" do
      %{primary: p} = T.classify("", %{conclusion: "startup_failure"})
      assert p.rule == T.id("workflow_startup_failure")
      assert p.action == :report
    end

    test "a generated file other than REPOSITORY-MAP is referred, not run" do
      %{primary: p} = T.classify("FAIL: committed docs/INDEX.adoc is stale — run: just index\n")
      assert p.rule == T.id("generated_file_stale")
      assert p.action == :report
    end

    test "a nonzero exit with no known signature is reported, never clean" do
      %{primary: p} =
        T.classify("Traceback (most recent call last):\nValueError: bad\n", %{exit_code: 1})

      assert p.rule == T.id("unrecognized_failure")
      assert p.action == :report
    end

    test "exit code 0 with no signature stays clean" do
      assert %{primary: nil} = T.classify("all good\n", %{exit_code: 0})
    end

    test "exit code 139 with no log line is a crash, attributed to the program" do
      %{primary: p} = T.classify("", %{exit_code: 139})
      assert p.rule == T.id("process_segfault")
    end
  end

  describe "loop limit" do
    test "a negative attempt count is refused" do
      f = T.classify("gh: HTTP 502\n").primary
      assert_raise ArgumentError, fn -> T.decide(f, -1) end
    end

    test "a transient rule reruns within budget, then escalates as persistent" do
      %{primary: p} = T.classify(File.read!(Path.join(@dir, "rate-limit-setup-bun.txt")))
      assert %{action: :rerun, persistent: false} = T.decide(p, 0)
      assert %{action: :owner, persistent: true, route: "owner-only"} = T.decide(p, 1)
      assert %{action: :owner, persistent: true} = T.decide(p, 50)
    end

    test "every budget is at most 1, matching the squabbler's one re-run per job" do
      assert Enum.all?(T.rules(), &(&1.retries <= 1))
      assert Enum.all?(T.rules(), &(&1.action == :rerun or &1.retries == 0))
    end

    test "a fix or owner decision never becomes a rerun" do
      %{primary: p} = T.classify(File.read!(Path.join(@dir, "invalid-lockfile-pin.txt")))
      assert T.decide(p, 0).action == :fix
    end

    test "security findings may never be closed or baselined by a rule" do
      assert :close_or_baseline_security_finding in T.forbidden_actions()
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

    test "a CRLF block-form input default is a pin" do
      contents = %{
        "a.yml" =>
          String.replace(
            "inputs:\n  panic-ref:\n    type: string\n    default: #{@a}\n",
            "\n",
            "\r\n"
          ),
        "b.yml" => "env:\n  PA_SHA: #{@b}\n"
      }

      assert length(T.check_scanner_pin_drift(contents)) == 2
    end

    test "a quoted default key in a flow mapping is a pin" do
      contents = %{
        "a.yml" => "inputs:\n  panic-ref: {\"default\": #{@a}}\n",
        "b.yml" => "env:\n  PA_SHA: #{@b}\n"
      }

      assert length(T.check_scanner_pin_drift(contents)) == 2
    end

    test "a quoted hash before a flow pin does not hide the pin" do
      contents = %{
        "a.yml" => "env: {NOTE: \"use # carefully\", PA_SHA: #{@a}}\n",
        "b.yml" => "env:\n  PA_SHA: #{@b}\n"
      }

      assert length(T.check_scanner_pin_drift(contents)) == 2
    end

    test "a quoted pin key is still a pin" do
      contents = %{
        "a.yml" => "env:\n  \"PA_SHA\": #{@a}\n",
        "b.yml" => "env:\n  PA_SHA: #{@b}\n"
      }

      assert length(T.check_scanner_pin_drift(contents)) == 2
    end

    test "YAML flow-mapping pins are read" do
      contents = %{
        "a.yml" => "env: {PA_SHA: #{@a}}\n",
        "b.yml" =>
          "on:\n  workflow_call:\n    inputs:\n      panic-ref: {type: string, default: #{@b}}\n"
      }

      assert length(T.check_scanner_pin_drift(contents)) == 2
    end

    test "a quoted default key is still a pin" do
      contents = %{
        "a.yml" => "  PA_SHA: #{@a}\n",
        "b.yml" =>
          "on:\n  workflow_call:\n    inputs:\n      panic-ref:\n        type: string\n        \"default\": #{@b}\n"
      }

      assert length(T.check_scanner_pin_drift(contents)) == 2
    end

    test "a commented-out old pin is not a second pin" do
      contents = %{
        "a.yml" => "# old pin: {PA_SHA: #{@b}}\nenv:\n  PA_SHA: #{@a}\n",
        "b.yml" => "  PA_SHA: #{@a}\n"
      }

      assert T.check_scanner_pin_drift(contents) == []
    end

    test "one commit everywhere is not drift" do
      contents = %{"a.yml" => "  PA_SHA: #{@a}\n", "b.yml" => "  PANIC_ATTACK_SHA: #{@a}\n"}
      assert T.check_scanner_pin_drift(contents) == []
    end
  end

  describe "actions.lock stale after a bump" do
    @lock """
    version: 'v0.0.3'
    workflows:
        '.github/workflows/ci.yml':
            - 'actions/checkout@v7.0.1'
    dependencies:
        'actions/checkout@v7.0.1':
            ref: 'v7.0.1'
            commit: 'sha1-1111111111111111111111111111111111111111'
            owner_id: 44036562
            repo_id: 197814629
    """

    test "a workflow bumped past its lock entry is a stale lock, not an unpinned action" do
      wf = %{
        "ci.yml" =>
          "jobs:\n  b:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: actions/checkout@v7.0.2\n"
      }

      case Hypatia.Rules.ActionsLock.parse(@lock) do
        {:ok, _} ->
          findings = WorkflowAudit.check_unpinned_actions(wf, actions_lock: @lock)
          assert [%{type: :actions_lock_stale, locked_ref: "actions/checkout@v7.0.1"}] = findings
          refute Enum.any?(findings, &(&1.type == :unpinned_action))

        {:error, reason} ->
          flunk("fixture lock did not parse: #{inspect(reason)}")
      end
    end

    @tag :tmp_dir
    test "a standalone hardening scan keeps a stale ref, with the regenerate remedy", %{
      tmp_dir: dir
    } do
      wf_dir = Path.join([dir, ".github", "workflows"])
      File.mkdir_p!(wf_dir)
      File.write!(Path.join(wf_dir, "actions.lock"), @lock)

      File.write!(
        Path.join(wf_dir, "ci.yml"),
        "jobs:\n  b:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: actions/checkout@v7.0.2\n"
      )

      [f] = Hypatia.Rules.WorkflowHardening.wh004_unpinned_uses(dir)
      assert f.rule == "WH004"
      assert f.action == :regenerate_actions_lock
      assert f.detail.kind == :actions_lock_stale
      assert f.detail.locked_ref == "actions/checkout@v7.0.1"
      refute f.reason =~ "40-char commit SHA"
    end

    @tag :tmp_dir
    test "a combined scan keeps hardening's unreadable-lock finding", %{tmp_dir: dir} do
      wf_dir = Path.join([dir, ".github", "workflows"])
      File.mkdir_p!(Path.join(wf_dir, "actions.lock"))

      File.write!(
        Path.join(wf_dir, "ci.yml"),
        "jobs:\n  b:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: actions/checkout@v7.0.2\n"
      )

      found = Hypatia.CLI.collect_findings(dir, [:workflow_audit, :workflow_hardening])
      assert Enum.any?(found, &(&1.type == "invalid_actions_lock"))
    end

    @tag :tmp_dir
    test "a combined scan reports a missing permissions block once", %{tmp_dir: dir} do
      wf_dir = Path.join([dir, ".github", "workflows"])
      File.mkdir_p!(wf_dir)

      File.write!(
        Path.join(wf_dir, "ci.yml"),
        "on: push\njobs:\n  b:\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo ok\n"
      )

      found = Hypatia.CLI.collect_findings(dir, [:workflow_audit, :workflow_hardening])
      assert Enum.count(found, &(&1.type == "missing_permissions")) == 1
      refute Enum.any?(found, &(&1.type == "WH002"))

      alone = Hypatia.CLI.collect_findings(dir, [:workflow_hardening])
      assert Enum.any?(alone, &(&1.type == "WH002"))
    end

    test "the locked ref itself is still pinned" do
      wf = %{
        "ci.yml" =>
          "jobs:\n  b:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: actions/checkout@v7.0.1\n"
      }

      assert WorkflowAudit.check_unpinned_actions(wf, actions_lock: @lock) == []
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
          - uses: actions/upload-artifact@v7.0.0
            with:
              name: findings
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
      assert f.action == :fix
    end

    test "a job with literal continue-on-error: true cannot fail the run" do
      tol = String.replace(@deposit, "if: always()", "if: always()\n    continue-on-error: true")
      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => tol}) == []
    end

    test "a run-id composed from github.run_id is this run" do
      composed = @deposit <> "          run-id: ${{ format('{0}', github.run_id) }}\n"
      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => composed})
    end

    test "always() || cancelled() is not a guard and still fires" do
      pos = String.replace(@deposit, "if: always()", "if: ${{ always() || cancelled() }}")
      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => pos})
    end

    test "always() || !cancelled() on the job is not a guard" do
      pos = String.replace(@deposit, "if: always()", "if: ${{ always() || !cancelled() }}")
      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => pos})
    end

    test "an unrelated comparison beside !cancelled() keeps the guard" do
      g =
        String.replace(
          @deposit,
          "if: always()",
          "if: ${{ always() && !cancelled() && github.event_name == 'push' }}"
        )

      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => g}) == []
    end

    test "!cancelled() inside a quoted string is not a guard" do
      g = String.replace(@deposit, "if: always()", "if: ${{ always() && 'x && !cancelled()' }}")
      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => g})
    end

    test "always() && !cancelled() on the job is a guard" do
      g = String.replace(@deposit, "if: always()", "if: ${{ always() && !cancelled() }}")
      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => g}) == []
    end

    test "a step guarded by always() || !cancelled() still counts" do
      step =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n",
          "      - if: ${{ always() || !cancelled() }}\n        uses: actions/download-artifact@v8.0.2\n"
        )

      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => step})
    end

    test "a comparison around !cancelled() is not a guard" do
      job =
        String.replace(@deposit, "if: always()", "if: ${{ always() && (!cancelled() == false) }}")

      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => job})

      step =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n",
          "      - if: ${{ !cancelled() == false }}\n        uses: actions/download-artifact@v8.0.2\n"
        )

      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => step})
    end

    test "!cancelled() inside a string literal is not a guard" do
      job =
        String.replace(
          @deposit,
          "if: always()",
          "if: ${{ always() && 'x && !cancelled() && y' }}"
        )

      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => job})

      step =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n",
          "      - if: ${{ 'x && !cancelled()' }}\n        uses: actions/download-artifact@v8.0.2\n"
        )

      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => step})
    end

    test "a download step with continue-on-error: true does not fire" do
      tolerated =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n",
          "      - continue-on-error: true\n        uses: actions/download-artifact@v8.0.2\n"
        )

      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => tolerated}) == []
    end

    test "step-level success() is not a guard (true at the first step)" do
      step =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n",
          "      - if: success()\n        uses: actions/download-artifact@v8.0.2\n"
        )

      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => step})
    end

    test "an artifact no needed job uploads does not fire" do
      other =
        String.replace(
          @deposit,
          "          name: findings\n  deposit:",
          "          name: other\n  deposit:"
        )

      assert other =~ "name: other"
      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => other}) == []
    end

    test "an artifact uploaded by a transitive need still fires" do
      chain =
        String.replace(
          @deposit,
          "  deposit:",
          "  mid:\n    runs-on: ubuntu-latest\n    needs: [scan]\n    steps:\n      - run: echo mid\n  deposit:"
        )
        |> String.replace(
          "needs: [scan]\n    if: always()",
          "needs: [mid]\n    if: always()"
        )

      # Guard the fixture edits themselves: deposit now needs only mid.
      assert chain =~ "needs: [mid]\n    if: always()"
      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => chain})
    end

    test "a needed reusable-workflow job is a possible producer" do
      reusable =
        String.replace(
          @deposit,
          "  scan:\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo scan\n      - uses: actions/upload-artifact@v7.0.0\n        with:\n          name: findings\n",
          "  scan:\n    uses: org/repo/.github/workflows/scan.yml@v1\n"
        )

      assert reusable =~ "uses: org/repo/.github/workflows/scan.yml@v1"
      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => reusable})
    end

    test "a download step guarded by !cancelled() does not fire" do
      guarded =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n",
          "      - if: ${{ !cancelled() }}\n        uses: actions/download-artifact@v8.0.2\n"
        )

      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => guarded}) == []
    end

    test "the action identity is matched case-insensitively" do
      upper = String.replace(@deposit, "actions/download-artifact", "Actions/Download-Artifact")
      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => upper})
    end

    test "!cancelled() is the fix and does not fire" do
      fixed = String.replace(@deposit, "if: always()", "if: ${{ !cancelled() }}")
      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => fixed}) == []
    end

    test "a cross-run download (run-id) is not this run's artifact" do
      cross =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n        with:\n          name: findings\n",
          "      - uses: actions/download-artifact@v8.0.2\n        with:\n          name: findings\n          run-id: 12345\n          github-token: x\n"
        )

      assert WorkflowAudit.check_always_after_cancel(%{"gate.yml" => cross}) == []
    end

    test "run-id set to this run still counts as a current-run download" do
      this_run =
        String.replace(
          @deposit,
          "      - uses: actions/download-artifact@v8.0.2\n        with:\n          name: findings\n",
          "      - uses: actions/download-artifact@v8.0.2\n        with:\n          name: findings\n          run-id: ${{ github.run_id }}\n"
        )

      assert [_] = WorkflowAudit.check_always_after_cancel(%{"gate.yml" => this_run})
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

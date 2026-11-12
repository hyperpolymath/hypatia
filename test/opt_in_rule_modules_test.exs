# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.OptInRuleModulesTest do
  @moduledoc """
  supply_chain, dogfooding, audit_prevention and branch_protection had
  detectors and tests but no scan caller. They are now reachable through
  `--rules`, and only through it: the default set must not grow until
  canary scans have cleared their false positives.
  """
  use ExUnit.Case, async: false

  alias Hypatia.CLI

  @opt_in [:supply_chain, :dogfooding, :audit_prevention, :branch_protection]

  setup do
    repo = Path.join(System.tmp_dir!(), "hyp-opt-in-#{System.unique_integer([:positive])}")
    wf = Path.join(repo, ".github/workflows")
    File.mkdir_p!(wf)
    File.mkdir_p!(Path.join(repo, "src"))
    File.mkdir_p!(Path.join(repo, "scripts/fix-scripts"))
    # Stale licence name in a live workflow: HYP-DOG-003.
    File.write!(Path.join(wf, "build.yml"), "name: build\n# AGPL-3.0\non: push\n")
    # The same name in a fix script, a default exemption: suppressed.
    File.write!(Path.join(repo, "scripts/fix-scripts/fix.sh"), "# AGPL-3.0\n")
    # Rust crate, no cargo audit, no test workflow: AP003 and AP005.
    File.write!(Path.join(repo, "Cargo.toml"), "[package]\nname = \"x\"\n")
    File.write!(Path.join(repo, "src/main.rs"), "fn main() {}\n")

    saved = for k <- ["GITHUB_TOKEN", "HYPATIA_DISPATCH_PAT"], do: {k, System.get_env(k)}
    Enum.each(saved, fn {k, _} -> System.delete_env(k) end)

    on_exit(fn ->
      File.rm_rf!(repo)
      Enum.each(saved, fn {k, v} -> if v, do: System.put_env(k, v) end)
    end)

    %{repo: repo}
  end

  test "the default rule set does not include the opt-in modules" do
    assert Enum.all?(@opt_in, &(&1 not in CLI.parse_rules(nil)))
  end

  test "--rules accepts each opt-in module" do
    assert CLI.parse_rules("supply_chain,branch_protection") == [
             :supply_chain,
             :branch_protection
           ]
  end

  test "the default rule set emits no opt-in findings", %{repo: repo} do
    modules = repo |> CLI.collect_findings(CLI.parse_rules(nil)) |> Enum.map(& &1.rule_module)
    assert Enum.all?(@opt_in, &(to_string(&1) not in modules))
  end

  test "dogfooding findings are repo-relative, so exemptions apply", %{repo: repo} do
    findings = CLI.collect_findings(repo, [:dogfooding])
    files = for f <- findings, f.type == "HYP-DOG-003", do: f.file

    assert ".github/workflows/build.yml" in files
    refute Enum.any?(files, &String.starts_with?(&1, "scripts/fix-scripts/"))
  end

  test "audit_prevention gets file and workflow evidence", %{repo: repo} do
    types = repo |> CLI.collect_findings([:audit_prevention]) |> Enum.map(& &1.type)
    assert "AP003" in types
    assert "AP005" in types

    File.write!(Path.join(repo, ".github/workflows/test.yml"), "steps:\n  - run: cargo audit\n")
    types = repo |> CLI.collect_findings([:audit_prevention]) |> Enum.map(& &1.type)
    refute "AP003" in types
    refute "AP005" in types
  end

  test "supply_chain findings carry the normalised shape", %{repo: repo} do
    [f | _] = CLI.collect_findings(repo, [:supply_chain])
    assert f.rule_module == "supply_chain"
    assert f.type =~ ~r/^SC\d{3}$/
    assert is_binary(f.severity) and is_binary(f.reason)
  end

  test "branch_protection without a token contributes nothing", %{repo: repo} do
    assert CLI.collect_findings(repo, [:branch_protection]) == []
  end

  test "scan_all_estate_policies accepts owner_repo without crashing", %{repo: repo} do
    # Was BranchProtection.scan(owner, repo): the repo name landed in opts
    # and Keyword.get/2 raised.
    assert %{findings: findings} =
             Hypatia.Rules.scan_all_estate_policies(repo, owner_repo: {"o", "r"})

    refute Enum.any?(findings, &String.starts_with?(to_string(&1.rule), "BP"))
  end
end

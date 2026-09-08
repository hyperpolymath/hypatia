# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ResearchExtensionsWiringTest do
  @moduledoc """
  RE001-RE010 landed in PR #325 (2026-05-26) with a full test suite and
  no caller. `test/research_extensions_test.exs` proves the *rules* work;
  these tests prove the *wiring*, which is what was missing:

    * `Hypatia.CLI.collect_findings/2` actually reaches the module,
    * the normalized finding shape survives the seam,
    * RE004's line - which the module nests under `:detail` - is carried
      through rather than dropped,
    * the `:warn` tier renders as a real SARIF level instead of falling
      to the catch-all.

  Without these, a future refactor can unwire the module and every rule
  test still passes.
  """
  use ExUnit.Case, async: true

  alias Hypatia.CLI
  alias Hypatia.Rules
  alias Hypatia.SARIF

  @tmp_dir System.tmp_dir!()

  # Trips RE001 (touches `secrets.*` with no harden-runner; :warn, no line)
  # and RE004 (`docker://` pinned by tag; :warn, line nested under :detail).
  @tripwire """
  name: Deploy
  on: [push]
  jobs:
    deploy:
      runs-on: ubuntu-latest
      steps:
        - uses: docker://alpine:3.21
        - run: deploy --token=${{ secrets.DEPLOY_KEY }}
  """

  defp tripwire_repo do
    repo = Path.join(@tmp_dir, "re_wiring_#{System.unique_integer([:positive])}")
    wf = Path.join([repo, ".github", "workflows"])
    File.mkdir_p!(wf)
    File.write!(Path.join(wf, "deploy.yml"), @tripwire)
    on_exit(fn -> File.rm_rf!(repo) end)
    repo
  end

  defp re004(repo) do
    repo
    |> CLI.collect_findings([:research_extensions])
    |> Enum.find(&(&1.type == "RE004"))
  end

  describe "CLI.collect_findings/2 reaches ResearchExtensions" do
    test "the :research_extensions branch emits normalized findings" do
      findings = CLI.collect_findings(tripwire_repo(), [:research_extensions])

      refute findings == [],
             "collect_findings/2 returned nothing for :research_extensions - the branch is unwired"

      assert Enum.all?(findings, &(&1.rule_module == "research_extensions"))
      assert Enum.all?(findings, &is_binary(&1.severity))
      assert Enum.all?(findings, &is_binary(&1.action))
      assert "RE004" in Enum.map(findings, & &1.type)
    end

    test "a different rule module does not emit research_extensions findings" do
      findings = CLI.collect_findings(tripwire_repo(), [:code_safety])
      refute Enum.any?(findings, &(&1.rule_module == "research_extensions"))
    end

    test "a repo with no workflows produces no research_extensions findings" do
      clean = Path.join(@tmp_dir, "re_clean_#{System.unique_integer([:positive])}")
      File.mkdir_p!(clean)
      on_exit(fn -> File.rm_rf!(clean) end)

      assert CLI.collect_findings(clean, [:research_extensions]) == []
    end
  end

  describe ":line carry-through" do
    test "RE004's line, nested under :detail, survives normalization" do
      f = re004(tripwire_repo())

      assert f, "RE004 did not fire on the tripwire workflow"

      assert is_integer(f.line) and f.line > 0,
             "ResearchExtensions nests RE004's line under :detail; the " <>
               "normalizer must reach it, not just Map.get(f, :line)"
    end

    test "the carried line renders as a non-degenerate SARIF startLine" do
      repo = tripwire_repo()
      f = re004(repo)

      [result] =
        [f]
        |> SARIF.from_findings(repo)
        |> Map.fetch!("runs")
        |> hd()
        |> Map.fetch!("results")

      start_line =
        get_in(result, ["locations", Access.at(0), "physicalLocation", "region", "startLine"])

      assert start_line == f.line

      refute start_line == 1,
             "startLine 1 is the fallback SARIF uses when :line is absent - " <>
               "the line was lost at the normalization seam"
    end
  end

  describe "the :warn tier is not discarded" do
    # Six of the ten RE rules emit `severity: :warn`. "warn" was absent from
    # CLI's @severity_order, so `Map.get(@severity_order, "warn", 5)` gave it
    # rank 5; the filter `rank <= threshold` at the default threshold of
    # "medium" (3) then dropped every one of them *after* the rules had run.
    # SARIF's level mapping mirrors that ranking, so it guards the same fix.
    test "warn maps to the SARIF warning level, not the note catch-all" do
      [result] =
        [
          %{
            severity: "warn",
            rule_module: "research_extensions",
            type: "RE001",
            file: ".github/workflows/deploy.yml",
            reason: "no harden-runner"
          }
        ]
        |> SARIF.from_findings("/tmp")
        |> Map.fetch!("runs")
        |> hd()
        |> Map.fetch!("results")

      assert result["level"] == "warning",
             "warn fell through to the catch-all - it is being treated as " <>
               "lower than info by every consumer that ranks severities"
    end
  end

  describe "Rules facade" do
    test "scan_research_extensions/2 delegates to the module" do
      result = Rules.scan_research_extensions(tripwire_repo())

      assert %{findings: _, total: total, by_severity: _, dispatch: _} = result
      assert total > 0
    end

    test "scan_all_estate_policies/2 includes the research-extension family" do
      %{findings: findings} = Rules.scan_all_estate_policies(tripwire_repo())

      assert Enum.any?(findings, &(Map.get(&1, :rule) in ["RE001", "RE004"])),
             "the estate-policy facade documents 'every estate-policy rule' " <>
               "but omitted RE001-RE010"
    end
  end
end

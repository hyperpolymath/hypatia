# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.RuleLoaderTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.RuleLoader
  alias Hypatia.Rules.RuleLoader.RuleDef

  @fixtures Path.expand("../fixtures/hypatia_rules", __DIR__)

  describe "load_dir/1" do
    test "loads every .a2ml rule definition, sorted by id" do
      assert {:ok, rules} = RuleLoader.load_dir(@fixtures)
      assert Enum.map(rules, & &1.id) == ["HYP-S004", "HYP-S006", "HYP-S007"]
      assert Enum.all?(rules, &match?(%RuleDef{}, &1))
    end

    test "returns an error (not a crash) for an unreadable directory" do
      assert {:error, {:cannot_read_dir, _, _}} =
               RuleLoader.load_dir("/no/such/rules/dir")
    end
  end

  describe "parse/1 — metadata extraction" do
    setup do
      {:ok, rules} = RuleLoader.load_dir(@fixtures)
      %{by_id: Map.new(rules, &{&1.id, &1})}
    end

    test "extracts identity/severity/category from the @rule block", %{by_id: by_id} do
      s006 = by_id["HYP-S006"]
      assert s006.name =~ "Registry staleness"
      assert s006.severity == :medium
      assert s006.category == "StandardsCompliance"
      assert s006.auto_fixable == true

      s004 = by_id["HYP-S004"]
      assert s004.severity == :critical
      assert s004.category == "Dogfooding"
      assert s004.auto_fixable == false
    end

    test "extracts scanner globs", %{by_id: by_id} do
      globs = by_id["HYP-S006"].scanner_globs
      assert ".machine_readable/REGISTRY.a2ml" in globs
      assert "TOPOLOGY.md" in globs
    end

    test "extracts router default strategy, recipe, and action signal", %{by_id: by_id} do
      s006 = by_id["HYP-S006"]
      assert s006.router_default_strategy == :auto_execute
      assert s006.router_recipe == "rebuild-registry"
      assert s006.action_signal == "doc.drift"

      assert by_id["HYP-S004"].action_signal == "compliance.finding.new"
    end

    test "preserves the @logic block verbatim without interpreting it", %{by_id: by_id} do
      # This increment does NOT execute @logic — it keeps it for a later pass.
      assert is_binary(by_id["HYP-S006"].logic_raw)
      assert by_id["HYP-S006"].logic_raw =~ "for_each_spec"
    end
  end

  describe "strategy caps — the Manual-Only licence guardrail" do
    setup do
      {:ok, rules} = RuleLoader.load_dir(@fixtures)
      %{rules: rules, by_id: Map.new(rules, &{&1.id, &1})}
    end

    test "parses all strategy_caps entries with when/cap/reason", %{by_id: by_id} do
      caps = by_id["HYP-S006"].strategy_caps
      # registry-staleness declares three caps: licence, dead-home, external-pointer.
      assert length(caps) == 3
      assert Enum.all?(caps, &(&1[:cap] in [:review, :auto_execute, :report_only]))
      assert Enum.all?(caps, &is_binary(&1[:reason]))
    end

    test "the licence/SPDX cap is present and demotes to :review", %{by_id: by_id} do
      caps = by_id["HYP-S006"].strategy_caps
      licence = Enum.find(caps, &String.match?(&1[:when], ~r/SPDX|PMPL|licen[cs]e/i))
      assert licence, "registry-staleness must carry a licence strategy cap"
      assert licence[:cap] == :review
    end

    test "licence_caps/1 surfaces exactly the licence-overlapping :review caps", %{rules: rules} do
      lic = RuleLoader.licence_caps(rules)
      assert length(lic) == 1
      assert hd(lic)[:cap] == :review
      assert String.match?(hd(lic)[:when], ~r/SPDX|licen[cs]e/i)
    end
  end

  describe "parse/1 — failure modes (fail loudly)" do
    test "text with no @rule block is a clear error, not a fabricated rule" do
      assert {:error, _} = RuleLoader.parse("# just a comment\n")
    end
  end
end

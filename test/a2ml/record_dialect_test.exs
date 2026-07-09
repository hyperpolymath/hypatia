# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.A2ml.RecordDialectTest do
  use ExUnit.Case, async: true

  alias Hypatia.A2ml.RecordDialect
  alias Hypatia.Rules.RsrCriteria

  @ssot Path.expand("../fixtures/a2ml/rsr-criteria-v2.a2ml", __DIR__)

  describe "RecordDialect.parse/1 — scalars and structure" do
    test "parses sections, kebab keys, and the scalar types" do
      {:ok, t} =
        RecordDialect.parse("""
        # a comment
        [meta]
        name = "demo"
        count = 42          # trailing comment
        ratio = 1.5
        enabled = true
        disabled = false
        """)

      assert t["meta"]["name"] == "demo"
      assert t["meta"]["count"] == 42
      assert t["meta"]["ratio"] == 1.5
      assert t["meta"]["enabled"] == true
      assert t["meta"]["disabled"] == false
    end

    test "parses multi-line arrays with trailing commas and embedded comments" do
      {:ok, t} =
        RecordDialect.parse("""
        [x]
        items = [
          "a",
          "b",   # keep b
          "c",
        ]
        """)

      assert t["x"]["items"] == ["a", "b", "c"]
    end

    test "parses inline tables and arrays of inline tables" do
      {:ok, t} =
        RecordDialect.parse("""
        [x]
        rows = [
          { id = "1", ok = true },
          { id = "2", ok = false },
        ]
        """)

      assert t["x"]["rows"] == [%{"id" => "1", "ok" => true}, %{"id" => "2", "ok" => false}]
    end

    test "dotted section headers nest" do
      {:ok, t} = RecordDialect.parse("[a.b]\nk = \"v\"\n")
      assert t["a"]["b"]["k"] == "v"
    end

    test "[[array-of-tables]] appends" do
      {:ok, t} =
        RecordDialect.parse("""
        [[item]]
        id = 1
        [[item]]
        id = 2
        """)

      assert Enum.map(t["item"], & &1["id"]) == [1, 2]
    end

    test "braces inside a quoted string are literal, not structure" do
      {:ok, t} = RecordDialect.parse(~s([x]\ndesc = "a {b,c} d"\n))
      assert t["x"]["desc"] == "a {b,c} d"
    end

    test "reports (does not raise) on an unquoted scalar" do
      assert {:error, {:bad_value, "k", _}} = RecordDialect.parse("[x]\nk = bareword\n")
    end
  end

  describe "consuming the real RSR v2.0 criteria SSOT" do
    test "loads the shipped SSOT into 11 categories and 74 criteria" do
      {:ok, cat} = RsrCriteria.load(@ssot)

      assert cat.version == "2.0.0-draft"
      assert cat.status == "draft"
      assert length(cat.categories) == 11
      assert length(cat.criteria) == 74
    end

    test "tier thresholds parse as integers" do
      {:ok, cat} = RsrCriteria.load(@ssot)
      assert cat.tiers["bronze"] == 75
      assert cat.tiers["silver"] == 90
      assert cat.tiers["gold"] == 100
    end

    test "every criterion carries the seven fields the spec mandates" do
      {:ok, cat} = RsrCriteria.load(@ssot)

      for c <- cat.criteria do
        assert is_binary(c.id)
        assert is_binary(c.name)
        assert is_binary(c.desc)
        assert c.tier in ~w(bronze silver gold rhodium)
        assert is_binary(c.gate)
        assert is_binary(c.detect)
        assert is_binary(c.template_ref)
      end
    end

    test "gates are drawn from the capability taxonomy" do
      {:ok, cat} = RsrCriteria.load(@ssot)
      gates = cat.criteria |> Enum.map(& &1.gate) |> Enum.uniq()
      # universal + real capabilities; no stray tier names leaking in as gates.
      assert "universal" in gates
      refute Enum.any?(gates, &(&1 in ~w(bronze silver gold rhodium)))
    end

    test "helpers slice the catalogue by gate, tier, and automatability" do
      {:ok, cat} = RsrCriteria.load(@ssot)

      assert Enum.all?(RsrCriteria.universal(cat), &(&1.gate == "universal"))
      # bronze-required ⊆ gold-required
      assert length(RsrCriteria.required_for_tier(cat, "bronze")) <=
               length(RsrCriteria.required_for_tier(cat, "gold"))

      # A meaningful fraction is already automatable (a real detect rule id).
      auto = RsrCriteria.automatable(cat)
      assert length(auto) > 0
      refute Enum.any?(auto, &(&1.detect == "manual"))
    end
  end
end

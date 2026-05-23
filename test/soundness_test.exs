# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.SoundnessTest do
  @moduledoc """
  Soundness gate: every rule listed in `test/soundness/manifest.json`
  MUST fire on its declared fixture.

  This catches the class of regression PR #278 documented (the deployed
  escript was silently dropping the entire Elixir/Erlang/Coq/Lean/Agda/
  Zig/F\*/Ada pattern families because the binary was stale). Without a
  named fixture per rule, we can rebuild every binary in the world and
  still not know whether a given rule is firing — because "no findings"
  means both "code is clean" and "rule is broken."

  Adding a rule: drop a known-bad sample in `test/soundness/fixtures/`
  and add a manifest entry. The test will pick it up automatically.

  Removing a manifest entry: must be justified in the commit message
  (rule deprecated / merged / superseded). The default assumption is
  the entry stays.

  Tagged `:soundness` so CI can call this suite out separately in
  reports — a soundness failure is qualitatively different from a
  product test failure.
  """

  use ExUnit.Case, async: true

  @moduletag :soundness

  alias Hypatia.Rules.CodeSafety

  @manifest_path Path.expand("soundness/manifest.json", __DIR__)

  setup_all do
    manifest =
      @manifest_path
      |> File.read!()
      |> Jason.decode!()
      |> Map.fetch!("entries")

    {:ok, manifest: manifest}
  end

  describe "manifest" do
    test "is non-empty", %{manifest: manifest} do
      assert length(manifest) > 0,
             "Soundness manifest must list at least one rule. " <>
               "An empty manifest defeats the entire purpose of this test."
    end

    test "every fixture file exists on disk", %{manifest: manifest} do
      missing =
        Enum.filter(manifest, fn entry ->
          not File.exists?(Map.fetch!(entry, "fixture"))
        end)

      assert missing == [],
             "Soundness manifest references fixtures that don't exist: " <>
               inspect(Enum.map(missing, &Map.fetch!(&1, "fixture")))
    end

    test "every entry has the required fields", %{manifest: manifest} do
      required = ~w(rule_module rule_id language fixture expected_severity)

      bad =
        Enum.filter(manifest, fn entry ->
          Enum.any?(required, fn key -> not Map.has_key?(entry, key) end)
        end)

      assert bad == [],
             "Soundness manifest entries missing required fields: " <> inspect(bad)
    end
  end

  describe "code_safety rules fire on their fixtures" do
    @manifest_path
    |> File.read!()
    |> Jason.decode!()
    |> Map.fetch!("entries")
    |> Enum.filter(fn entry -> Map.fetch!(entry, "rule_module") == "code_safety" end)
    |> Enum.each(fn entry ->
      rule_id = Map.fetch!(entry, "rule_id")
      language = Map.fetch!(entry, "language")
      fixture = Map.fetch!(entry, "fixture")
      expected_severity = Map.fetch!(entry, "expected_severity")

      test "code_safety/#{rule_id} fires on #{fixture}" do
        content = File.read!(unquote(fixture))
        findings = CodeSafety.scan_content(content, unquote(language))

        finding = Enum.find(findings, &(&1.rule == unquote(String.to_atom(rule_id))))

        assert finding != nil,
               "Soundness gate FAILED: rule code_safety/#{unquote(rule_id)} " <>
                 "did NOT fire on its fixture #{unquote(fixture)}. " <>
                 "Either the rule was removed / weakened / the regex broke, " <>
                 "or the fixture was sanitised. See PR #278 for context."

        actual_severity = to_string(finding.severity)

        assert actual_severity == unquote(expected_severity),
               "Soundness gate: rule code_safety/#{unquote(rule_id)} fired but at " <>
                 "severity '#{actual_severity}', expected '#{unquote(expected_severity)}'. " <>
                 "If this is intentional, update the manifest in the same commit."
      end
    end)
  end
end

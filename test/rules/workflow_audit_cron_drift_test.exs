# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.WorkflowAudit.CronDriftTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowAudit

  # WF025 — codeql.yml cron drift against canonical `0 6 1 * *`
  # (standards#286 / standards#288 / standards#323 / standards#324).

  describe "cron_drift_subcategory/1 — classifier" do
    test "canonical `0 6 1 * *` is :canonical (no drift)" do
      assert WorkflowAudit.cron_drift_subcategory("0 6 1 * *") == :canonical
    end

    test "weekly-style (DOW != *) → :weekly_to_monthly" do
      assert WorkflowAudit.cron_drift_subcategory("0 6 * * 1") == :weekly_to_monthly
    end

    test "monthly but non-canonical HH/MM → :non_canonical_monthly" do
      assert WorkflowAudit.cron_drift_subcategory("30 4 15 * *") == :non_canonical_monthly
    end

    test "malformed (3 fields) → :malformed" do
      assert WorkflowAudit.cron_drift_subcategory("0 6 *") == :malformed
    end

    test "monthly but different day-of-month → :non_canonical_monthly" do
      assert WorkflowAudit.cron_drift_subcategory("0 6 15 * *") == :non_canonical_monthly
    end
  end

  describe "check_codeql_cron_drift/1 — file-level routing" do
    test "fires :weekly_to_monthly with recipe_id codeql-cron-monthly" do
      content = """
      name: CodeQL
      on:
        schedule:
          - cron: '0 6 * * 1'
      """

      assert [finding] =
               WorkflowAudit.check_codeql_cron_drift([
                 {".github/workflows/codeql.yml", content}
               ])

      assert finding.rule == "WF025"
      assert finding.severity == :warn
      assert finding.auto_fixable == true
      assert finding.recipe_id == "codeql-cron-monthly"
      assert finding.sub_category == :weekly_to_monthly
      assert finding.canonical == "0 6 1 * *"
    end

    test "fires :non_canonical_monthly for differing HH/DOM" do
      content = """
      on:
        schedule:
          - cron: '0 12 15 * *'
      """

      assert [finding] =
               WorkflowAudit.check_codeql_cron_drift([
                 {".github/workflows/codeql.yml", content}
               ])

      assert finding.sub_category == :non_canonical_monthly
      assert finding.recipe_id == "codeql-cron-monthly"
    end

    test "ignores non-codeql workflows" do
      content = """
      on:
        schedule:
          - cron: '0 6 * * 1'
      """

      assert [] =
               WorkflowAudit.check_codeql_cron_drift([
                 {".github/workflows/scorecard.yml", content}
               ])
    end

    test "canonical cron emits NO finding" do
      content = """
      name: CodeQL
      on:
        schedule:
          - cron: '0 6 1 * *'
      """

      assert [] =
               WorkflowAudit.check_codeql_cron_drift([
                 {".github/workflows/codeql.yml", content}
               ])
    end

    test "multiple cron entries → one finding per non-canonical entry" do
      content = """
      on:
        schedule:
          - cron: '0 6 1 * *'
          - cron: '0 6 * * 1'
          - cron: '30 4 15 * *'
      """

      findings =
        WorkflowAudit.check_codeql_cron_drift([
          {".github/workflows/codeql.yml", content}
        ])

      assert length(findings) == 2

      subcats = findings |> Enum.map(& &1.sub_category) |> Enum.sort()
      assert subcats == [:non_canonical_monthly, :weekly_to_monthly]
    end
  end
end

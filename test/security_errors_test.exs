# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.SecurityErrorsTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.SecurityErrors

  describe "error_categories/0" do
    test "returns a map of error categories" do
      cats = SecurityErrors.error_categories()
      assert is_map(cats)
      assert map_size(cats) > 10
    end

    test "includes key security categories" do
      cats = SecurityErrors.error_categories()
      assert Map.has_key?(cats, :workflow_permission)
      assert Map.has_key?(cats, :unpinned_action)
      assert Map.has_key?(cats, :missing_spdx)
      assert Map.has_key?(cats, :branch_protection)
    end
  end

  describe "error_category/1" do
    test "returns description for known category" do
      assert "Missing or incorrect workflow permissions" =
               SecurityErrors.error_category(:workflow_permission)
    end

    test "returns nil for unknown category" do
      assert nil == SecurityErrors.error_category(:nonexistent)
    end
  end

  describe "severity_levels/0" do
    test "returns all 5 severity levels" do
      levels = SecurityErrors.severity_levels()
      assert Map.has_key?(levels, :critical)
      assert Map.has_key?(levels, :high)
      assert Map.has_key?(levels, :medium)
      assert Map.has_key?(levels, :low)
      assert Map.has_key?(levels, :info)
    end

    test "critical has lowest numeric value" do
      levels = SecurityErrors.severity_levels()
      assert levels[:critical] < levels[:high]
      assert levels[:high] < levels[:medium]
      assert levels[:medium] < levels[:low]
      assert levels[:low] < levels[:info]
    end
  end

  describe "severity_value/1" do
    test "returns numeric value for known severity" do
      assert 1 = SecurityErrors.severity_value(:critical)
      assert 2 = SecurityErrors.severity_value(:high)
      assert 5 = SecurityErrors.severity_value(:info)
    end

    test "returns 5 for unknown severity" do
      assert 5 = SecurityErrors.severity_value(:unknown)
    end
  end

  describe "SHA-pin exemptions (hypatia#262)" do
    test "slsa-github-generator is pin-exempt in tag form" do
      assert SecurityErrors.pin_exempt?("slsa-framework/slsa-github-generator@v2.1.0")
    end

    test "slsa generator reusable-workflow path form is also exempt" do
      ref =
        "slsa-framework/slsa-github-generator/.github/workflows/" <>
          "generator_generic_slsa3.yml@v2.1.0"

      assert SecurityErrors.pin_exempt?(ref)
      assert SecurityErrors.pin_exemption_reason(ref) =~ "provenance"
    end

    test "own-org standards reusables are exempt under either name of the repository" do
      for owner <- ["hyperpolymath", "metadatastician"] do
        ref = "#{owner}/standards/.github/workflows/governance-reusable.yml@main"
        assert SecurityErrors.pin_exempt?(ref)
        assert SecurityErrors.pin_exemption_reason(ref) =~ "wrapper pattern"
      end
    end

    test "pin_action returns :exempt (never a SHA fix) for exempt actions" do
      assert {:exempt, reason} =
               SecurityErrors.pin_action("slsa-framework/slsa-github-generator@v2.1.0")

      assert reason =~ "semver tag"
    end

    test "non-exempt actions still pin normally" do
      assert {:ok, pinned} = SecurityErrors.pin_action("actions/checkout@v4")
      assert pinned =~ "actions/checkout@"
    end

    test "the harmful slsa SHA mapping was removed from @sha_pins" do
      refute Map.has_key?(
               SecurityErrors.sha_pins(),
               "slsa-framework/slsa-github-generator@v2.1.0"
             )
    end
  end

  describe "ECHIDNA security_learning transcription" do
    test "alert_categories/0 carries all 18 alert_category/2 facts" do
      expected = %{
        "TokenPermissionsID" => :workflow_security,
        "PinnedDependenciesID" => :workflow_security,
        "missing-workflow-permissions" => :workflow_security,
        "hard-coded-cryptographic-value" => :code_security,
        "remote-property-injection" => :code_security,
        "sql-injection" => :code_security,
        "unused-local-variable" => :code_quality,
        "syntax-error" => :code_quality,
        "VulnerabilitiesID" => :dependency_vuln,
        "unmaintained" => :dependency_vuln,
        "SecurityPolicyID" => :process_hygiene,
        "MaintainedID" => :process_hygiene,
        "CodeReviewID" => :process_hygiene,
        "BranchProtectionID" => :process_hygiene,
        "CIIBestPracticesID" => :process_hygiene,
        "FuzzingID" => :missing_tests,
        "SASTID" => :missing_tests,
        "CITestsID" => :missing_tests
      }

      assert SecurityErrors.alert_categories() == expected
    end

    test "alert_category/1 returns nil outside the transcribed facts" do
      assert SecurityErrors.alert_category("TokenPermissionsID") == :workflow_security
      assert SecurityErrors.alert_category("unused-import") == nil
    end

    test "Logtalk severity catch-all members are explicit :low, not :info" do
      for alert_type <- ["SASTID", "CITestsID", "unmaintained", "FuzzingID"] do
        assert SecurityErrors.issue_severity(alert_type) == :low
      end
    end

    test "validate_alert/1 composes the transcribed predicates" do
      validation = SecurityErrors.validate_alert("TokenPermissionsID")

      assert validation.alert_type == "TokenPermissionsID"
      assert validation.category == :workflow_security
      assert validation.severity == :high
      assert validation.auto_fixable == true
      assert validation.fix =~ "granular workflow-level permissions"
      assert validation.prevention_workflow == "workflow-linter.yml"
    end

    test "validate_alert/1 keeps the catch-all members complete" do
      validation = SecurityErrors.validate_alert("SASTID")

      assert validation.category == :missing_tests
      assert validation.severity == :low
      assert validation.auto_fixable == false
      assert validation.fix == nil
      assert validation.prevention_workflow == nil
    end

    test "validate_alert/1 on an unknown type mirrors the failure fields" do
      validation = SecurityErrors.validate_alert("never-seen-alert")

      assert validation.category == nil
      assert validation.severity == :info
      assert validation.auto_fixable == false
      assert validation.fix == nil
      assert validation.prevention_workflow == nil
    end

    test "remote property injection carries its exact CWE" do
      assert {cwe, _title} = SecurityErrors.cwe_for("remote-property-injection")
      assert cwe == "CWE-915"
    end
  end

  describe "language policy (2026-11-01 refresh)" do
    test "logtalk is banned with Elixir as the replacement" do
      refute SecurityErrors.language_allowed?("logtalk")
      assert SecurityErrors.language_banned?("logtalk")
      assert SecurityErrors.language_replacement("logtalk") == "elixir"
      assert SecurityErrors.language_allowed?("elixir")
    end
  end
end

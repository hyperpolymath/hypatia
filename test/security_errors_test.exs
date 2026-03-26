# SPDX-License-Identifier: PMPL-1.0-or-later
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
      assert "Missing or incorrect workflow permissions" = SecurityErrors.error_category(:workflow_permission)
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
end

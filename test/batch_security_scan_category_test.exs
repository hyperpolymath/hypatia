# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.BatchSecurityScanCategoryTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.SecurityErrors
  alias Mix.Tasks.Hypatia.BatchSecurityScan

  defp exported(rule_id) do
    BatchSecurityScan.category_string(SecurityErrors.validate_alert(rule_id).category, rule_id)
  end

  test "a canonical alert type exports its SecurityErrors category" do
    assert exported("TokenPermissionsID") == "workflow_security"
  end

  test "an alert type outside the canonical table falls back to the task grouping, never \"nil\"" do
    assert SecurityErrors.validate_alert("unused-import").category == nil
    assert exported("unused-import") == "code_quality"
  end

  test "an alert type no grouping knows exports \"unknown\"" do
    assert exported("no-such-alert") == "unknown"
  end
end

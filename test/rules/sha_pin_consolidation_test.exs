# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.ShaPinConsolidationTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.SecurityErrors
  alias Hypatia.Rules.WorkflowAudit

  # Audit 2026-05-28 Part 3.7: consolidate the two SHA-pin databases into
  # one canonical source. SecurityErrors.@sha_pins is the source of truth;
  # WorkflowAudit.known_good_shas/0 delegates to it.

  describe "SHA-pin database consolidation" do
    test "WorkflowAudit.known_good_shas/0 delegates to SecurityErrors.sha_pins/0" do
      assert WorkflowAudit.known_good_shas() == SecurityErrors.sha_pins()
    end

    test "canonical set contains entries previously unique to security_errors" do
      pins = SecurityErrors.sha_pins()
      assert Map.has_key?(pins, "ocaml/setup-ocaml@v3")
      assert Map.has_key?(pins, "softprops/action-gh-release@v2")
    end

    test "canonical set contains entries previously unique to workflow_audit" do
      pins = SecurityErrors.sha_pins()
      assert Map.has_key?(pins, "github/codeql-action@v4")
      assert Map.has_key?(pins, "denoland/setup-deno@v2")
      assert Map.has_key?(pins, "hyperpolymath/a2ml-validate-action@main")
      assert Map.has_key?(pins, "hyperpolymath/k9-validate-action@main")
      assert Map.has_key?(pins, "hyperpolymath/panic-attacker/.github/workflows/scan-and-report.yml@main")
    end

    test "shared entries (in both old maps) match the same SHA" do
      pins = SecurityErrors.sha_pins()
      # actions/checkout@v4 was in both — make sure it survived consolidation
      assert pins["actions/checkout@v4"] == "34e114876b0b11c390a56381ad16ebd13914f8d5"
      # github/codeql-action@v3 was in both
      assert pins["github/codeql-action@v3"] == "6624720a57d4c312633c7b953db2f2da5bcb4c3a"
    end

    test "no entries lost in consolidation — at least 22 pins total" do
      # security_errors had 17 entries before; workflow_audit had 19.
      # Union = 17 + 19 - intersection (15 shared) = 21.
      # Adding the WH004-only github/codeql-action@v4 etc, expecting >= 22.
      pins = SecurityErrors.sha_pins()
      assert map_size(pins) >= 22, "Expected union to be at least 22 pins; got #{map_size(pins)}"
    end
  end
end

# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.CiHealthAllowlistCorpusTest do
  @moduledoc """
  Runtime check that `scripts/ci-health/remediate.sh` B-ALLOWLIST is additive.

  `test/ci-health-test.sh` runs the real `remediate.sh` and `detect.sh` against
  a mock `gh` over the corpus in `test/fixtures/ci-health/allowlist/`. For each
  row it checks that no current pattern is dropped, every required pattern is
  present, the trust flags are written back as read, a second run writes
  nothing, and `detect.sh` stops reporting B-ALLOWLIST. It also checks that
  `verification/proofs/idris2/AllowlistRemediation.idr` states the same counts
  for every row, so the model and the script cannot drift apart.

  This wrapper puts that shell suite under ExUnit so the proof ledger's exunit
  lane runs it and derives the `allowlist-remediation-corpus` row from the run.
  """
  use ExUnit.Case, async: true

  @script "test/ci-health-test.sh"

  test "the CI-health corpus passes against the real scripts" do
    {out, status} = System.cmd("bash", [@script], stderr_to_stdout: true)

    assert status == 0, out
    assert out =~ "PASS: CI-health classification and remediation fixtures"
  end
end

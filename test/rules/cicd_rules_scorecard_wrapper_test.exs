# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.CicdRules.ScorecardWrapperTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CicdRules

  # #390 — a `.github/workflows/scorecard.yml` that CALLS the standards
  # `scorecard-reusable.yml` must grant `security-events: write` on the
  # calling job, or the scheduled Scorecard run fails with `startup_failure`
  # (no logs). Detection: uses "scorecard-reusable.yml" AND NOT
  # "security-events: write". Sensitivity + specificity both covered.

  @wf_path ".github/workflows/scorecard.yml"

  @reusable_no_perm """
  # SPDX-License-Identifier: MPL-2.0
  name: Scorecard
  on:
    schedule:
      - cron: "0 2 * * 1"
  permissions: read-all
  jobs:
    analysis:
      uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@abc1234
  """

  @reusable_with_perm """
  # SPDX-License-Identifier: MPL-2.0
  name: Scorecard
  on:
    schedule:
      - cron: "0 2 * * 1"
  permissions: read-all
  jobs:
    analysis:
      permissions:
        security-events: write
        id-token: write
      uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@abc1234
  """

  @inline_no_reusable """
  # SPDX-License-Identifier: MPL-2.0
  name: Scorecard
  on:
    schedule:
      - cron: "0 2 * * 1"
  permissions: read-all
  jobs:
    analysis:
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v4
        - uses: ossf/scorecard-action@v2
  """

  setup do
    dir = Path.join(System.tmp_dir!(), "hyp-scw-#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  defp write_scorecard(dir, body, sub \\ "") do
    rel = if sub == "", do: @wf_path, else: Path.join(sub, @wf_path)
    path = Path.join(dir, rel)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, body)
    path
  end

  describe "scan_scorecard_wrapper_permissions/2 — sensitivity" do
    test "fires when wrapper uses reusable but lacks the perm", %{dir: dir} do
      write_scorecard(dir, @reusable_no_perm)
      assert [finding] = CicdRules.scan_scorecard_wrapper_permissions(dir)
      assert finding.rule == :scorecard_wrapper_missing_job_permissions
      assert finding.severity == :high
      assert finding.file == @wf_path
      assert finding.fix =~ "security-events: write"
    end

    test "fires on a nested monorepo copy", %{dir: dir} do
      write_scorecard(dir, @reusable_no_perm, "packages/api")
      assert [finding] = CicdRules.scan_scorecard_wrapper_permissions(dir)
      assert finding.file == "packages/api/" <> @wf_path
    end
  end

  describe "scan_scorecard_wrapper_permissions/2 — specificity" do
    test "silent when wrapper grants the perm", %{dir: dir} do
      write_scorecard(dir, @reusable_with_perm)
      assert CicdRules.scan_scorecard_wrapper_permissions(dir) == []
    end

    test "silent for inline scorecard not using the reusable", %{dir: dir} do
      write_scorecard(dir, @inline_no_reusable)
      assert CicdRules.scan_scorecard_wrapper_permissions(dir) == []
    end

    test "path_allow_prefixes carve-out skips the wrapper", %{dir: dir} do
      write_scorecard(dir, @reusable_no_perm, "vendor/upstream")

      findings =
        CicdRules.scan_scorecard_wrapper_permissions(dir, path_allow_prefixes: ["vendor/"])

      assert findings == []
    end
  end

  describe "check_scorecard_wrapper_permissions/2 — pure predicate" do
    test "fail when reusable present and perm absent" do
      result = CicdRules.check_scorecard_wrapper_permissions(@wf_path, @reusable_no_perm)
      assert {:fail, finding} = result
      assert finding.rule == :scorecard_wrapper_missing_job_permissions
      assert finding.reason =~ "startup_failure"
    end

    test "ok when perm present with irregular spacing" do
      body = String.replace(@reusable_no_perm, "uses:", "security-events:   write\n      uses:")
      assert :ok = CicdRules.check_scorecard_wrapper_permissions(@wf_path, body)
    end

    test "ok when reusable not called" do
      assert :ok = CicdRules.check_scorecard_wrapper_permissions(@wf_path, @inline_no_reusable)
    end
  end
end

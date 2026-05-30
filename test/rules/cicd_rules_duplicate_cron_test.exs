# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.CicdRules.DuplicateCronTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CicdRules

  # #362 — multiple `cron:` entries firing on the same day-of-week are a
  # consolidation-artefact waste pattern. Two shapes: same exact day-of-week
  # field (>= 2 entries), and a daily `* * *` entry strictly covering a
  # day-specific entry at the same HH:MM. Oracles from hypatia#331.

  @three_monday "on:\n  schedule:\n    - cron: 0 5 * * 1\n    - cron: 30 4 * * 1\n    - cron: 0 4 * * 1\n"
  @daily_plus_monday "on:\n  schedule:\n    - cron: \"0 3 * * *\"\n    - cron: \"0 3 * * 1\"\n"
  @daily_plus_other_time "on:\n  schedule:\n  - cron: 0 2 * * *\n  - cron: 0 3 * * 1\n"
  @single "on:\n  schedule:\n    - cron: '0 3 * * *'\n"
  @commented "on:\n  schedule:\n    - cron: 0 4 * * 1\n    # - cron: 0 5 * * 1\n"

  defp wf(dir, name, body) do
    path = Path.join([dir, ".github/workflows", name])
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, body)
  end

  setup do
    dir = Path.join(System.tmp_dir!(), "hyp-cron-#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  describe "check_duplicate_cron_schedules/2 — sensitivity" do
    test "three crons on the same day-of-week (verify-proofs.yml oracle)" do
      assert [finding] =
               CicdRules.check_duplicate_cron_schedules("verify-proofs.yml", @three_monday)

      assert finding.rule == :duplicate_cron_schedule
      assert finding.severity == :medium
      assert length(finding.crons) == 3
    end

    test "daily strictly covers a same-time day-specific entry (tests.yml oracle)" do
      assert [finding] = CicdRules.check_duplicate_cron_schedules("tests.yml", @daily_plus_monday)
      assert finding.rule == :duplicate_cron_schedule
      assert finding.reason =~ "subset"
    end
  end

  describe "check_duplicate_cron_schedules/2 — specificity" do
    test "daily + different-time day-specific is NOT a subset (security-policy.yml)" do
      assert CicdRules.check_duplicate_cron_schedules(
               "security-policy.yml",
               @daily_plus_other_time
             ) ==
               []
    end

    test "a single cron is never flagged" do
      assert CicdRules.check_duplicate_cron_schedules("x.yml", @single) == []
    end

    test "commented-out cron lines are ignored" do
      assert CicdRules.check_duplicate_cron_schedules("x.yml", @commented) == []
    end
  end

  describe "scan_duplicate_cron_schedules/1" do
    test "walks workflow files and reports only the offending one", %{dir: dir} do
      wf(dir, "clean.yml", @single)
      wf(dir, "dirty.yml", @three_monday)
      assert [finding] = CicdRules.scan_duplicate_cron_schedules(dir)
      assert finding.file == ".github/workflows/dirty.yml"
    end
  end
end

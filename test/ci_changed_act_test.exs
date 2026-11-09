# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.CiChangedActTest do
  use ExUnit.Case, async: false
  alias Hypatia.CiChanged

  @moduletag :act_integration
  @moduletag timeout: 180_000

  test "real noninteractive act runs changed source and refuses a failing execution" do
    assert System.find_executable("act"),
           "provision the pinned act executable and container daemon"

    repo =
      Path.join(
        System.tmp_dir!(),
        "hypatia-real-act-#{System.pid()}-#{System.os_time(:nanosecond)}"
      )

    File.mkdir_p!(Path.join(repo, ".github/workflows"))
    File.mkdir_p!(Path.join(repo, "scripts"))
    on_exit(fn -> File.rm_rf!(repo) end)

    File.write!(Path.join(repo, ".github/workflows/ci.yml"), """
    on: push
    permissions: read-all
    jobs:
      build:
        runs-on: ubuntu-24.04
        steps:
          - uses: actions/checkout@v7
          - run: bash scripts/input.sh
    """)

    script = Path.join(repo, "scripts/input.sh")
    File.write!(script, "exit 0\n")

    for args <- [
          ["init", "-q"],
          ["add", "."],
          [
            "-c",
            "commit.gpgsign=false",
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.org",
            "commit",
            "-qm",
            "baseline"
          ]
        ] do
      assert {_, 0} = System.cmd("git", args, cd: repo, stderr_to_stdout: true)
    end

    File.write!(script, """
    set -eu
    test ! -t 0
    test -z "${GITHUB_TOKEN:-}"
    test -f scripts/input.sh
    printf 'HYPATIA_REAL_ACT_CHANGED_INPUT\n'
    """)

    assert [%{local_blocker: nil}] = jobs = CiChanged.plan(repo, "HEAD").jobs
    assert [%{outcome: :completed, output: output}] = CiChanged.execute_jobs(repo, jobs)
    assert output =~ "HYPATIA_REAL_ACT_CHANGED_INPUT"

    File.write!(script, "exit 23\n")
    assert [%{outcome: :failed}] = CiChanged.execute_jobs(repo, CiChanged.plan(repo, "HEAD").jobs)
  end
end

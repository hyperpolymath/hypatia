# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.CiChangedTest do
  use ExUnit.Case, async: true
  alias Hypatia.CiChanged

  test "git plan includes untracked workflows and the scan catches a changed atom conversion" do
    repo =
      Path.join(System.tmp_dir!(), "hypatia-ci-changed-#{System.unique_integer([:positive])}")

    File.mkdir_p!(repo)
    on_exit(fn -> File.rm_rf!(repo) end)
    git = fn args -> assert {_, 0} = System.cmd("git", args, cd: repo, stderr_to_stdout: true) end
    git.(["init", "-q"])

    git.([
      "-c",
      "user.name=Test",
      "-c",
      "user.email=test@example.org",
      "-c",
      "commit.gpgsign=false",
      "commit",
      "--allow-empty",
      "-qm",
      "baseline"
    ])

    File.mkdir_p!(Path.join(repo, ".github/workflows"))

    File.write!(
      Path.join(repo, ".github/workflows/ci.yml"),
      "on: push\njobs: {a: {steps: [{run: echo a}]}, b: {steps: [{run: echo b}]}}\n"
    )

    plan = CiChanged.plan(repo, "HEAD")
    assert Enum.map(plan.jobs, & &1.job) == ["a", "b"]
    File.mkdir_p!(Path.join(repo, "lib"))

    File.write!(
      Path.join(repo, "lib/changed.ex"),
      "defmodule Changed do\n def convert(input), do: String.to_atom(input)\nend\n"
    )

    assert Enum.any?(
             CiChanged.scan(repo, ["lib/changed.ex"]),
             &(&1.type == "elixir_atom_from_user")
           )

    assert CiChanged.scan(repo, ["unrelated.ex"]) == []
    assert_raise ArgumentError, fn -> CiChanged.plan(repo, "missing-base") end
  end

  @old "on: pull_request\njobs:\n  a:\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo a\n  b:\n    runs-on: ubuntu-latest\n    steps:\n      - run: echo b\n"

  test "semantic changes select the job, not moved source lines" do
    assert CiChanged.changed_jobs(@old, "# comment\n" <> @old) == []
    assert CiChanged.changed_jobs(@old, String.replace(@old, "echo a", "echo changed")) == ["a"]
    assert Enum.sort(CiChanged.changed_jobs(nil, @old)) == ["a", "b"]

    assert Enum.sort(CiChanged.changed_jobs(@old, String.replace(@old, "pull_request", "push"))) ==
             ["a", "b"]
  end

  test "aliases and block scalars are read by the parser" do
    before =
      "on: push\njobs:\n  a: &job {runs-on: ubuntu-latest, steps: [{run: echo a}]}\n  b: *job\n"

    after_text = String.replace(before, "echo a", "echo b")
    assert Enum.sort(CiChanged.changed_jobs(before, after_text)) == ["a", "b"]
    block = "on: push\njobs:\n  a:\n    steps:\n      - run: >-\n          echo\n          hi\n"
    assert CiChanged.changed_jobs(block, String.replace(block, "hi", "bye")) == ["a"]
    assert_raise ArgumentError, fn -> CiChanged.changed_jobs(nil, "jobs: [bad]\n") end
    assert_raise ArgumentError, fn -> CiChanged.changed_jobs(nil, "jobs: {a: 1, a: 2}\n") end
  end

  test "executor preserves exact job argv and propagates failure" do
    jobs = [%{workflow: ".github/workflows/ci.yml", job: "new-job"}]

    runner = fn "act", argv, opts ->
      assert argv == [
               "pull_request",
               "-W",
               ".github/workflows/ci.yml",
               "-j",
               "new-job",
               "--bind",
               "--container-options",
               "--cap-drop=ALL"
             ]

      assert opts[:cd] == "/repo"
      {"real job failed", 1}
    end

    assert [%{exit: 1}] = CiChanged.execute_jobs("/repo", jobs, runner)
  end

  test "the existing atom rule names the unsafe API" do
    [finding] = Hypatia.Rules.CodeSafety.scan_content("String.to_atom(input)", "elixir")
    assert finding.rule == :elixir_atom_from_user
    assert finding.description =~ "String.to_atom with user input"
    assert Hypatia.Rules.CodeSafety.scan_content("String.to_existing_atom(input)", "elixir") == []
  end
end

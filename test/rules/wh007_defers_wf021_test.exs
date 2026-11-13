# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.Wh007DefersWf021Test do
  # Owner ruling 2026-11-13: WF021 wins over WH007. WH007 must never advise
  # cancel-in-progress on a workflow WF021 skips (publishers, mutators,
  # anything with a write scope).
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowHardening, as: WH

  setup do
    dir = Path.join(System.tmp_dir!(), "wh007-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, ".github/workflows"))
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  defp write(dir, name, body), do: File.write!(Path.join([dir, ".github/workflows", name]), body)

  test "a read-only PR check without concurrency fires", %{dir: dir} do
    write(dir, "ci.yml", """
    on:
      pull_request:
    permissions:
      contents: read
    jobs:
      test:
        runs-on: ubuntu-latest
        steps:
          - run: echo ok
    """)

    assert [%{rule: "WH007"}] = WH.wh007_missing_concurrency(dir)
  end

  test "a PR workflow that pushes is skipped, as WF021 does", %{dir: dir} do
    write(dir, "sync.yml", """
    on:
      pull_request:
    permissions:
      contents: write
    jobs:
      sync:
        runs-on: ubuntu-latest
        steps:
          - run: git push origin HEAD
    """)

    assert WH.wh007_missing_concurrency(dir) == []
  end

  test "a Pages deploy is skipped", %{dir: dir} do
    write(dir, "pages.yml", """
    on:
      pull_request:
    permissions:
      contents: read
    jobs:
      deploy:
        runs-on: ubuntu-latest
        steps:
          - uses: actions/deploy-pages@v4
    """)

    assert WH.wh007_missing_concurrency(dir) == []
  end

  test "no permissions block (default token) is not advised cancel-in-progress", %{dir: dir} do
    write(dir, "ci.yml", """
    on:
      pull_request:
    jobs:
      test:
        runs-on: ubuntu-latest
        steps:
          - run: echo ok
    """)

    assert WH.wh007_missing_concurrency(dir) == []
  end
end

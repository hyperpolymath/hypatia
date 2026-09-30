# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.NpxInWorkflowTest do
  use ExUnit.Case, async: true

  alias Hypatia.CLI

  setup do
    dir = Path.join(System.tmp_dir!(), "hyp-npx-test-#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  defp findings(dir, line) do
    path = Path.join(dir, "scripts/x.sh")
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, line <> "\n")

    dir
    |> CLI.collect_findings([:content_patterns])
    |> Enum.filter(&(&1.type == "npx_in_workflow"))
  end

  test "running npx is reported", %{dir: dir} do
    assert [_] = findings(dir, "npx prettier --check .")
  end

  test "npx after a quoted echo on the same line is still reported", %{dir: dir} do
    assert [_] = findings(dir, ~s|echo "formatting" && npx prettier .|)
  end

  # The three lines hypatia reported on echidna's scripts/ban-npm.sh.
  for line <- [
        ~s{if grep -r "npm install\\|npm i \\|npx \\|npm run" scripts/ 2>/dev/null; then},
        ~s{if [ -f "Justfile" ] && grep -q "npm\\|npx" Justfile; then},
        ~s{echo "  ✗ npm, npx, node_modules"}
      ] do
    test "npx named in a quoted grep/echo argument is not reported: #{line}", %{dir: dir} do
      assert [] = findings(dir, unquote(line))
    end
  end
end

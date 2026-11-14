# SPDX-License-Identifier: MPL-2.0
defmodule Mix.Tasks.Hypatia.Coverage.ReportTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Mix.Tasks.Hypatia.Coverage.Report, as: Task

  @fixtures Path.expand("../fixtures/coverage", __DIR__)
  @demo_root Path.join(@fixtures, "demo_repo")
  @sha "0123456789abcdef0123456789abcdef01234567"

  setup do
    tmp = Path.join(System.tmp_dir!(), "hypatia-covtask-#{System.unique_integer([:positive])}")
    File.mkdir_p!(tmp)
    on_exit(fn -> File.rm_rf(tmp) end)

    # The fixtures were produced in CI-like paths; point them at the checked-in demo repo so the
    # task's single --root both strips the prefix and finds the file (as in real CI).
    for n <- ~w(raw_branch proven_branch) do
      text = @fixtures |> Path.join("llvm_#{n}.json") |> File.read!()

      File.write!(
        Path.join(tmp, "#{n}.json"),
        String.replace(text, "/home/runner/work/demo/demo", @demo_root)
      )
    end

    %{tmp: tmp}
  end

  defp args(tmp, extra \\ []) do
    [
      "--raw",
      Path.join(tmp, "raw_branch.json"),
      "--proven",
      Path.join(tmp, "proven_branch.json"),
      "--evidence",
      Path.join(@fixtures, "evidence.json"),
      "--repo",
      "example/demo",
      "--sha",
      @sha,
      "--root",
      @demo_root
    ] ++ extra
  end

  defp run(argv) do
    me = self()
    out = capture_io(fn -> send(me, {:r, Task.execute(argv)}) end)
    assert_received {:r, result}
    {result, out}
  end

  test "judged fail: Markdown on stdout, exit 1", %{tmp: tmp} do
    {{:ok, 1, output}, out} = run(args(tmp))
    assert out == output
    assert out =~ "## Hypatia coverage — FAIL"
    assert out =~ "SHADOW — not a gate"
    assert out =~ "| Lines | 20/38 (52.6%) | 32/38 (84.2%) |"
  end

  test "--shadow forces exit 0 but prints the true verdict", %{tmp: tmp} do
    {{:ok, 0, _}, out} = run(args(tmp, ["--shadow"]))
    assert out =~ "## Hypatia coverage — FAIL"
  end

  test "unjudged: exit 2, and --shadow still prints UNJUDGED", %{tmp: tmp} do
    bad =
      Enum.map(
        args(tmp),
        &if(&1 == Path.join(tmp, "raw_branch.json"), do: "/nonexistent/raw.json", else: &1)
      )

    {{:ok, 2, _}, out} = run(bad)
    assert out =~ "UNJUDGED" and out =~ "COV-E001"
    {{:ok, 0, _}, shadow_out} = run(bad ++ ["--shadow"])
    assert shadow_out =~ "UNJUDGED"
  end

  test "judged pass: exit 0 (LCOV inputs)", %{tmp: tmp} do
    lcov = Path.join(tmp, "all.info")
    File.write!(lcov, "SF:src/lib.rs\nDA:1,1\nDA:2,4\nend_of_record\n")
    {{:ok, 0, _}, out} = run(["--raw", lcov, "--proven", lcov] ++ Enum.drop(args(tmp), 4))
    assert out =~ "— PASS"
    assert out =~ "| Lines | 2/2 (100.0%) | 2/2 (100.0%) |"
    assert out =~ "| Branches | not measurable | not measurable |"
  end

  test "--format json", %{tmp: tmp} do
    {{:ok, 1, output}, _} = run(args(tmp, ["--format", "json"]))
    v = Jason.decode!(output)
    assert v["outcome"] == "FAIL" and v["shadow"] == true
    assert v["line"]["proven"] == 20
  end

  test "--out writes the file and prints one status line", %{tmp: tmp} do
    path = Path.join([tmp, "sub", "r.md"])
    {{:ok, 1, output}, out} = run(args(tmp, ["--out", path]))
    assert File.read!(path) == output
    assert out =~ "FAIL (shadow) -> #{path}"
    refute out =~ "### "
  end

  test "--min-cases changes what counts", %{tmp: tmp} do
    {{:ok, 2, _}, out} = run(args(tmp, ["--min-cases", "100000"]))
    assert out =~ "COV-E005"
  end

  test "usage errors are tagged, not raised", %{tmp: tmp} do
    assert {{:usage, "missing required option(s): --raw" <> _}, _} = run(tl(tl(args(tmp))))

    assert {{:usage, "--repo must look like OWNER/NAME" <> _}, _} =
             run(args(tmp, ["--repo", "nope"]))

    assert {{:usage, "--format must be" <> _}, _} = run(args(tmp, ["--format", "xml"]))

    assert {{:usage, "unknown or malformed option(s): --bogus"}, _} =
             run(args(tmp, ["--bogus", "1"]))
  end

  test "run/1 exits with the shutdown code and zero returns normally", %{tmp: tmp} do
    assert {:shutdown, 1} == catch_exit(capture_io(fn -> Task.run(args(tmp)) end))
    assert capture_io(fn -> assert Task.run(args(tmp, ["--shadow"])) == :ok end) =~ "FAIL"
  end

  test "run/1 usage error exits 2" do
    capture_io(:stderr, fn -> assert {:shutdown, 2} == catch_exit(Task.run([])) end)
  end
end

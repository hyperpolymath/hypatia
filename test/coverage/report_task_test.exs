# SPDX-License-Identifier: MPL-2.0
defmodule Mix.Tasks.Hypatia.Coverage.ReportTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Hypatia.Coverage.ReportMeta
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

    for {n, role} <- [{"raw_branch", "raw"}, {"proven_branch", "proven"}] do
      ReportMeta.write!(Path.join(tmp, "#{n}.json.meta.json"), meta(role))
    end

    %{tmp: tmp}
  end

  defp meta(role, extra \\ %{}) do
    Map.merge(
      %{
        repo: "example/demo",
        commit: @sha,
        run_id: "r1",
        role: role,
        tool: "cargo-llvm-cov",
        tool_version: "0.6"
      },
      extra
    )
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

  test "a missing default sidecar is UNJUDGED COV-E013", %{tmp: tmp} do
    File.rm!(Path.join(tmp, "raw_branch.json.meta.json"))
    {{:ok, 2, out}, _} = run(args(tmp))
    assert out =~ "COV-E013"
  end

  test "--raw-meta/--proven-meta override the default; a wrong commit is COV-E007", %{tmp: tmp} do
    other = Path.join(tmp, "elsewhere.json")
    ReportMeta.write!(other, meta("raw", %{commit: String.duplicate("b", 40)}))
    {{:ok, 2, out}, _} = run(args(tmp, ["--raw-meta", other]))
    assert out =~ "COV-E007"
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
    lcov2 = Path.join(tmp, "counted.info")
    File.write!(lcov, "SF:src/lib.rs\nDA:1,1\nDA:2,4\nend_of_record\n")
    File.cp!(lcov, lcov2)
    ReportMeta.write!(lcov <> ".meta.json", meta("raw"))
    ReportMeta.write!(lcov2 <> ".meta.json", meta("proven"))
    {{:ok, 0, _}, out} = run(["--raw", lcov, "--proven", lcov2] ++ Enum.drop(args(tmp), 4))
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

  test "--min-cases below the floor of 100, or not an integer, is a usage error", %{tmp: tmp} do
    assert {{:usage, "--min-cases must be an integer >= 100" <> _}, _} =
             run(args(tmp, ["--min-cases", "99"]))

    assert {{:usage, "unknown or malformed option(s): --min-cases" <> _}, _} =
             run(args(tmp, ["--min-cases", "lots"]))

    assert {:ok, _, _} = elem(run(args(tmp, ["--min-cases", "100"])), 0)
  end

  test "a relative --root is expanded so absolute prefixes are stripped", %{tmp: tmp} do
    rel = Path.relative_to(@demo_root, File.cwd!())
    assert Path.type(rel) == :relative
    argv = Enum.drop(args(tmp), -2) ++ ["--root", rel]
    {{:ok, 1, _}, out} = run(argv)
    assert out =~ "| Lines | 20/38 (52.6%) | 32/38 (84.2%) |"
    refute out =~ "COV-E003"
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

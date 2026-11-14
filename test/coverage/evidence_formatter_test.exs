# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.EvidenceFormatterTest do
  use ExUnit.Case, async: false

  alias Hypatia.Coverage.{Evidence, EvidenceFormatter}

  @repo "example/demo"
  @sha String.duplicate("a", 40)
  @cfg [repo: @repo, commit: @sha, run_id: "99"]

  defp test_struct(name, state, tags) do
    %ExUnit.Test{
      name: name,
      case: __MODULE__.Sample,
      module: __MODULE__.Sample,
      state: state,
      time: 12_345,
      tags: Map.merge(%{file: Path.join(File.cwd!(), "test/sample_test.exs"), line: 7}, tags)
    }
  end

  defp assertion_failure,
    do: {:failed, [{:error, %ExUnit.AssertionError{message: "no"}, []}]}

  defp counted?(record),
    do: Evidence.counted?(record, repo: @repo, commit: @sha)

  describe "record/2" do
    test "a stronger kind without fixtures gets none, so it is not admissible" do
      tag = %{evidence: %{kind: "R", generator: "g", seed: "1", cases: 500}}
      r = EvidenceFormatter.record(test_struct(:"test prop", nil, tag), @cfg)
      refute Map.has_key?(r, "fixtures")
      assert counted?(r) == {:not_counted, :no_fixtures}
    end

    test "parameterized runs get distinct test ids and record ids" do
      t = test_struct(:"test p", nil, %{})
      a = EvidenceFormatter.record(Map.put(t, :parameters, %{n: 1}), @cfg)
      b = EvidenceFormatter.record(Map.put(t, :parameters, %{n: 2}), @cfg)
      assert a["test_id"]["parameters"] == %{"n" => 1}
      assert a["id"] != b["id"]
    end

    test "an untagged test is kind E, passed, example_test, and fully shaped" do
      r = EvidenceFormatter.record(test_struct(:"test plain", nil, %{}), @cfg)

      assert r["schema"] == "hypatia.proven-coverage.evidence/1"
      assert r["kind"] == "E"
      assert r["language"] == "elixir"
      assert r["repo"] == @repo and r["commit"] == @sha and r["run_id"] == "99"

      assert r["test_id"] == %{
               "module_path" => "Hypatia.Coverage.EvidenceFormatterTest.Sample",
               "name" => "test plain",
               "file" => "test/sample_test.exs",
               "line" => 7
             }

      assert r["run"]["outcome"] == "passed"
      assert r["run"]["duration_ms"] == 12
      assert r["run"]["toolchain"] =~ "elixir"
      assert r["id"] =~ ~r/\A[0-9a-f]{64}\z/
      assert counted?(r) == {:not_counted, :example_test}
    end

    test "an R tag with property, fixtures and mutation counts" do
      tag = %{
        kind: "R",
        generator: "int 1..50",
        seed: 42,
        cases: 500,
        fixtures: %{
          silence: "n in 1..50",
          firing: %{source: "off by one", defect_class: :wrong_branch}
        },
        mutation: %{tool: "muex", killed: 7, survived: 0}
      }

      r = EvidenceFormatter.record(test_struct(:"test prop", nil, %{evidence: tag}), @cfg)
      assert r["kind"] == "R"
      assert r["run"]["property"] == %{"generator" => "int 1..50", "seed" => 42, "cases" => 500}
      assert r["run"]["mutation"]["tool"] == "muex"
      assert r["fixtures"]["firing"]["defect_class"] == "wrong_branch"
      assert counted?(r) == :counted
    end

    test "a nested property map and string keys work the same" do
      tag = %{
        "kind" => "R",
        "property" => %{"generator" => "g", "seed" => "1", "cases" => 100},
        "fixtures" => %{"cannot_fail_by_design" => "pure"},
        "mutation" => %{"tool" => "muex", "killed" => 1, "survived" => 0}
      }

      r = EvidenceFormatter.record(test_struct(:"test p2", nil, %{evidence: tag}), @cfg)
      assert counted?(r) == :counted
    end

    test "too few cases and no seed are overclaims, as the tags say" do
      few = %{
        kind: "R",
        generator: "g",
        seed: 1,
        cases: 10,
        fixtures: %{cannot_fail_by_design: "x"}
      }

      r = EvidenceFormatter.record(test_struct(:"test few", nil, %{evidence: few}), @cfg)
      assert counted?(r) == {:not_counted, :too_few_cases}

      noseed = %{kind: "R", generator: "g", cases: 500, fixtures: %{cannot_fail_by_design: "x"}}
      r = EvidenceFormatter.record(test_struct(:"test noseed", nil, %{evidence: noseed}), @cfg)
      assert counted?(r) == {:not_counted, :no_seed}
    end

    test "C and B tags, and M" do
      c = %{
        kind: "C",
        fixtures: %{cannot_fail_by_design: "vectors"},
        conformance: %{
          spec: "rfc8259",
          version: "1",
          vectors_sha256: String.duplicate("0", 64),
          vectors: 8,
          failed: 0
        },
        mutation: %{tool: "muex", killed: 2, survived: 0}
      }

      assert counted?(EvidenceFormatter.record(test_struct(:"test c", nil, %{evidence: c}), @cfg)) ==
               :counted

      m = %{kind: :M}
      r = EvidenceFormatter.record(test_struct(:"test m", nil, %{evidence: m}), @cfg)
      assert r["kind"] == "M"
      assert counted?(r) == {:not_counted, :mock_test}
    end

    test "an unknown kind is kept as given and not counted" do
      r = EvidenceFormatter.record(test_struct(:"test x", nil, %{evidence: %{kind: "Z"}}), @cfg)
      assert counted?(r) == {:not_counted, :unknown_kind}
    end

    test "outcomes" do
      o = &EvidenceFormatter.outcome/1
      assert o.(nil) == "passed"
      assert o.(assertion_failure()) == "failed"
      assert o.({:failed, [{:error, %RuntimeError{message: "x"}, []}]}) == "error"
      assert o.({:failed, [{:exit, :boom, []}]}) == "error"
      assert o.({:failed, [{:throw, :x, []}]}) == "error"
      assert o.({:skipped, "later"}) == "skipped"
      assert o.({:excluded, "tag"}) == "skipped"
      assert o.({:invalid, __MODULE__}) == "error"
    end

    test "a failed R test is not counted (not_passed)" do
      tag = %{
        kind: "R",
        generator: "g",
        seed: 1,
        cases: 500,
        fixtures: %{cannot_fail_by_design: "x"}
      }

      r =
        EvidenceFormatter.record(
          test_struct(:"test f", assertion_failure(), %{evidence: tag}),
          @cfg
        )

      assert r["run"]["outcome"] == "failed"
      assert counted?(r) == {:not_counted, :not_passed}
    end

    test "repo and commit unknown: keys are left out, nothing invented" do
      for k <- ~w(GITHUB_REPOSITORY GITHUB_SHA), do: System.delete_env(k)
      r = EvidenceFormatter.record(test_struct(:"test u", nil, %{}), [])
      refute Map.has_key?(r, "repo")
      refute Map.has_key?(r, "commit")
    end

    test "odd values in tags still encode as JSON" do
      tag = %{kind: "R", generator: {:tuple, self()}, seed: 1, cases: 1}
      r = EvidenceFormatter.record(test_struct(:"test odd", nil, %{evidence: tag}), @cfg)
      assert {:ok, _} = Jason.encode(r)
    end

    test "ids differ per test and kind, and are stable" do
      a = EvidenceFormatter.record(test_struct(:"test a", nil, %{}), @cfg)
      b = EvidenceFormatter.record(test_struct(:"test b", nil, %{}), @cfg)
      assert a["id"] != b["id"]
      assert a["id"] == EvidenceFormatter.record(test_struct(:"test a", nil, %{}), @cfg)["id"]
    end
  end

  describe "the GenServer (handle_cast)" do
    setup do
      out =
        Path.join(
          System.tmp_dir!(),
          "hypatia-ev-#{System.unique_integer([:positive])}/evidence.json"
        )

      on_exit(fn -> File.rm_rf(Path.dirname(out)) end)
      %{out: out}
    end

    test "writes a file that round-trips through Evidence.load/1", %{out: out} do
      state = %{config: Keyword.put(@cfg, :out, out), records: []}

      tag = %{
        kind: "R",
        generator: "g",
        seed: 1,
        cases: 500,
        fixtures: %{cannot_fail_by_design: "x"},
        mutation: %{tool: "muex", killed: 1, survived: 0}
      }

      {:noreply, state} =
        EvidenceFormatter.handle_cast({:test_finished, test_struct(:"test one", nil, %{})}, state)

      {:noreply, state} =
        EvidenceFormatter.handle_cast(
          {:test_finished, test_struct(:"test two", nil, %{evidence: tag})},
          state
        )

      {:noreply, state} =
        EvidenceFormatter.handle_cast(
          {:test_finished, test_struct(:"test three", {:excluded, "proven"}, %{})},
          state
        )

      {:noreply, state} = EvidenceFormatter.handle_cast({:module_started, :ignored}, state)
      {:noreply, _} = EvidenceFormatter.handle_cast({:suite_finished, %{run: 1}}, state)

      assert {:ok, records} = Evidence.load(out)
      assert Enum.map(records, & &1["test_id"]["name"]) == ["test one", "test two", "test three"]
      assert Enum.map(records, & &1["kind"]) == ["E", "R", "E"]

      assert Enum.map(records, &counted?/1) == [
               {:not_counted, :example_test},
               :counted,
               {:not_counted, :not_passed}
             ]

      assert Enum.at(records, 2)["run"]["outcome"] == "skipped"
    end

    test "with no output path nothing is written", %{out: out} do
      System.delete_env("HYPATIA_EVIDENCE_OUT")
      state = %{config: @cfg, records: []}
      assert {:noreply, _} = EvidenceFormatter.handle_cast({:suite_finished, %{}}, state)
      refute File.exists?(out)
    end

    test "init reads the application config" do
      assert {:ok, %{records: []}} = EvidenceFormatter.init([])
    end
  end

  describe "a real ExUnit run (subprocess)" do
    @script """
    ExUnit.start(autorun: false)
    ExUnit.configure(formatters: [Hypatia.Coverage.EvidenceFormatter])
    if System.get_env("ONLY_PROVEN"), do: ExUnit.configure(include: [:proven], exclude: [:test])

    defmodule RunDemoTest do
      use ExUnit.Case
      test "plain", do: assert(1 == 1)

      @tag :proven
      @tag evidence: %{kind: "R", generator: "g", seed: 1, cases: 100, fixtures: %{cannot_fail_by_design: "x"}}
      test "prop", do: assert(true)

      test "bad", do: assert(1 == 2)
      test "boom", do: raise("x")
    end

    ExUnit.run()
    """

    defp run_script(extra_env) do
      root = Path.join(System.tmp_dir!(), "hypatia-ev-run-#{System.unique_integer([:positive])}")
      File.mkdir_p!(root)
      on_exit(fn -> File.rm_rf(root) end)
      script = Path.join(root, "run.exs")
      out = Path.join(root, "ev.json")
      File.write!(script, @script)

      {_o, 0} =
        System.cmd(
          "elixir",
          [
            "-pa",
            Path.expand("_build/test/lib/hypatia/ebin"),
            "-pa",
            Path.expand("_build/test/lib/jason/ebin"),
            script
          ],
          env:
            [{"HYPATIA_EVIDENCE_OUT", out}, {"GITHUB_REPOSITORY", @repo}, {"GITHUB_SHA", @sha}] ++
              extra_env,
          stderr_to_stdout: true
        )

      {:ok, records} = Evidence.load(out)
      Map.new(records, &{&1["test_id"]["name"], &1})
    end

    test "every test gets a record with the right outcome and kind" do
      by = run_script([])

      assert by |> Map.keys() |> Enum.sort() == [
               "test bad",
               "test boom",
               "test plain",
               "test prop"
             ]

      assert by["test plain"]["run"]["outcome"] == "passed"
      assert by["test bad"]["run"]["outcome"] == "failed"
      assert by["test boom"]["run"]["outcome"] == "error"
      assert by["test prop"]["kind"] == "R"
      assert by["test plain"]["kind"] == "E"
      assert by["test prop"]["test_id"]["module_path"] == "RunDemoTest"
      assert counted?(by["test prop"]) == {:not_counted, :no_mutation_result}
    end

    test "--only proven style selection: untagged tests are recorded as skipped" do
      by = run_script([{"ONLY_PROVEN", "1"}])
      assert by["test prop"]["run"]["outcome"] == "passed"
      assert by["test plain"]["run"]["outcome"] == "skipped"
    end
  end
end

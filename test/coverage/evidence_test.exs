# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.EvidenceTest do
  use ExUnit.Case, async: true

  alias Hypatia.Coverage.Evidence

  @fixture Path.expand("../fixtures/coverage/evidence.json", __DIR__)

  @sha "a3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  @rung %{
    "theorem" => "parseTotal",
    "file" => "src/Proven/SafeJson/Proofs.idr",
    "line" => 12,
    "checker" => "idris2",
    "check" => "passed"
  }

  defp rec(kind, run \\ %{}, extra \\ %{}) do
    Map.merge(
      %{
        "id" => "x",
        "kind" => kind,
        "test_id" => %{"name" => "t"},
        "fixtures" => %{"silence" => "s", "firing" => %{"source" => "f", "defect_class" => "d"}},
        "run" => Map.merge(%{"outcome" => "passed"}, run)
      },
      extra
    )
  end

  defp prop(overrides \\ %{}),
    do: rec("R", %{"property" => Map.merge(%{"seed" => "42", "cases" => 100}, overrides)})

  defp conf(overrides \\ %{}) do
    base = %{
      "spec" => "RFC 8259",
      "version" => "2017",
      "vectors_sha256" => @sha,
      "vectors" => 64,
      "failed" => 0
    }

    rec("C", %{"conformance" => Map.merge(base, overrides)})
  end

  defp bench(checksum),
    do: rec("B", %{"benchmark" => %{"iterations" => 10, "checksum" => checksum}})

  defp proof(ladder), do: rec("P", %{"proof" => %{"ladder" => ladder}})

  describe "counted records" do
    test "R with seed and cases at the default floor" do
      assert Evidence.counted?(prop()) == :counted
    end

    test "C with spec, version, hash and no failures" do
      assert Evidence.counted?(conf()) == :counted
    end

    test "B with a checksum that holds" do
      assert Evidence.counted?(bench(true)) == :counted
    end

    test "P with a complete, passed ladder (three accepted spellings of 'passed')" do
      assert Evidence.counted?(proof([@rung])) == :counted

      assert Evidence.counted?(proof([Map.put(@rung, "check", %{"outcome" => "passed"})])) ==
               :counted

      assert Evidence.counted?(proof([@rung |> Map.delete("check") |> Map.put("passed", true)])) ==
               :counted
    end

    test "a cannot_fail_by_design fixture is admissible" do
      r = prop() |> Map.put("fixtures", %{"cannot_fail_by_design" => "total function"})
      assert Evidence.counted?(r) == :counted
    end
  end

  describe "not counted: general" do
    for outcome <- ["failed", "error", "skipped", nil] do
      test "outcome #{inspect(outcome)}" do
        r = prop() |> put_in(["run", "outcome"], unquote(outcome))
        assert Evidence.counted?(r) == {:not_counted, :not_passed}
      end
    end

    test "no run at all" do
      assert Evidence.counted?(%{"kind" => "R"}) == {:not_counted, :not_passed}
    end

    test "FixtureDebt" do
      r = prop() |> Map.put("fixtures", %{"debt" => "ISSUE-12"})
      assert Evidence.counted?(r) == {:not_counted, :fixture_debt}
    end

    test "not admissible: no fixtures, half a pair, or an empty reason" do
      for fx <- [
            nil,
            %{},
            %{"silence" => "s"},
            %{"firing" => %{"source" => "f"}},
            %{"silence" => " ", "firing" => %{"source" => "f"}},
            %{"cannot_fail_by_design" => ""},
            %{"cannot_fail_by_design" => "   "}
          ] do
        r = if fx, do: Map.put(prop(), "fixtures", fx), else: Map.delete(prop(), "fixtures")
        assert Evidence.counted?(r) == {:not_counted, :no_fixtures}, inspect(fx)
      end
    end

    test "kind E never counts, even when passed and well formed" do
      assert Evidence.counted?(rec("E")) == {:not_counted, :example_test}
    end

    test "kind M never counts" do
      assert Evidence.counted?(rec("M")) == {:not_counted, :mock_test}
    end

    test "unknown or missing kind" do
      assert Evidence.counted?(rec("Z")) == {:not_counted, :unknown_kind}
      assert Evidence.counted?(Map.delete(rec("R"), "kind")) == {:not_counted, :unknown_kind}
    end

    test "a failed run beats a kind claim; debt beats kind requirements" do
      assert Evidence.counted?(rec("E", %{"outcome" => "failed"})) == {:not_counted, :not_passed}

      assert Evidence.counted?(Map.put(rec("R"), "fixtures", %{"debt" => "x"})) ==
               {:not_counted, :fixture_debt}
    end
  end

  describe "not counted: R" do
    test "no seed" do
      assert Evidence.counted?(prop(%{"seed" => nil})) == {:not_counted, :no_seed}
      assert Evidence.counted?(prop(%{"seed" => ""})) == {:not_counted, :no_seed}
      assert Evidence.counted?(rec("R")) == {:not_counted, :no_seed}
    end

    test "seed 0 is a seed" do
      assert Evidence.counted?(prop(%{"seed" => 0})) == :counted
    end

    test "cases below the default floor of 100" do
      assert Evidence.counted?(prop(%{"cases" => 99})) == {:not_counted, :too_few_cases}
      assert Evidence.counted?(prop(%{"cases" => nil})) == {:not_counted, :too_few_cases}
      assert Evidence.counted?(prop(%{"cases" => "1000"})) == {:not_counted, :too_few_cases}
    end

    test "the floor is configurable per repo" do
      r = prop(%{"cases" => 500})
      assert Evidence.counted?(r, min_cases: 500) == :counted
      assert Evidence.counted?(r, min_cases: 501) == {:not_counted, :too_few_cases}
      assert Evidence.counted?(prop(%{"cases" => 10}), min_cases: 10) == :counted
    end
  end

  describe "not counted: C" do
    test "missing spec, version, vector hash" do
      assert Evidence.counted?(conf(%{"spec" => nil})) == {:not_counted, :no_spec}
      assert Evidence.counted?(conf(%{"version" => " "})) == {:not_counted, :no_version}

      assert Evidence.counted?(conf(%{"vectors_sha256" => nil})) ==
               {:not_counted, :no_vectors_sha256}
    end

    test "any failed vector, or an unknown failure count" do
      assert Evidence.counted?(conf(%{"failed" => 1})) == {:not_counted, :conformance_failures}
      assert Evidence.counted?(conf(%{"failed" => nil})) == {:not_counted, :conformance_failures}
    end

    test "no conformance block at all" do
      assert Evidence.counted?(rec("C")) == {:not_counted, :no_spec}
    end
  end

  describe "not counted: B" do
    test "checksum false, missing or not exactly true" do
      assert Evidence.counted?(bench(false)) == {:not_counted, :no_checksum}
      assert Evidence.counted?(bench(nil)) == {:not_counted, :no_checksum}
      assert Evidence.counted?(bench("yes")) == {:not_counted, :no_checksum}
      assert Evidence.counted?(rec("B")) == {:not_counted, :no_checksum}
    end
  end

  describe "not counted: P" do
    test "empty or missing ladder" do
      assert Evidence.counted?(proof([])) == {:not_counted, :empty_ladder}
      assert Evidence.counted?(rec("P")) == {:not_counted, :empty_ladder}
      assert Evidence.counted?(proof("nope")) == {:not_counted, :empty_ladder}
    end

    test "a rung missing theorem, file or checker" do
      for k <- ["theorem", "file", "checker"] do
        assert Evidence.counted?(proof([Map.delete(@rung, k)])) ==
                 {:not_counted, :incomplete_rung}
      end

      assert Evidence.counted?(proof([@rung, "not a rung"])) == {:not_counted, :incomplete_rung}
    end

    test "a rung with no passed check, in any rung" do
      assert Evidence.counted?(proof([Map.delete(@rung, "check")])) ==
               {:not_counted, :proof_check_not_passed}

      assert Evidence.counted?(proof([%{@rung | "check" => "failed"}])) ==
               {:not_counted, :proof_check_not_passed}

      assert Evidence.counted?(proof([@rung, %{@rung | "check" => "failed"}])) ==
               {:not_counted, :proof_check_not_passed}
    end
  end

  describe "overclaim (COV-E010)" do
    test "kind-requirement failures are overclaims; general exclusions are not" do
      assert Evidence.overclaim?(Evidence.counted?(prop(%{"seed" => nil})))
      assert Evidence.overclaim?(Evidence.counted?(proof([])))
      assert Evidence.overclaim?(Evidence.counted?(bench(false)))
      assert Evidence.overclaim?(Evidence.counted?(conf(%{"failed" => 2})))
      refute Evidence.overclaim?(Evidence.counted?(rec("E")))
      refute Evidence.overclaim?(Evidence.counted?(rec("R", %{"outcome" => "failed"})))

      refute Evidence.overclaim?(
               Evidence.counted?(Map.put(rec("R"), "fixtures", %{"debt" => "x"}))
             )

      refute Evidence.overclaim?(:counted)
    end

    test "every not-counted reason has display text and is classified one way or the other" do
      reasons =
        [
          prop(%{"seed" => nil}),
          prop(%{"cases" => 1}),
          conf(%{"spec" => nil}),
          conf(%{"version" => nil}),
          conf(%{"vectors_sha256" => nil}),
          conf(%{"failed" => 1}),
          bench(false),
          proof([]),
          proof([%{}]),
          proof([Map.delete(@rung, "check")]),
          rec("E"),
          rec("M"),
          rec("Z"),
          Map.put(rec("R"), "fixtures", %{"debt" => "x"}),
          Map.delete(rec("R"), "fixtures"),
          rec("R", %{"outcome" => "failed"})
        ]
        |> Enum.map(fn r ->
          {:not_counted, why} = Evidence.counted?(r)
          why
        end)

      assert Enum.sort(Enum.uniq(reasons)) ==
               Enum.sort(
                 [
                   :fixture_debt,
                   :no_fixtures,
                   :example_test,
                   :mock_test,
                   :unknown_kind,
                   :not_passed
                 ] ++
                   Evidence.overclaim_reasons()
               )

      for why <- reasons, do: assert(Evidence.reason_text(why) != to_string(why))
    end
  end

  describe "loading" do
    test "the fixture file" do
      assert {:ok, records} = Evidence.load(@fixture)
      assert length(records) == 6
      results = Evidence.classify_all(records)
      assert Enum.count(results, &(elem(&1, 1) == :counted)) == 2
    end

    test "classify_all keeps input order and loses nothing" do
      records = [rec("E"), prop(), rec("M")]

      assert Enum.map(Evidence.classify_all(records), &elem(&1, 1)) ==
               [{:not_counted, :example_test}, :counted, {:not_counted, :mock_test}]
    end

    test "bare array and wrapped object are both accepted" do
      assert {:ok, [%{"kind" => "E"}]} = Evidence.decode(~s([{"kind": "E"}]))

      assert {:ok, [%{"kind" => "E"}]} =
               Evidence.decode(
                 ~s({"schema": "#{Evidence.schema()}", "evidence": [{"kind": "E"}]})
               )
    end

    test "per-record schema must match when present" do
      ok = ~s([{"schema": "#{Evidence.schema()}", "kind": "E"}])
      assert {:ok, _} = Evidence.decode(ok)

      assert {:error, "evidence[1]: unsupported schema \"other/9\""} =
               Evidence.decode(~s([{"kind": "E"}, {"schema": "other/9"}]))
    end

    test "wrapped object with a wrong schema" do
      assert {:error, "unsupported schema \"x\""} =
               Evidence.decode(~s({"schema": "x", "evidence": []}))
    end

    test "errors: bad JSON, wrong shape, non-object records" do
      assert {:error, "invalid JSON at byte " <> _} = Evidence.decode("[{")
      assert {:error, "expected a JSON array" <> _} = Evidence.decode(~s({"a": 1}))
      assert {:error, "evidence[0] is not an object"} = Evidence.decode("[1]")
    end

    test "load/1 maps file problems to tagged errors" do
      assert {:error, {:missing, "/nonexistent/e.json"}} = Evidence.load("/nonexistent/e.json")
      path = Path.join(System.tmp_dir!(), "hypatia-ev-#{System.unique_integer([:positive])}.json")
      File.write!(path, "not json")
      on_exit(fn -> File.rm(path) end)
      assert {:error, {:parse, ^path, "invalid JSON at byte " <> _}} = Evidence.load(path)
    end
  end

  test "name/1 prefers module::name, then id" do
    assert Evidence.name(%{"test_id" => %{"module_path" => "m", "name" => "n"}}) == "m::n"
    assert Evidence.name(%{"test_id" => %{"name" => "n"}}) == "n"
    assert Evidence.name(%{"id" => "abc"}) == "abc"
    assert Evidence.name(%{}) == "(unnamed record)"
  end
end

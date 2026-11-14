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
    "checker_version" => "0.7.0",
    "statement_sha256" => @sha,
    "check" => "passed"
  }

  defp rec(kind, run \\ %{}, extra \\ %{}) do
    Map.merge(
      %{
        "id" => "x",
        "kind" => kind,
        "language" => "zig",
        "in_proven_run" => true,
        "test_id" => %{"name" => "t"},
        "fixtures" => %{"silence" => "s", "firing" => %{"source" => "f", "defect_class" => "d"}},
        "run" => Map.merge(%{"outcome" => "passed"}, run)
      },
      extra
    )
  end

  defp prop(overrides \\ %{}),
    do:
      rec("R", %{
        "property" => Map.merge(%{"generator" => "g", "seed" => "42", "cases" => 100}, overrides)
      })

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

  defp bench(checksum, overrides \\ %{}) do
    base = %{
      "workload" => "parse 1MiB",
      "iterations" => 10,
      "samples_ns" => [100, 110, 105],
      "median_ns" => 105,
      "runner" => "ubuntu-24.04 x86_64",
      "toolchain" => "rustc 1.80",
      "baseline" => "main@abc",
      "checksum" => checksum
    }

    rec("B", %{"benchmark" => Map.merge(base, overrides)})
  end

  defp proof(ladder, decls \\ ["Proven.SafeJson.parseTotal"]),
    do: rec("P", %{"proof" => %{"ladder" => ladder, "covers_decls" => decls}})

  @mut %{"tool" => "cargo-mutants", "killed" => 4, "survived" => 0}
  @sha1 "0123456789abcdef0123456789abcdef01234567"

  defp lang(r, l), do: Map.put(r, "language", l)
  defp mut(r, m), do: put_in(r, ["run", "mutation"], m)
  defp bound(r), do: Map.merge(r, %{"repo" => "o/r", "commit" => @sha1})

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

    test "P never counts in this increment, even when complete (three spellings of 'passed')" do
      nc = {:not_counted, :proof_binding_not_implemented}
      assert Evidence.counted?(proof([@rung])) == nc
      assert Evidence.counted?(proof([Map.put(@rung, "check", %{"outcome" => "passed"})])) == nc

      assert Evidence.counted?(proof([@rung |> Map.delete("check") |> Map.put("passed", true)])) ==
               nc

      refute Evidence.overclaim?(nc)
      assert Evidence.reason_text(:proof_binding_not_implemented) =~ "COV-E006"
    end

    test "a cannot_fail_by_design fixture is admissible" do
      r = prop() |> Map.put("fixtures", %{"cannot_fail_by_design" => "total function"})
      assert Evidence.counted?(r) == :counted
    end
  end

  describe "not counted: general" do
    test "proven-run membership must be affirmative, whichever collector wrote it" do
      assert Evidence.counted?(Map.delete(prop(), "in_proven_run")) ==
               {:not_counted, :not_in_proven_run}

      assert Evidence.counted?(Map.put(prop(), "in_proven_run", "yes")) ==
               {:not_counted, :not_in_proven_run}
    end

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
      assert Evidence.counted?(prop(%{"cases" => 150}), min_cases: 150) == :counted
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
      assert Evidence.counted?(conf(%{"failed" => nil})) == {:not_counted, :inconsistent_vectors}
    end

    test "vectors must be an integer >= 1; failed within 0..vectors" do
      for v <- [0, nil, "64", -1],
          do: assert(Evidence.counted?(conf(%{"vectors" => v})) == {:not_counted, :no_vectors})

      for f <- [-1, 65, "0"],
          do:
            assert(
              Evidence.counted?(conf(%{"failed" => f})) == {:not_counted, :inconsistent_vectors}
            )
    end

    test "a per-vector results list must agree with vectors and failed" do
      ok = List.duplicate(%{"passed" => true}, 64)
      assert Evidence.counted?(conf(%{"results" => ok})) == :counted

      assert Evidence.counted?(conf(%{"results" => Enum.take(ok, 63)})) ==
               {:not_counted, :inconsistent_vectors}

      bad = [%{"passed" => false} | tl(ok)]

      assert Evidence.counted?(conf(%{"results" => bad})) ==
               {:not_counted, :inconsistent_vectors}
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
      assert Evidence.counted?(rec("B")) == {:not_counted, :incomplete_benchmark}
    end

    test "incomplete benchmark" do
      for {k, v} <- [
            {"workload", " "},
            {"iterations", 0},
            {"samples_ns", []},
            {"samples_ns", [1, 0]},
            {"median_ns", 0},
            {"runner", nil},
            {"toolchain", ""},
            {"baseline", nil}
          ] do
        assert Evidence.counted?(bench(true, %{k => v})) == {:not_counted, :incomplete_benchmark}
      end
    end

    test "runner and toolchain may sit under run.*" do
      r = bench(true, %{"runner" => nil, "toolchain" => nil})
      r = put_in(r, ["run", "runner"], "ci") |> put_in(["run", "toolchain"], "rustc")
      assert Evidence.counted?(r) == :counted
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

  describe "mutation condition (Q7)" do
    for l <- ~w(rust js ts javascript typescript elixir julia), kind <- [:prop, :conf, :bench] do
      test "#{l} #{kind}: needs a mutation result" do
        r =
          case unquote(kind) do
            :prop -> prop()
            :conf -> conf()
            :bench -> bench(true)
          end
          |> lang(unquote(l))

        assert Evidence.counted?(r) == {:not_counted, :no_mutation_result}
        assert Evidence.counted?(mut(r, @mut)) == :counted
      end
    end

    test "malformed or weak results are :no_mutation_result" do
      r = lang(prop(), "Rust")

      for m <- [
            nil,
            "x",
            %{},
            %{@mut | "tool" => ""},
            %{@mut | "tool" => 3},
            %{@mut | "killed" => 0},
            %{@mut | "killed" => "4"},
            Map.delete(@mut, "survived"),
            %{@mut | "survived" => nil}
          ] do
        assert Evidence.counted?(mut(r, m)) == {:not_counted, :no_mutation_result}, inspect(m)
      end
    end

    test "surviving mutants" do
      r = lang(prop(), "elixir") |> mut(%{@mut | "survived" => 2})
      assert Evidence.counted?(r) == {:not_counted, :mutants_survived}
      refute Evidence.overclaim?({:not_counted, :mutants_survived})
      refute Evidence.overclaim?({:not_counted, :no_mutation_result})
    end

    test "languages without a tool, and P, are exempt" do
      for l <- ["zig", "idris2", "IDRIS2"],
          do: assert(Evidence.counted?(lang(prop(), l)) == :counted)

      assert Evidence.counted?(lang(proof([@rung]), "rust")) ==
               {:not_counted, :proof_binding_not_implemented}
    end
  end

  describe "language, generator, checker" do
    test "unknown, missing or misspelled language never fails open" do
      for l <- ["cobol", "rsut", "", nil, 5, %{}] do
        for r <- [prop(), conf(), bench(true)] do
          assert Evidence.counted?(lang(r, l)) == {:not_counted, :unknown_language}, inspect(l)
        end
      end

      assert Evidence.counted?(Map.delete(prop(), "language")) ==
               {:not_counted, :unknown_language}

      assert Evidence.counted?(lang(prop(), "idris2")) == :counted
      refute Evidence.overclaim?({:not_counted, :unknown_language})

      for why <- [:no_vectors, :inconsistent_vectors, :incomplete_benchmark],
          do: assert(Evidence.overclaim?({:not_counted, why}))
    end

    test "R needs a generator (overclaim)" do
      for g <- [nil, "", " ", 3] do
        r = prop(%{"generator" => g})
        assert Evidence.counted?(r) == {:not_counted, :no_generator}
      end

      assert Evidence.overclaim?({:not_counted, :no_generator})
    end

    test "P checker must be an approved deductive verifier" do
      for c <- ~w(kani tla echo) do
        assert Evidence.counted?(proof([%{@rung | "checker" => c}])) ==
                 {:not_counted, :unapproved_checker}
      end

      for c <- ~w(idris2 creusot verus prusti spark agda lean coq rocq Lean COQ) do
        assert Evidence.counted?(proof([%{@rung | "checker" => c}])) ==
                 {:not_counted, :proof_binding_not_implemented}
      end

      assert Evidence.overclaim?({:not_counted, :unapproved_checker})
    end
  end

  describe "proof identity" do
    test "each identity field is required" do
      for k <- ~w(line checker_version statement_sha256) do
        assert Evidence.counted?(proof([Map.delete(@rung, k)])) ==
                 {:not_counted, :incomplete_rung}
      end

      for {k, v} <- [
            {"line", "12"},
            {"statement_sha256", "abc"},
            {"statement_sha256", String.duplicate("g", 64)},
            {"checker_version", ""}
          ] do
        assert Evidence.counted?(proof([Map.put(@rung, k, v)])) ==
                 {:not_counted, :incomplete_rung}
      end
    end

    test "covers_decls must be a non-empty list of non-empty strings" do
      for d <- [[], nil, "x", [""], [1], ["a", " "]] do
        assert Evidence.counted?(proof([@rung], d)) == {:not_counted, :no_covers_decls},
               inspect(d)
      end

      assert Evidence.overclaim?({:not_counted, :no_covers_decls})
    end
  end

  describe "evidence binding" do
    test "counts only for the requested repo and commit" do
      r = bound(prop())
      assert Evidence.counted?(r, repo: "o/r", commit: @sha1) == :counted
      assert Evidence.counted?(r, repo: "O/R", commit: String.upcase(@sha1)) == :counted
      assert Evidence.counted?(r, repo: "o/other", commit: @sha1) == {:not_counted, :wrong_repo}
      assert Evidence.counted?(Map.delete(r, "repo"), repo: "o/r") == {:not_counted, :wrong_repo}

      assert Evidence.counted?(Map.delete(r, "commit"), commit: @sha1) ==
               {:not_counted, :no_commit}

      assert Evidence.counted?(%{r | "commit" => "abc"}, commit: @sha1) ==
               {:not_counted, :wrong_commit}

      assert Evidence.counted?(%{r | "commit" => String.duplicate("1", 40)}, commit: @sha1) ==
               {:not_counted, :wrong_commit}

      assert Evidence.counted?(%{r | "commit" => 5}, commit: @sha1) == {:not_counted, :no_commit}
    end

    test "without options nothing is bound" do
      assert Evidence.counted?(prop()) == :counted
    end
  end

  describe "min_cases floor" do
    test "the library cannot lower the floor of 100" do
      assert Evidence.counted?(prop(%{"cases" => 10}), min_cases: 1) ==
               {:not_counted, :too_few_cases}

      assert Evidence.counted?(prop(%{"cases" => 10}), min_cases: "x") ==
               {:not_counted, :too_few_cases}
    end
  end

  describe "malformed records never crash" do
    test "wrong-typed fields" do
      for r <- [
            %{
              "kind" => %{},
              "run" => %{"outcome" => "passed"},
              "fixtures" => %{"cannot_fail_by_design" => "x"}
            },
            %{"kind" => ["R"], "run" => "passed"},
            %{"kind" => "R", "run" => [1]},
            %{
              "kind" => "R",
              "run" => %{"outcome" => "passed", "property" => 5},
              "fixtures" => %{"cannot_fail_by_design" => "x"}
            },
            %{
              "kind" => "P",
              "language" => %{},
              "run" => %{
                "outcome" => "passed",
                "proof" => %{"ladder" => [1], "covers_decls" => 3}
              },
              "fixtures" => []
            },
            %{"kind" => "R", "fixtures" => 7, "test_id" => 9, "id" => %{}}
          ] do
        assert {:not_counted, _} = Evidence.counted?(r, repo: "o/r", commit: @sha1)
        assert is_binary(Evidence.name(r))
        assert Evidence.kind(r) == nil or is_binary(Evidence.kind(r))
      end
    end

    test "name/1 with non-string parts" do
      assert Evidence.name(%{"test_id" => %{"name" => %{"a" => 1}}, "id" => "i"}) == "i"
      assert Evidence.name(%{"test_id" => %{"name" => "n", "module_path" => 3}}) == "n"
      assert Evidence.name(%{"test_id" => "x", "id" => 4}) == "(unnamed record)"
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
          conf(%{"vectors" => 0}),
          conf(%{"failed" => nil}),
          rec("B"),
          bench(false),
          proof([]),
          proof([%{}]),
          proof([Map.delete(@rung, "check")]),
          proof([@rung], []),
          proof([%{@rung | "checker" => "kani"}]),
          proof([@rung]),
          prop(%{"generator" => nil}),
          lang(prop(), "cobol"),
          lang(prop(), "rust"),
          lang(prop(), "rust") |> mut(%{@mut | "survived" => 1}),
          rec("E"),
          rec("M"),
          rec("Z"),
          Map.put(rec("R"), "fixtures", %{"debt" => "x"}),
          Map.delete(rec("R"), "fixtures"),
          Map.put(prop(), "in_proven_run", false),
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
                   :not_in_proven_run,
                   :example_test,
                   :mock_test,
                   :unknown_kind,
                   :not_passed,
                   :no_mutation_result,
                   :unknown_language,
                   :proof_binding_not_implemented,
                   :mutants_survived
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

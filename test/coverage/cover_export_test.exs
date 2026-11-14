# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.CoverExportTest do
  use ExUnit.Case, async: false

  alias Hypatia.Coverage.{CoverExport, CoverTool, Facts, Lcov, Prop}

  @compile {:no_warn_undefined, :cover}

  describe "to_lcov/2 (pure)" do
    test "writes SF, DA, LF, LH and end_of_record, sorted, with no branch records" do
      analyses = %{A => [{{A, 5}, 0}, {{A, 3}, 2}], B => [{{B, 1}, 1}]}
      sources = %{A => "lib/a.ex", B => "lib/b.ex"}

      assert CoverExport.to_lcov(analyses, sources) ==
               "SF:lib/a.ex\nDA:3,2\nDA:5,0\nLF:2\nLH:1\nend_of_record\n" <>
                 "SF:lib/b.ex\nDA:1,1\nLF:1\nLH:1\nend_of_record\n"

      refute CoverExport.to_lcov(analyses, sources) =~ ~r/BR(DA|F|H)/
    end

    test "line 0 (generated code) is dropped" do
      out = CoverExport.to_lcov(%{A => [{{A, 0}, 9}, {{A, 2}, 1}]}, %{A => "lib/a.ex"})
      assert out =~ "DA:2,1"
      refute out =~ "DA:0"
    end

    test "modules sharing a source file are merged and their calls summed" do
      analyses = [{A, [{{A, 2}, 1}, {{A, 3}, 0}]}, {B, [{{B, 2}, 4}, {{B, 7}, 0}]}]
      out = CoverExport.to_lcov(analyses, %{A => "lib/x.ex", B => "lib/x.ex"})
      assert out == "SF:lib/x.ex\nDA:2,5\nDA:3,0\nDA:7,0\nLF:3\nLH:1\nend_of_record\n"
    end

    test "accepts {:ok, entries} as :cover.analyse/3 returns it; skips errors and unknown sources" do
      analyses = %{
        A => {:ok, [{{A, 1}, 1}]},
        B => {:error, {:not_cover_compiled, B}},
        C => [{{C, 1}, 1}]
      }

      assert CoverExport.to_lcov(analyses, %{A => "lib/a.ex", B => "lib/b.ex"}) ==
               "SF:lib/a.ex\nDA:1,1\nLF:1\nLH:1\nend_of_record\n"
    end

    test "empty input is an empty file, and parses as no files" do
      assert CoverExport.to_lcov(%{}, %{}) == ""
      assert {:ok, %{files: files}} = Lcov.parse("")
      assert files == %{}
    end

    test "relative_source/2 strips the root and leaves outside paths alone" do
      assert CoverExport.relative_source(~c"/r/lib/a.ex", "/r") == "lib/a.ex"
      assert CoverExport.relative_source("/r/lib/a.ex", "/r/") == "lib/a.ex"
      assert CoverExport.relative_source("/elsewhere/a.ex", "/r") == "/elsewhere/a.ex"
    end
  end

  describe "round trip with Lcov.parse/1 (property)" do
    test "to_lcov |> parse gives the generated lines and never measures branches" do
      Prop.check("cover export round trip", fn ->
        paths = ~w(lib/a.ex lib/b.ex lib/c/d.ex)
        mods = [M1, M2, M3, M4]
        sources = Map.new(mods, fn m -> {m, Enum.random(paths)} end)

        analyses =
          Map.new(mods, fn m ->
            entries =
              for l <- Enum.take_random(0..40, :rand.uniform(15) - 1) do
                {{m, l}, :rand.uniform(5) - 1}
              end

            {m, entries}
          end)

        expected =
          for {m, entries} <- analyses,
              {{_, l}, c} <- entries,
              l > 0,
              reduce: %{} do
            acc -> Map.update(acc, {sources[m], l}, c, &(&1 + c))
          end

        text = CoverExport.to_lcov(analyses, sources)
        assert {:ok, facts} = Lcov.parse(text)
        assert Facts.line_hits(facts) == expected
        assert facts.branch_measurable == false
        assert Facts.branch_hits(facts) == %{}
        # Deterministic: a second render is identical.
        assert text == CoverExport.to_lcov(analyses, sources)
      end)
    end
  end

  describe "real :cover (integration)" do
    @source_path "lib/hypatia_cover_fixture/tiny.ex"

    setup do
      Mix.ensure_application!(:tools)
      started? = match?({:ok, _}, :cover.start())
      :cover.local_only()
      dir = Path.join(System.tmp_dir!(), "hypatia-cover-#{System.unique_integer([:positive])}")
      File.mkdir_p!(dir)

      code = """
      defmodule HypatiaCoverFixture.Tiny do
        def classify(n) do
          if n > 0 do
            :pos
          else
            :nonpos
          end
        end

        def unused(x), do: x + 1
      end
      """

      # :cover needs the abstract code, which the compiler keeps with debug_info.
      old = Code.compiler_options()[:debug_info]
      Code.put_compiler_option(:debug_info, true)

      [{mod, beam}] =
        Code.compile_string(code, Path.join(File.cwd!(), @source_path))

      Code.put_compiler_option(:debug_info, old)
      beam_path = Path.join(dir, "#{mod}.beam")
      File.write!(beam_path, beam)
      {:ok, ^mod} = :cover.compile_beam(String.to_charlist(beam_path))

      on_exit(fn ->
        :cover.reset(mod)
        if started?, do: :cover.stop(), else: :cover.reset()
        :code.purge(mod)
        :code.delete(mod)
        File.rm_rf(dir)
      end)

      %{mod: mod, dir: dir}
    end

    test "export matches the calls made, and parses back", %{mod: mod, dir: dir} do
      assert mod.classify(1) == :pos
      assert mod.classify(2) == :pos

      {:ok, entries} = :cover.analyse(mod, :calls, :line)
      text = CoverExport.to_lcov(%{mod => entries}, %{mod => @source_path})

      assert {:ok, facts} = Lcov.parse(text)
      assert facts.branch_measurable == false
      assert Map.keys(facts.files) == [@source_path]
      lines = facts.files[@source_path].lines

      # classify/1 body (the `if`, line 3) ran twice; the `:pos` arm line is the same
      # executable unit as the `if` for :cover or absent; `unused/1` (line 10) never ran.
      assert lines[3] == 2
      assert lines[10] == 0
      # every reported line is a line of the fixture; nothing from line 0
      assert Enum.all?(Map.keys(lines), &(&1 in 1..11))

      # the tool writes the same file and its sidecar
      out = Path.join(dir, "lcov.info")

      assert :ok =
               CoverTool.write_lcov(out,
                 role: :proven,
                 repo: "o/n",
                 commit: String.duplicate("a", 40)
               )

      assert File.read!(out) =~ "SF:#{@source_path}\n"
      assert {:ok, tool_facts} = Lcov.parse(File.read!(out))
      assert tool_facts.files[@source_path].lines == lines

      meta = (out <> ".meta.json") |> File.read!() |> Jason.decode!()
      assert meta["role"] == "proven"
      assert meta["repo"] == "o/n"
    end
  end
end

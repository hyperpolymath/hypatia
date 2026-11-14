# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.DeedConformanceTest do
  @moduledoc """
  `Hypatia.Deed` against the standards DEED fixtures (deed.abnf v2.0.0).

  Every file under a core `valid/` directory must parse, and every file under a
  core `invalid/` one must be refused, by `parse/2` with its file name, as
  `deed_lint.js FILE` judges them. The fixtures are read from
  `HYPATIA_DEED_FIXTURES`, else the `:hypatia, :deed_standards_fixtures`
  config key, else the vendored copies (`test/fixtures/deed/standards/`, whose
  `UPSTREAM.adoc` names the standards commit).

  Pointed at a standards checkout, the clause-profile directories are read
  too. Their `invalid/` cases fail profile checks this reader does not run, so
  only their `valid/` cases are asserted: a deed valid under a profile is a
  grammatical deed first.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Deed

  @root System.get_env("HYPATIA_DEED_FIXTURES") ||
          Application.compile_env(:hypatia, :deed_standards_fixtures) ||
          Path.expand("fixtures/deed/standards", __DIR__)

  # The directories whose verdict is the grammar's and the gate's core check.
  @core ~w(valid invalid heads bang)

  # {relative path, expected verdict}. A file's verdict is its nearest
  # valid/ or invalid/ directory; files under neither (registries, receipts)
  # are not fixtures of a verdict.
  @cases (for path <- Path.wildcard(Path.join(@root, "**/*.deed")),
              rel = Path.relative_to(path, @root),
              parts = Path.split(rel),
              verdict =
                parts |> Enum.reverse() |> Enum.find(&(&1 in ["valid", "invalid"])),
              verdict != nil,
              hd(parts) in @core or verdict == "valid" do
            {rel, String.to_atom(verdict)}
          end)
         |> Enum.sort()

  test "the fixture set is present and holds both verdicts" do
    assert File.dir?(@root), "no DEED fixtures at #{@root}"
    core = Enum.filter(@cases, fn {rel, _} -> hd(Path.split(rel)) in @core end)
    # deed.abnf v2.0.0's core set: 18 valid and 39 invalid at standards b2aaeb0.
    assert Enum.count(core, &match?({_, :valid}, &1)) >= 18
    assert Enum.count(core, &match?({_, :invalid}, &1)) >= 39
  end

  for {rel, verdict} <- @cases do
    @rel rel
    path = Path.join(@root, rel)
    @external_resource path

    case verdict do
      :valid ->
        test "accepts #{rel}" do
          text = File.read!(Path.join(@root, @rel))
          assert {:ok, {_head, _items}} = Deed.parse(text, Path.basename(@rel))
        end

      :invalid ->
        test "refuses #{rel}" do
          text = File.read!(Path.join(@root, @rel))
          assert {:error, reason} = Deed.parse(text, Path.basename(@rel))
          assert is_binary(reason)
        end
    end
  end

  describe "the v1.4.0 to v2.0.0 deltas, one case each" do
    @hdr ";; SPDX-License-Identifier: MPL-2.0\n"

    test "filename dispatch picks the doc-head, case-sensitively" do
      assert Deed.dispatch("estate_chora.deed") == {:ok, "estate-deed"}
      assert Deed.dispatch("ATLAS.deed") == {:ok, "estate-atlas-deed"}
      assert Deed.dispatch("x/0-AI-MANIFEST.deed") == {:ok, "repo-deed"}
      assert Deed.dispatch("Mustfile.deed") == {:ok, "contractile-deed"}
      assert Deed.dispatch("CLADE.deed") == {:ok, "descriptile-deed"}
      assert Deed.dispatch("INTENT.deed") == {:ok, "descriptile-deed"}
      assert Deed.dispatch("my.project_praxis.deed") == {:ok, "praxis-deed"}
      assert Deed.dispatch("my.project_chora.deed") == {:ok, "repo-deed"}

      for name <- ~w(mustfile.deed MUSTFILE.deed Meta.deed State.deed ANCHOR.deed anchor.deed),
          do: assert({:error, _} = Deed.dispatch(name), name)
    end

    test "a bang stands only after a top-level rule's :id in a contractile-deed" do
      rule = fn head, body -> @hdr <> "(#{head} :schema-version \"1\"\n  #{body})\n" end

      assert {:ok, {"contractile-deed", [_, {:clause, "rule", fields}]}} =
               Deed.parse(
                 rule.("contractile-deed", ~s|(rule :id "r1" ! :text "x")|),
                 "Mustfile.deed"
               )

      assert {:field, "id", {:string, "r1"}, :bang} in fields
      assert {:field, "text", {:string, "x"}} in fields

      # A bang may close the clause, or be followed by a comment.
      assert {:ok, _} = Deed.parse(rule.("contractile-deed", ~s|(rule :id "r1" !)|))
      assert {:ok, _} = Deed.parse(rule.("contractile-deed", "(rule :id \"r1\" ! ; b\n)"))

      for {head, body} <- [
            {"contractile-deed", ~s|(rule :text "x" !)|},
            {"contractile-deed", ~s|(edict (rule :id "r1" !))|},
            {"contractile-deed", ~s|(other :id "r1" !)|},
            {"repo-deed", ~s|(rule :id "r1" !)|}
          ] do
        assert {:error, "'!' is allowed only" <> _} = Deed.parse(rule.(head, body)), body
      end

      # "!" run on to a value or to text is not the bang token.
      assert {:error, _} = Deed.parse(rule.("contractile-deed", ~s|(rule :id "r1"!)|))
      assert {:error, _} = Deed.parse(rule.("contractile-deed", ~s|(rule :id "r1" !x)|))
      # Inside a symbol, "!" is a symbol character.
      assert {:ok, _} = Deed.parse(rule.("repo-deed", ~s|(rule :id r1!)|))
    end

    test "a line is at most 998 octets, the last line included" do
      ok = @hdr <> "(repo-deed :schema-version \"1\" :a \"#{String.duplicate("é", 470)}\")"
      assert {:ok, _} = Deed.parse(ok)
      long = @hdr <> "(repo-deed :schema-version \"1\" :a \"#{String.duplicate("é", 490)}\")"
      assert {:error, "line 2 is " <> _} = Deed.parse(long)
    end

    test "a #uuid is version 5 or 8 with variant 10" do
      v = fn id -> Deed.parse(@hdr <> ~s|(repo-deed :schema-version "1" :x #uuid"#{id}")|) end
      assert {:ok, _} = v.("b32d6be4-73ba-873e-9d61-e57f43c75cd1")
      assert {:ok, _} = v.("b32d6be4-73ba-573e-bd61-e57f43c75cd1")
      assert {:error, _} = v.("b32d6be4-73ba-473e-9d61-e57f43c75cd1")
      assert {:error, _} = v.("b32d6be4-73ba-873e-cd61-e57f43c75cd1")
    end

    test "an identity field carries no #u5 and no version-5 #uuid, wherever it is written" do
      p = fn body -> Deed.parse(@hdr <> ~s|(repo-deed :schema-version "1" #{body})|) end
      assert {:ok, _} = p.(~s|:repo-uuid #u8"chora-registry:github.com/o/x"|)
      assert {:ok, _} = p.(~s|:repo-uuid #uuid"b32d6be4-73ba-873e-9d61-e57f43c75cd1"|)
      # #u5 is legal where no identity is claimed.
      assert {:ok, _} = p.(~s|:legacy #u5"github.com/o/x"|)
      assert {:error, _} = p.(~s|:repo-uuid #u5"github.com/o/x"|)
      assert {:error, _} = p.(~s|(parent :of-chora #u5"github.com/o/x")|)
      assert {:error, _} = p.(~s|:repo-uuid #uuid"b32d6be4-73ba-573e-9d61-e57f43c75cd1"|)
      # Read lexically, as check-uuid-v8.sh reads it: a comment between the
      # field and its value hides nothing, and a comment is read too.
      assert {:error, _} = p.(~s|:repo-uuid ; old\n #u5"github.com/o/x"|)
      assert {:error, _} = p.(~s|:a "x" ; :repo-uuid #u5"github.com/o/x"\n|)
    end
  end
end

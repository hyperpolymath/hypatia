# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.DeedTest do
  use ExUnit.Case, async: true

  alias Hypatia.Deed

  @fixtures Path.expand("fixtures/deed_egress", __DIR__)
  @tmp_dir System.tmp_dir!()

  defp repo_with_deed(name, text) do
    repo = Path.join(@tmp_dir, "deed_test_#{System.unique_integer([:positive])}")
    # unique_integer restarts with each VM, so clear what an earlier run left.
    File.rm_rf!(repo)
    File.mkdir_p!(repo)
    File.write!(Path.join(repo, name), text)
    repo
  end

  defp repo_with_fixture(sub, name) do
    repo_with_deed(name, File.read!(Path.join([@fixtures, sub, name])))
  end

  describe "parse/1" do
    test "reads fields, clauses and every value form" do
      text = """
      ;; SPDX-License-Identifier: CC-BY-SA-4.0
      ; a comment
      (repo-deed :schema-version "1.0.0" :canonical-name "x"
        :repo-uuid #u5"github.com/o/x" :beholding-chora #u8"estate:chora"
        (status :present #t :ended #f :count -7 :tags ('a "b" (c)) :phase active
          :note "say \\"hi\\"\\n"))
      """

      assert {:ok, {"repo-deed", items}} = Deed.parse(text)
      assert {:field, "repo-uuid", {:uuid5, "github.com/o/x"}} in items
      assert {:field, "beholding-chora", {:uuid8, "estate:chora"}} in items
      assert {:clause, "status", status} = List.last(items)

      assert status == [
               {:field, "present", {:boolean, true}},
               {:field, "ended", {:boolean, false}},
               {:field, "count", {:integer, -7}},
               {:field, "tags",
                {:list, [{:quoted, {:symbol, "a"}}, {:string, "b"}, {:list, [{:symbol, "c"}]}]}},
               {:field, "phase", {:symbol, "active"}},
               {:field, "note", {:string, "say \"hi\"\n"}}
             ]
    end

    test "rejects what it cannot read" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n"
      assert {:error, _} = Deed.parse("(repo-deed :schema-version \"1.0.0\")")
      assert {:error, _} = Deed.parse(hdr <> "(repo-deed :schema-version \"1.0.0\"")
      assert {:error, _} = Deed.parse(hdr <> "(repo-deed :x \"\\r\")")
      assert {:error, _} = Deed.parse(hdr <> "(widget-deed :schema-version \"1.0.0\")")
      assert {:error, _} = Deed.parse(hdr <> "(repo-deed :x \"1\") (repo-deed :x \"2\")")
    end

    test "enforces :schema-version exactly once, as a string" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n"
      assert {:error, msg} = Deed.parse(hdr <> "(repo-deed :canonical-name \"x\")")
      assert msg =~ "found 0"
      assert {:error, _} = Deed.parse(hdr <> "(repo-deed :schema-version 1)")

      assert {:error, msg} =
               Deed.parse(hdr <> "(repo-deed :schema-version \"1\" :schema-version \"2\")")

      assert msg =~ "found 2"
    end

    test "requires the separators the grammar requires" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n"
      ok = ~s|(repo-deed :schema-version "1.0.0" (egress (job :secrets ("A" "B"))))|
      assert {:ok, _} = Deed.parse(hdr <> ok)
      # Codex's example: no separators anywhere, and no :schema-version.
      assert {:error, _} = Deed.parse(hdr <> ~s|(repo-deed(egress(job :secrets("X"))))|)
      # Each missing separator on its own.
      assert {:error, _} = Deed.parse(hdr <> ~s|(repo-deed:schema-version "1.0.0")|)
      assert {:error, _} = Deed.parse(hdr <> ~s|(repo-deed :schema-version"1.0.0")|)
      assert {:error, _} = Deed.parse(hdr <> ~s|(repo-deed :schema-version "1.0.0"(egress))|)
      assert {:error, _} = Deed.parse(hdr <> ~s|(repo-deed :schema-version "1.0.0" :a ("A""B"))|)
      assert {:error, _} = Deed.parse(hdr <> ~s|(repo-deed :schema-version "1.0.0" :a #tx)|)
    end

    # Each case below is a class the differential run against deed.abnf
    # (apg-js) found the earlier reader accepting.
    test "strings hold no raw control characters" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version \"1.0.0\" "
      assert {:ok, _} = Deed.parse(hdr <> ~s|:a "é © \u0085 \\n \\t")|)

      for c <- ["\n", "\r", "\t", <<1>>, <<0x7F>>] do
        assert {:error, msg} = Deed.parse(hdr <> ~s|:a "x#{c}y")|)
        assert msg =~ "raw control character"
      end
    end

    test "separators are SP, CRLF, LF and comments that end in a line-end" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n"

      assert {:ok, _} =
               Deed.parse(hdr <> "(repo-deed\r\n:schema-version \"1\" ; ok\r\n)\n; end\n")

      assert {:error, _} = Deed.parse(hdr <> "(repo-deed\r:schema-version \"1\")")
      assert {:error, _} = Deed.parse(hdr <> "(repo-deed\t:schema-version \"1\")")
      assert {:error, _} = Deed.parse(hdr <> "(repo-deed :schema-version \"1\" ;\tx\n)")
      # A comment must end in a line-end, even the last one.
      assert {:error, _} = Deed.parse(hdr <> "(repo-deed :schema-version \"1\")\n; end")
    end

    test "SPDX header lines are text-chars with a non-blank payload" do
      form = "(repo-deed :schema-version \"1\")"
      assert {:ok, _} = Deed.parse(";; SPDX-FileCopyrightText: © 2026 x\r\n" <> form)
      assert {:error, _} = Deed.parse(";; SPDX-   \n" <> form)
      assert {:error, _} = Deed.parse(";; SPDX-a\tb\n" <> form)
      assert {:error, _} = Deed.parse(";; SPDX-License-Identifier: MPL-2.0" <> form)
    end

    test "a list may open with a separator (deed.abnf v1.2.1)" do
      # As rsr-template-repo_chora.deed writes them: `:purpose (` then a newline.
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version \"1\" "
      assert {:ok, _} = Deed.parse(hdr <> ~s|:a () :b ( ) :c ("x" ) :d ("x"\n"y"))|)
      assert {:ok, _} = Deed.parse(hdr <> ~s|:secrets (\n  "DEPLOY_KEY"\n  ; c\n  "X"))|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:secrets (\t"DEPLOY_KEY"))|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:secrets ("A""B"))|)
    end

    test "a #u8 body is domain:name with a printable ASCII domain" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version \"1\" "
      assert {:ok, _} = Deed.parse(hdr <> ~s|:id #u8"gv-clade-index:github.com/x/y:z")|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:id #u8"no-colon")|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:id #u8":empty-domain")|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:id #u8"dé:x")|)
    end

    test "a field's value is never a boolean spelled as a word, as the gate rules" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version \"1\" "
      assert {:ok, _} = Deed.parse(hdr <> ":a #t :b truthy :c (true no))")

      for word <- ~w(true false yes no) do
        assert {:error, msg} = Deed.parse(hdr <> ":enabled #{word})")
        assert msg =~ "write #t or #f"
      end
    end

    test "quotes only a symbol or a list, with nothing between" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version \"1.0.0\" "
      assert {:ok, _} = Deed.parse(hdr <> ~s|:a 'sym :b '("x" y))|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:note '"text")|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:n '7)|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:t '#t)|)
      assert {:error, _} = Deed.parse(hdr <> ~s|:a ' sym)|)
    end

    test "reads the estate template's real deed" do
      # Copied from standards 1-formats/deed/tools/fixtures/valid/, where deed_lint passes it.
      text = File.read!(Path.join(@fixtures, "rsr-template-repo_chora.deed"))
      assert {:ok, {"repo-deed", items}} = Deed.parse(text)
      assert {:field, "canonical-name", {:string, "rsr-template-repo"}} in items
    end
  end

  describe "egress_declarations/1 against the standards fixtures" do
    for name <- File.ls!(Path.expand("fixtures/deed_egress/valid", __DIR__)) do
      @name name
      test "accepts valid/#{name}" do
        repo = repo_with_fixture("valid", @name)
        assert {:ok, @name, jobs} = Deed.egress_declarations(repo)
        assert is_list(jobs)
      end
    end

    for name <- File.ls!(Path.expand("fixtures/deed_egress/invalid", __DIR__)) do
      @name name
      test "rejects invalid/#{name}" do
        repo = repo_with_fixture("invalid", @name)
        assert {:error, @name, reason} = Deed.egress_declarations(repo)
        assert is_binary(reason)
      end
    end

    test "takes :policy at most once, before or after the jobs" do
      hdr = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version \"1.0.0\" "

      job =
        ~s|(job :workflow ".github/workflows/a.yml" :job "a" | <>
          ~s|:secrets ("A") :endpoints () :enforcement none)|

      repo = repo_with_deed("x_chora.deed", hdr <> ~s|(egress #{job} :policy "a"))|)
      assert {:ok, _, [_]} = Deed.egress_declarations(repo)

      repo = repo_with_deed("x_chora.deed", hdr <> ~s|(egress :policy "a" :policy "b" #{job}))|)
      assert {:error, _, "(egress …) repeats :policy"} = Deed.egress_declarations(repo)
    end

    test "reads the template's live deed and its three declared jobs" do
      repo = Path.join(@fixtures, "template-main")

      assert {:ok, "rsr-template-repo_chora.deed", jobs} = Deed.egress_declarations(repo)

      assert Enum.map(jobs, & &1.job) == ["notify", "sonarqube", "trigger-boj"]
    end

    test "returns the declared jobs with their secrets" do
      repo = repo_with_fixture("valid", "template-pair_chora.deed")

      assert {:ok, _, [notify, sonar]} = Deed.egress_declarations(repo)

      assert notify == %{
               workflow: ".github/workflows/push-email-notify.yml",
               job: "notify",
               secrets: MapSet.new(~w(SMTP_HOST SMTP_PORT SMTP_USER SMTP_PASS))
             }

      assert sonar.secrets == MapSet.new(["SONAR_TOKEN"])
    end
  end

  describe "repo_deed_path/1" do
    test "no deed, one deed, two deeds" do
      repo = Path.join(@tmp_dir, "deed_none_#{System.unique_integer([:positive])}")
      File.rm_rf!(repo)
      File.mkdir_p!(repo)
      assert Deed.repo_deed_path(repo) == :none
      assert Deed.egress_declarations(repo) == :none

      File.write!(Path.join(repo, "estate_chora.deed"), "")
      assert Deed.repo_deed_path(repo) == :none

      File.write!(Path.join(repo, "a_chora.deed"), "")
      assert {:ok, _} = Deed.repo_deed_path(repo)

      File.write!(Path.join(repo, "b_chora.deed"), "")
      assert {:error, _} = Deed.repo_deed_path(repo)
    end

    test "the stem is deed.abnf's: letters, digits, '-', '.', '_', at least one" do
      for {name, ok?} <- [
            {"my.project-x_chora.deed", true},
            {"bad name_chora.deed", false},
            {"_chora.deed", false},
            {"é_chora.deed", false}
          ] do
        repo = Path.join(@tmp_dir, "deed_stem_#{System.unique_integer([:positive])}")
        File.rm_rf!(repo)
        File.mkdir_p!(repo)
        File.write!(Path.join(repo, name), "")

        if ok?,
          do: assert({:ok, _} = Deed.repo_deed_path(repo), name),
          else:
            assert({:error, "illegal deed filename stem" <> _} = Deed.repo_deed_path(repo), name)
      end
    end

    test "a <stem>_chora.deed must be a repo-deed" do
      repo =
        repo_with_deed(
          "x_chora.deed",
          ";; SPDX-License-Identifier: CC-BY-SA-4.0\n(praxis-deed :schema-version \"1\")\n"
        )

      assert {:error, _, "doc-head/filename mismatch" <> _} = Deed.egress_declarations(repo)
    end
  end
end

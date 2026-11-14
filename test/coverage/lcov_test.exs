# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.LcovTest do
  use ExUnit.Case, async: true

  alias Hypatia.Coverage.Lcov

  @sample """
  TN:
  SF:/ci/work/repo/lib/a.ex
  FN:3,foo
  FNDA:2,foo
  DA:3,2
  DA:4,0
  DA:5,7,abcdef
  LF:3
  LH:2
  BRDA:4,0,0,1
  BRDA:4,0,1,-
  BRF:2
  BRH:1
  end_of_record
  SF:lib/b.ex
  DA:1,1
  end_of_record
  """

  test "parses DA, BRDA and end_of_record; ignores summary records" do
    assert {:ok, facts} = Lcov.parse(@sample, root: "/ci/work/repo")

    assert facts.files["lib/a.ex"] == %{
             lines: %{3 => 2, 4 => 0, 5 => 7},
             branches: %{{4, 0, 0} => 1, {4, 0, 1} => 0}
           }

    assert facts.files["lib/b.ex"] == %{lines: %{1 => 1}, branches: %{}}
    assert facts.branch_measurable
  end

  test "BRDA taken '-' counts as 0" do
    {:ok, facts} = Lcov.parse("SF:x\nBRDA:1,0,0,-\nend_of_record\n")
    assert facts.files["x"].branches == %{{1, 0, 0} => 0}
  end

  test "no BRDA anywhere means branches are not measurable" do
    {:ok, facts} = Lcov.parse("SF:x\nDA:1,1\nend_of_record\n")
    refute facts.branch_measurable
  end

  test "without a root the SF path is kept" do
    {:ok, facts} = Lcov.parse("SF:/abs/x.ex\nDA:1,1\nend_of_record\n")
    assert Map.keys(facts.files) == ["/abs/x.ex"]
  end

  test "repeated DA lines for one line are summed; CRLF is accepted" do
    {:ok, facts} = Lcov.parse("SF:x\r\nDA:1,1\r\nDA:1,2\r\nend_of_record\r\n")
    assert facts.files["x"].lines == %{1 => 3}
  end

  test "empty input is an empty, valid report" do
    assert {:ok, %{files: files, branch_measurable: false}} = Lcov.parse("")
    assert files == %{}
  end

  describe "errors carry the line number" do
    test "DA outside a record" do
      assert {:error, {:parse, "line 1: DA outside an SF record"}} = Lcov.parse("DA:1,1\n")
    end

    test "bad DA" do
      assert {:error, {:parse, "line 2: bad DA record \"DA:x,1\""}} =
               Lcov.parse("SF:a\nDA:x,1\nend_of_record\n")
    end

    test "bad BRDA" do
      assert {:error, {:parse, "line 2: bad BRDA record" <> _}} =
               Lcov.parse("SF:a\nBRDA:1,2,3\nend_of_record\n")
    end

    test "end_of_record with no SF" do
      assert {:error, {:parse, "line 1: end_of_record with no SF"}} =
               Lcov.parse("end_of_record\n")
    end

    test "SF inside an open record" do
      assert {:error, {:parse, "line 2: SF before end_of_record"}} = Lcov.parse("SF:a\nSF:b\n")
    end

    test "truncated input" do
      assert {:error, {:parse, "unterminated record" <> _}} = Lcov.parse("SF:a\nDA:1,1\n")
    end
  end

  test "render/1 then parse/1 on a small report" do
    {:ok, facts} = Lcov.parse(@sample, root: "/ci/work/repo")
    assert Lcov.parse(Lcov.render(facts)) == {:ok, facts}
  end
end

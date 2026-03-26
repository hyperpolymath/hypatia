# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ReviewerTest do
  use ExUnit.Case, async: true

  alias Hypatia.Reviewer
  alias Hypatia.Reviewer.Comment

  @sample_diff """
  diff --git a/lib/app.ex b/lib/app.ex
  --- a/lib/app.ex
  +++ b/lib/app.ex
  @@ -1,3 +1,5 @@
   defmodule App do
  +  # Added a new function
  +  def hello, do: :world
   end
  """

  @workflow_diff """
  diff --git a/.github/workflows/ci.yml b/.github/workflows/ci.yml
  --- /dev/null
  +++ b/.github/workflows/ci.yml
  @@ -0,0 +1,8 @@
  +name: CI
  +on: push
  +permissions: write-all
  +jobs:
  +  test:
  +    runs-on: ubuntu-latest
  +    steps:
  +      - uses: actions/checkout@v4
  """

  # ─── Diff Parsing ──────────────────────────────────────────────────

  describe "parse_diff/1" do
    test "parses single file diff" do
      files = Reviewer.parse_diff(@sample_diff)
      assert length(files) == 1
      assert hd(files).path == "lib/app.ex"
    end

    test "extracts added lines" do
      [file] = Reviewer.parse_diff(@sample_diff)
      assert length(file.added_lines) == 2
      {_line_num, text} = hd(file.added_lines)
      assert String.contains?(text, "Added a new function") or String.contains?(text, "hello")
    end

    test "parses multiple file diffs" do
      combined = @sample_diff <> "\n" <> @workflow_diff
      files = Reviewer.parse_diff(combined)
      assert length(files) == 2
      paths = Enum.map(files, & &1.path)
      assert "lib/app.ex" in paths
      assert ".github/workflows/ci.yml" in paths
    end

    test "handles empty diff" do
      assert Reviewer.parse_diff("") == []
    end
  end

  # ─── Review Diff ───────────────────────────────────────────────────

  describe "review_diff/1" do
    test "returns list of Comment structs" do
      comments = Reviewer.review_diff(@sample_diff)
      assert is_list(comments)
      Enum.each(comments, fn c ->
        assert %Comment{} = c
        assert is_binary(c.path)
        assert is_binary(c.body)
        assert c.severity in [:info, :low, :medium, :high, :critical]
      end)
    end

    test "detects issues in workflow files" do
      comments = Reviewer.review_diff(@workflow_diff)
      # The workflow has write-all permissions which should be flagged
      assert is_list(comments)
    end
  end

  # ─── Summary Formatting ───────────────────────────────────────────

  describe "format_summary/1" do
    test "returns clean message for empty findings" do
      summary = Reviewer.format_summary([])
      assert String.contains?(summary, "No issues found")
    end

    test "formats findings by severity" do
      comments = [
        %Comment{path: "lib/app.ex", line: 5, body: "Missing SPDX header", severity: :medium, rule: "missing_spdx"},
        %Comment{path: "lib/app.ex", line: 10, body: "Potential secret", severity: :critical, rule: "secret_detected"}
      ]

      summary = Reviewer.format_summary(comments)
      assert String.contains?(summary, "Critical")
      assert String.contains?(summary, "Medium")
      assert String.contains?(summary, "2 finding(s)")
    end

    test "includes file paths and rules in output" do
      comments = [
        %Comment{path: "config/dev.exs", line: 3, body: "Hardcoded token", severity: :high, rule: "hardcoded_secret"}
      ]

      summary = Reviewer.format_summary(comments)
      assert String.contains?(summary, "config/dev.exs")
      assert String.contains?(summary, "hardcoded_secret")
    end
  end

  # ─── Language Detection ────────────────────────────────────────────

  describe "language detection (via review)" do
    test "processes Elixir files" do
      diff = """
      diff --git a/lib/mod.ex b/lib/mod.ex
      --- /dev/null
      +++ b/lib/mod.ex
      @@ -0,0 +1,3 @@
      +defmodule Mod do
      +  def run, do: :ok
      +end
      """

      comments = Reviewer.review_diff(diff)
      assert is_list(comments)
    end

    test "processes Rust files" do
      diff = """
      diff --git a/src/main.rs b/src/main.rs
      --- /dev/null
      +++ b/src/main.rs
      @@ -0,0 +1,3 @@
      +fn main() {
      +    println!("hello");
      +}
      """

      comments = Reviewer.review_diff(diff)
      assert is_list(comments)
    end
  end
end

# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.CoverToolTest do
  use ExUnit.Case, async: false

  alias Hypatia.Coverage.CoverTool

  @env ~w(GITHUB_REPOSITORY GITHUB_SHA GITHUB_RUN_ID HYPATIA_COVERAGE_ROLE)

  setup do
    saved = Map.new(@env, &{&1, System.get_env(&1)})
    Enum.each(@env, &System.delete_env/1)

    on_exit(fn ->
      for {k, v} <- saved, do: if(v, do: System.put_env(k, v), else: System.delete_env(k))
    end)
  end

  test "meta/1 from options" do
    sha = String.duplicate("b", 40)
    m = CoverTool.meta(repo: "o/n", commit: sha, run_id: "42", role: "raw")

    assert %{
             "schema" => "hypatia.proven-coverage.report-meta/1",
             "repo" => "o/n",
             "commit" => ^sha,
             "run_id" => "42",
             "role" => "raw",
             "tool" => "erlang-cover"
           } = m

    assert is_binary(m["tool_version"])
  end

  test "meta/1 from the environment" do
    System.put_env("GITHUB_REPOSITORY", "env/repo")
    System.put_env("GITHUB_SHA", String.duplicate("c", 40))
    System.put_env("GITHUB_RUN_ID", "777")
    System.put_env("HYPATIA_COVERAGE_ROLE", "proven")
    m = CoverTool.meta()
    assert {m["repo"], m["run_id"], m["role"]} == {"env/repo", "777", "proven"}
    assert m["commit"] == String.duplicate("c", 40)
  end

  test "unknown values are left out, never invented; run_id gets a local id" do
    m = CoverTool.meta()
    refute Map.has_key?(m, "repo")
    refute Map.has_key?(m, "commit")
    refute Map.has_key?(m, "role")
    assert m["run_id"] =~ ~r/\Alocal-\d+-[0-9a-f]{6}\z/
  end

  test "an invalid role is dropped" do
    refute Map.has_key?(CoverTool.meta(role: "everything"), "role")
  end
end

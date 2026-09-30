# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.CicdRepoRequirementsTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CicdRules

  defp repo_with(files) do
    repo = Path.join(System.tmp_dir!(), "req_test_#{System.unique_integer([:positive])}")

    for f <- files do
      path = Path.join(repo, f)
      File.mkdir_p!(Path.dirname(path))
      File.write!(path, "x\n")
    end

    File.mkdir_p!(repo)
    repo
  end

  defp missing(repo) do
    %{visibility: "public", has_deps: false, files: [], repo_path: repo}
    |> CicdRules.check_repo_requirements()
    |> Enum.map(& &1.missing)
  end

  describe "security policy in any markup (absolute-zero SECURITY.adoc)" do
    test "SECURITY.adoc at the root satisfies the requirement" do
      repo = repo_with(["SECURITY.adoc"])
      refute "SECURITY.md" in missing(repo)
      File.rm_rf!(repo)
    end

    test ".github/SECURITY.rst satisfies it too" do
      repo = repo_with([".github/SECURITY.rst"])
      refute "SECURITY.md" in missing(repo)
      File.rm_rf!(repo)
    end

    test "no policy document at all is still reported" do
      repo = repo_with(["README.adoc"])
      assert "SECURITY.md" in missing(repo)
      File.rm_rf!(repo)
    end

    test "a non-document requirement is not widened to other extensions" do
      repo = repo_with([".github/workflows/scorecard.adoc"])
      assert ".github/workflows/scorecard.yml" in missing(repo)
      File.rm_rf!(repo)
    end
  end
end

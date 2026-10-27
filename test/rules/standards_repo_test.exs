# SPDX-License-Identifier: MPL-2.0
# Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.Rules.StandardsRepoTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.StandardsRepo

  test "the current name comes first, the former name stays known" do
    assert StandardsRepo.current() == "metadatastician/standards"
    assert StandardsRepo.names() == ["metadatastician/standards", "hyperpolymath/standards"]
  end

  test "name?/1 accepts either name in any case, and nothing else" do
    assert StandardsRepo.name?("metadatastician/standards")
    assert StandardsRepo.name?("HyperPolymath/Standards")
    refute StandardsRepo.name?("hyperpolymath/standards-mirror")
    refute StandardsRepo.name?("hyperpolymath/hypatia")
    refute StandardsRepo.name?(nil)
  end

  test "path_regex_source/1 matches a standards path under either name" do
    re =
      Regex.compile!(
        "^" <>
          StandardsRepo.path_regex_source(
            "hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml"
          ) <> "$"
      )

    assert Regex.match?(re, "hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml")
    assert Regex.match?(re, "metadatastician/standards/.github/workflows/rust-ci-reusable.yml")
    assert Regex.match?(re, "Metadatastician/Standards/.github/workflows/rust-ci-reusable.yml")
    refute Regex.match?(re, "metadatastician/standards/.github/workflows/rust-ciXreusable.yml")
    refute Regex.match?(re, "someone/standards/.github/workflows/rust-ci-reusable.yml")
  end

  test "path_regex_source/1 escapes any other path as it stands" do
    assert StandardsRepo.path_regex_source(
             "hyperpolymath/cicd-suite/.github/workflows/rust-ci.yml"
           ) ==
             Regex.escape("hyperpolymath/cicd-suite/.github/workflows/rust-ci.yml")
  end
end

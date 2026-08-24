# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.ImplementationInsideCanonTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ImplementationInsideCanon
  alias Hypatia.Rules.RuleLoader

  @rule """
  @rule(version="1.0"):
  id: HYP-S009
  severity: medium
  @end
  @parameters:
  registry: .machine_readable/REGISTRY.a2ml
  implementation_basenames:
    - Cargo.toml
    - CNAME
  conditional_basenames:
    - guix.scm
  path_exemptions:
    - "**/examples/**"
    - "**/templates/**"
    - "**/tests/fixtures/**"
  @end
  @router:
  default_strategy: review
  @end
  @action:
  emit_signal: compliance.finding.new
  recipe: review-canon-carve-out
  @end
  """

  @registry """
  [registry]
  version = "1.0.0"

  [[spec]]
  id = "local-spec"
  home = "local/"

  [[spec]]
  id = "external-spec"
  home = "external/"
  kind = "external"
  """

  test "detects tracked product manifests only inside local canon homes" do
    {:ok, rule} = RuleLoader.parse(@rule)

    files = [
      "local/Cargo.toml",
      "local/CNAME",
      "local/guix.scm",
      "local/examples/Cargo.toml",
      "local/templates/Cargo.toml",
      "external/Cargo.toml",
      "outside/Cargo.toml"
    ]

    reader = fn
      "local/guix.scm" -> {:ok, "(use-modules (guix packages))\n"}
      _ -> {:error, :enoent}
    end

    assert {:ok, %{findings: findings}} =
             ImplementationInsideCanon.evaluate(rule, @registry, files, read_file: reader)

    assert Enum.map(findings, & &1.file) ==
             ["local/CNAME", "local/Cargo.toml", "local/guix.scm"]

    assert Enum.all?(findings, &(&1.type == "HYP-S009"))
    assert Enum.all?(findings, &(&1.action == "review"))
    assert Enum.all?(findings, &(&1.spec_id == "local-spec"))
  end

  test "accepts a comment-only guix stub and fixture paths" do
    {:ok, rule} = RuleLoader.parse(@rule)

    files = ["local/guix.scm", "local/tests/fixtures/CNAME"]
    reader = fn _ -> {:ok, "; documented non-building placeholder\n\n"} end

    assert {:ok, %{findings: []}} =
             ImplementationInsideCanon.evaluate(rule, @registry, files, read_file: reader)
  end

  test "fails closed on a traversing local home" do
    {:ok, rule} = RuleLoader.parse(@rule)
    registry = "[[spec]]\nid = \"bad\"\nhome = \"../outside/\"\n"

    assert {:error, {:invalid_local_home, "bad", "../outside/"}} =
             ImplementationInsideCanon.evaluate(rule, registry, [])
  end

  test "is dormant when the canonical rule is absent" do
    tmp = Path.join(System.tmp_dir!(), "hypatia-no-s009-#{System.unique_integer([:positive])}")
    File.mkdir_p!(tmp)
    on_exit(fn -> File.rm_rf!(tmp) end)

    assert {:ok, %{findings: []}} = ImplementationInsideCanon.scan(tmp)
  end
end

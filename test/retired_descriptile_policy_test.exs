# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.RetiredDescriptilePolicyTest do
  use ExUnit.Case, async: true
  alias Hypatia.Rules.StructuralDrift

  test "detects the contradictory check and routes reference repair" do
    repo =
      Path.join(System.tmp_dir!(), "descriptile-policy-#{System.unique_integer([:positive])}")

    File.mkdir_p!(Path.join(repo, ".github/workflows"))
    on_exit(fn -> File.rm_rf!(repo) end)
    file = Path.join(repo, ".github/workflows/compliance.yml")
    File.write!(file, "run: |\n  if [ ! -f .machine_readable/STATE.a2ml ]; then exit 1; fi\n")
    [finding] = StructuralDrift.sd024_retired_descriptile_policy(repo)
    assert finding.rule == "SD024"
    assert finding.line == 2
    assert finding.action == :update_reference
    assert finding.fix_script == "fix-retired-descriptile-policy.sh"

    normalized =
      Hypatia.CLI.collect_findings(repo, [:structural_drift])
      |> Enum.find(&(&1.type == "SD024"))

    assert normalized.category == "RetiredDescriptilePolicy"
    assert normalized.recipe_id == "recipe-retired-descriptile-policy"
    assert normalized.fix_script == "fix-retired-descriptile-policy.sh"
    assert normalized.line == 2

    File.write!(
      file,
      "run: |\n  if [ ! -f .machine_readable/descriptiles/STATE.a2ml ]; then exit 1; fi\n"
    )

    assert StructuralDrift.sd024_retired_descriptile_policy(repo) == []
    File.write!(file, "# if [ ! -f .machine_readable/6a2/STATE.a2ml ]; then exit 1; fi\n")
    assert StructuralDrift.sd024_retired_descriptile_policy(repo) == []
  end
end

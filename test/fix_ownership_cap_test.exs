# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.FixOwnershipCapTest do
  # Owner ruling R1 and the owner answers on hypatia#1080 (2026-11-14):
  # groups A-C belong to cicd-squabbler (report + referral only), group D is
  # flag-only (licence/SPDX), and groups E and F keep their confidence tier.
  # The cap is by recipe id, so a confidence that Bayesian updating raises
  # back to 0.99 still cannot reach auto_execute.
  use ExUnit.Case, async: true

  alias Hypatia.{DispatchManifest, TriangleRouter}

  @recipes_dir Path.expand("../data/verisim/recipes", __DIR__)

  @capped [
    {"A pins and locks", :squabbler_owned,
     ~w(recipe-pin-github-actions recipe-pin-dependencies recipe-scorecard-pinned-deps)},
    {"B workflow content", :squabbler_owned,
     ~w(recipe-scorecard-dangerous-workflow recipe-scorecard-sast recipe-add-dependabot recipe-scorecard-dependency-update)},
    {"C safe code fixes", :squabbler_owned,
     ~w(recipe-fix-http-to-https recipe-shell-quote-vars recipe-remove-believe-me)},
    {"D licence and SPDX", :flag_only,
     ~w(recipe-add-license-file recipe-fix-spdx-license recipe-fix-pmpl-drift recipe-fix-spdx-double-suffix recipe-scorecard-license)}
  ]

  @uncapped [
    {"E repository settings",
     ~w(recipe-scorecard-branch-protection recipe-scorecard-code-review recipe-scorecard-webhooks recipe-scorecard-signed-releases)},
    {"F seeds and Hypatia operations",
     ~w(recipe-add-security-policy recipe-scorecard-security-policy recipe-scorecard-binary-artifacts recipe-scorecard-vulnerabilities recipe-close-obsolete-pr recipe-budget-resume-sweep recipe-retired-descriptile-policy)}
  ]

  defp recipe(id), do: Path.join(@recipes_dir, id <> ".json") |> File.read!() |> Jason.decode!()

  @pattern %{
    "id" => "PA-test",
    "category" => "Test",
    "description" => "test finding",
    "routed_repo" => "test-repo",
    "file" => "x.sh"
  }

  for {group, cap, ids} <- @capped do
    test "group #{group}: every recipe is #{cap} and report_only, even at 0.99" do
      for id <- unquote(ids) do
        r = recipe(id)
        assert TriangleRouter.ownership_cap(r) == unquote(cap), id
        assert TriangleRouter.recipe_strategy(r) == :report_only, id
        assert TriangleRouter.recipe_strategy(Map.put(r, "confidence", 0.99)) == :report_only, id
      end
    end
  end

  for {group, ids} <- @uncapped do
    test "group #{group}: no cap, the confidence tier still applies" do
      for id <- unquote(ids) do
        r = recipe(id)
        assert TriangleRouter.ownership_cap(r) == nil, id

        assert TriangleRouter.recipe_strategy(r) ==
                 TriangleRouter.dispatch_strategy(r["confidence"]),
               id

        assert TriangleRouter.recipe_strategy(r) == :auto_execute, id
      end
    end
  end

  test "the manifest never sends a capped recipe to auto-fix" do
    {:ok, [elim, subst, open]} =
      DispatchManifest.compile([
        {:eliminate, recipe("recipe-fix-spdx-license"), @pattern},
        {:substitute, recipe("recipe-scorecard-sast"), @pattern},
        {:eliminate, recipe("recipe-budget-resume-sweep"), @pattern}
      ])

    assert {elim["strategy"], elim["auto_fixable"], elim["ownership_cap"]} ==
             {"report_only", false, "flag_only"}

    assert {subst["strategy"], subst["ownership_cap"]} == {"report_only", "squabbler_owned"}
    assert {open["strategy"], open["ownership_cap"]} == {"auto_execute", nil}
  end
end

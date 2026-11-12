# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.PermissionRecipesReviewTierTest do
  # A top-level `contents: read` block sets every other scope to none, so a job
  # that relied on e.g. `pull-requests: read` can break. The permission recipes
  # must therefore route to :review (a PR a human reads), never :auto_execute.
  use ExUnit.Case, async: true

  @recipes_dir Path.expand("../data/verisim/recipes", __DIR__)

  for name <- ["recipe-fix-workflow-permissions", "recipe-scorecard-token-permissions"] do
    test "#{name} routes to review, never auto-execute" do
      recipe =
        Path.join(@recipes_dir, unquote(name) <> ".json") |> File.read!() |> Jason.decode!()

      assert Hypatia.TriangleRouter.dispatch_strategy(recipe["confidence"]) == :review
    end
  end
end

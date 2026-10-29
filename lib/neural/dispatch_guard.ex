# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Neural.DispatchGuard do
  @moduledoc """
  Enforce the neural coordinator's strategy as a ceiling on triangle routing.

  A missing neural coordinator must never leave an otherwise automatic
  elimination eligible for auto-execution. A recommendation of :review or
  :report_only must not be silently discarded by the recipe confidence path.

  This is a dispatch guard, not evidence that any neural model is trained,
  calibrated or safe for autonomous decisions. Model-readiness gates remain a
  separate obligation.
  """

  @review_ceiling 0.94

  @doc """
  Apply the neural dispatch safety ceiling to an already routed action.

  Accepts `{:eliminate, recipe, pattern}`, `{:substitute, recipe, pattern}`
  or `{:control, pattern}`, with maps for recipes and patterns. Recommendations
  use atom keys; pass `:unavailable` when the coordinator call fails.

  `%{is_novel: true}` takes precedence over strategy. Novel findings and
  `%{strategy: :report_only}` return `{:control, pattern}`, discarding any
  recipe. Otherwise, `%{strategy: :auto_execute}` returns the action unchanged.

  `:unavailable`, `%{strategy: :review}` and unrecognised replies preserve
  the tier but cap elimination confidence at `0.94`. Lower numeric confidence
  is preserved; missing or non-numeric confidence becomes `0.0`. The prior
  value (default `0.0`) is recorded in `"neural_prior_confidence"` on the
  recipe. Substitution confidence is unchanged.

  Restricted actions record the reason in `"neural_dispatch_guard"` on the
  pattern and any retained recipe. Unsupported action shapes raise
  `FunctionClauseError` when a restriction applies; non-map recipes or
  patterns raise `BadMapError` when accessed.
  """
  def enforce(action, :unavailable), do: review_ceiling(action, "unavailable")

  def enforce(action, %{is_novel: true}), do: control(action, "novel")
  def enforce(action, %{strategy: :report_only}), do: control(action, "report_only")
  def enforce(action, %{strategy: :review}), do: review_ceiling(action, "review")
  def enforce(action, %{strategy: :auto_execute}), do: action
  def enforce(action, _unexpected), do: review_ceiling(action, "invalid_recommendation")

  defp review_ceiling({:eliminate, recipe, pattern}, reason) do
    prior = Map.get(recipe, "confidence", 0.0)
    confidence = if is_number(prior), do: min(prior, @review_ceiling), else: 0.0

    updated_recipe =
      recipe
      |> Map.put("confidence", confidence)
      |> Map.put("neural_prior_confidence", prior)
      |> Map.put("neural_dispatch_guard", reason)

    {:eliminate, updated_recipe, Map.put(pattern, "neural_dispatch_guard", reason)}
  end

  defp review_ceiling({:substitute, recipe, pattern}, reason) do
    {:substitute, Map.put(recipe, "neural_dispatch_guard", reason),
     Map.put(pattern, "neural_dispatch_guard", reason)}
  end

  defp review_ceiling({:control, pattern}, reason) do
    {:control, Map.put(pattern, "neural_dispatch_guard", reason)}
  end

  defp control({:eliminate, _recipe, pattern}, reason), do: tagged_control(pattern, reason)
  defp control({:substitute, _recipe, pattern}, reason), do: tagged_control(pattern, reason)
  defp control({:control, pattern}, reason), do: tagged_control(pattern, reason)

  defp tagged_control(pattern, reason) do
    {:control, Map.put(pattern, "neural_dispatch_guard", reason)}
  end
end

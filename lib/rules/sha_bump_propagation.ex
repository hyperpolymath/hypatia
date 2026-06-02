# SPDX-License-Identifier: MPL-2.0
# Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.Rules.ShaBumpPropagation do
  @moduledoc """
  Detection: an estate-wide reusable workflow has been bumped to a new SHA
  upstream and consumers pinning the old SHA need propagation.

  Detection-half of the three-system propagation architecture:

      hypatia (this module)  ->  gitbot-fleet (actuation)  ->  .git-private-farm (propagation)

  Strategy is `:review` (flag-only). The finding payload feeds the actuator,
  which pre-filters by title keyword (per `feedback_pr_sweep_title_keyword_exclusion`)
  before triggering the propagation primitive.

  See hypatia#418 for full spec.
  """

  @rule :reusable_workflow_sha_bump_needs_propagation
  @severity :medium
  @strategy :review

  @typedoc """
  A pull-request merge event payload consumed by `check/1`.

    * `:source_repo`    — `"hyperpolymath/<repo>"` (string)
    * `:files_changed`  — list of file paths changed in the PR (binary list)
    * `:merge_sha`      — 40-char hex commit SHA of the merge
    * `:old_sha`        — the SHA pinned prior to this merge (40-char hex)
    * `:pr_title`       — upstream PR title (kept verbatim in the finding)
    * `:pr_number`      — upstream PR number (integer)
    * `:estimated_consumers` — optional, integer count if known upstream

  Keys are atoms; binary keys are also accepted (`normalise/1`).
  """
  @type event :: %{
          required(:source_repo) => binary(),
          required(:files_changed) => [binary()],
          required(:merge_sha) => binary(),
          required(:old_sha) => binary(),
          required(:pr_title) => binary(),
          required(:pr_number) => non_neg_integer(),
          optional(:estimated_consumers) => non_neg_integer() | nil
        }

  @typedoc "A finding map emitted when an event matches."
  @type finding :: %{
          rule: atom(),
          severity: atom(),
          strategy: atom(),
          source_repo: binary(),
          source_workflow: binary(),
          old_sha: binary(),
          new_sha: binary(),
          pr_title: binary(),
          pr_number: non_neg_integer(),
          estimated_consumers: non_neg_integer() | nil
        }

  # Estate reusable-workflow registry. Sourced from hypatia#418 issue body.
  # Each entry: `{repo, workflow_path}` — matches against `event.source_repo`
  # and any file in `event.files_changed`.
  @known_reusables [
    {"hyperpolymath/standards", ".github/workflows/governance-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/rust-ci-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/deno-ci-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/elixir-ci-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/scorecard-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/secret-scanner-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/codeql-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/hypatia-scan-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/mirror-reusable.yml"},
    {"hyperpolymath/standards", ".github/workflows/changelog-reusable.yml"}
  ]

  # Composite actions (referenced via `uses: <owner>/<repo>@<SHA>`) also flow
  # through this rule — the workflow_path is the conventional `action.yml`
  # at the action repo root.
  @known_actions [
    {"hyperpolymath/a2ml-validate-action", "action.yml"}
  ]

  @doc "List of (repo, workflow_path) tuples treated as estate reusables."
  def known_reusables, do: @known_reusables

  @doc "List of (repo, action_path) tuples treated as estate composite actions."
  def known_actions, do: @known_actions

  @doc """
  Inspect a merge event and return `[finding]` if a reusable workflow / action
  changed, or `[]` otherwise.

  Sensitivity: every change touching a known reusable's YAML fires.
  Specificity: docs-only changes (where no `.yml`/`.yaml` in the reusable
  registry was modified) do NOT fire; only the matching workflow paths are
  emitted (one finding per touched reusable).

  The title-keyword exclusion is NOT enforced here — actuation owns that
  filter. The `pr_title` field is carried verbatim so the actuator can
  reject without round-tripping back to hypatia.
  """
  @spec check(event() | map()) :: [finding()]
  def check(event) when is_map(event) do
    case normalise(event) do
      {:ok, ev} ->
        ev
        |> matched_reusables()
        |> Enum.map(&build_finding(&1, ev))

      {:error, _reason} ->
        []
    end
  end

  # --- internals --------------------------------------------------------------

  @doc false
  def normalise(event) do
    repo = fetch(event, :source_repo)
    files = fetch(event, :files_changed) || []
    merge_sha = fetch(event, :merge_sha)
    old_sha = fetch(event, :old_sha)
    title = fetch(event, :pr_title) || ""
    number = fetch(event, :pr_number)
    consumers = fetch(event, :estimated_consumers)

    with true <- is_binary(repo),
         true <- is_list(files),
         true <- sha?(merge_sha),
         true <- sha?(old_sha),
         true <- is_integer(number) do
      {:ok,
       %{
         source_repo: repo,
         files_changed: files,
         merge_sha: merge_sha,
         old_sha: old_sha,
         pr_title: title,
         pr_number: number,
         estimated_consumers: consumers
       }}
    else
      _ -> {:error, :malformed_event}
    end
  end

  defp fetch(map, key) when is_atom(key) do
    Map.get(map, key) || Map.get(map, Atom.to_string(key))
  end

  defp sha?(s) when is_binary(s), do: Regex.match?(~r/^[0-9a-f]{40}$/, s)
  defp sha?(_), do: false

  # Return the workflow paths in the event that match a known reusable for the
  # source repo. Empty list = no finding.
  defp matched_reusables(%{source_repo: repo, files_changed: files}) do
    paths_for_repo =
      (@known_reusables ++ @known_actions)
      |> Enum.filter(fn {r, _path} -> r == repo end)
      |> Enum.map(fn {_r, path} -> path end)
      |> MapSet.new()

    files
    |> Enum.filter(&MapSet.member?(paths_for_repo, &1))
    |> Enum.uniq()
  end

  defp build_finding(workflow_path, ev) do
    %{
      rule: @rule,
      severity: @severity,
      strategy: @strategy,
      source_repo: ev.source_repo,
      source_workflow: workflow_path,
      old_sha: ev.old_sha,
      new_sha: ev.merge_sha,
      pr_title: ev.pr_title,
      pr_number: ev.pr_number,
      estimated_consumers: ev.estimated_consumers
    }
  end
end

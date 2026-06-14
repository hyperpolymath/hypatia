# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.Dispatcher do
  @moduledoc """
  Wiring — the merge-orchestration runtime's nervous system.

  It connects a `Strategist` decision (route × method × safety) to hypatia's
  *existing* dispatch pipeline by mapping it onto `Hypatia.DispatchManifest`'s
  entry shape — the `tier` / `strategy` vocabulary (`eliminate|substitute|control`
  × `auto_execute|review|report_only`) that the dispatch-runner and the
  `.git-private-farm` actuator already consume — and it mints the Kin.Gate
  coordination lease (artifact 5) for the PR's territory.

  `dispatch/1` and `decide_all/1` are pure (data in, data out). `write_manifest/2`
  is the thin operational glue that emits the merge-decision JSONL.
  """

  alias Hypatia.MergeOrchestration.Strategist

  @doc "Decide, then return `{decision, manifest_entry, lease}`."
  def dispatch(ctx) do
    decision = Strategist.decide(ctx)
    {decision, to_manifest_entry(decision), lease_for(ctx, decision)}
  end

  @doc "Batch-decide over many PR contexts; returns decisions/entries/leases + stats (pure)."
  def decide_all(pr_contexts) do
    results = Enum.map(pr_contexts, &dispatch/1)
    entries = Enum.map(results, fn {_d, e, _l} -> e end)

    %{
      decisions: Enum.map(results, fn {d, _e, _l} -> d end),
      entries: entries,
      leases: Enum.map(results, fn {_d, _e, l} -> l end),
      stats: %{
        total: length(entries),
        auto_execute: Enum.count(entries, &(&1["strategy"] == "auto_execute")),
        review: Enum.count(entries, &(&1["strategy"] == "review")),
        report_only: Enum.count(entries, &(&1["strategy"] == "report_only"))
      }
    }
  end

  @doc """
  Operational glue: decide over PR contexts and write the merge-decision manifest
  (JSONL) that the dispatch-runner + the `.git-private-farm` actuator consume.
  `opts[:dir]` (default the verisim dispatch dir); `opts[:encode]` (default `Jason.encode!/1`).
  Returns `{:ok, path, stats}`.
  """
  def write_manifest(pr_contexts, opts \\ []) do
    encode = Keyword.get_lazy(opts, :encode, fn -> &Jason.encode!/1 end)
    dir = Keyword.get(opts, :dir, "data/verisim/dispatch")
    path = Path.join(dir, "merge-decisions.jsonl")

    %{entries: entries, stats: stats} = decide_all(pr_contexts)
    File.mkdir_p!(dir)
    body = Enum.map_join(entries, "\n", encode)
    File.write!(path, if(body == "", do: "", else: body <> "\n"))
    {:ok, path, stats}
  end

  @doc "Map a decision onto the existing DispatchManifest entry shape, plus a `merge` extension."
  def to_manifest_entry(decision) do
    %{
      "tier" => tier(decision.safety),
      "strategy" => strategy(decision.safety),
      "repo" => decision.pr.repo,
      "confidence" => decision.confidence,
      "severity" => "Info",
      "timestamp" => now(),
      "merge" => %{
        "pr_number" => decision.pr.number,
        "method" => to_string(decision.method),
        "safety" => to_string(decision.safety),
        "route" => decision.route,
        "vetoes" => decision.vetoes,
        "change_class" => to_string(decision.change_class),
        "change_level" => to_string(decision.change_level),
        "pool" => to_string(decision.pool),
        "rationale" => decision.rationale
      }
    }
  end

  @doc "Mint the Kin.Gate coordination lease (artifact-5 schema) for this decision's territory."
  def lease_for(ctx, decision) do
    %{
      "lease_id" => "lease-" <> String.replace(decision.pr.repo, "/", "-") <> "-" <> to_string(decision.pr.number),
      "holder" => Map.get(ctx, :holder, "hypatia"),
      "repo" => decision.pr.repo,
      "territory" => %{
        "branch" => Map.get(ctx, :branch, "main"),
        "paths" => Map.get(ctx, :paths, []),
        "is_meta" => decision.change_level == :meta
      },
      "intent" => "merge-orchestration decision",
      "status" => "held",
      "acquired_at" => now(),
      "expires_at" => later(3600),
      # The dispatcher never self-authorises a meta claim (LE2); owner must.
      "owner_authorized" => Map.get(ctx, :owner_authorized, false)
    }
  end

  # safety axis -> the existing safety-triangle dispatch vocabulary
  defp strategy(:arm_auto), do: "auto_execute"
  defp strategy(:review), do: "review"
  defp strategy(:flag), do: "report_only"

  defp tier(:arm_auto), do: "eliminate"
  defp tier(:review), do: "substitute"
  defp tier(:flag), do: "control"

  defp now, do: DateTime.utc_now() |> DateTime.to_iso8601()
  defp later(secs), do: DateTime.utc_now() |> DateTime.add(secs, :second) |> DateTime.to_iso8601()
end

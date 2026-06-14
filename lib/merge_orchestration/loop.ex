# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.Loop do
  @moduledoc """
  The live operational loop — one entry point that threads the merge-orchestration
  modules into a single run over the shared store.

      observations ─▶ Sensor.sense ─▶ (KinCompetence) ─▶ Strategist/KinCouncil ─▶ KinGate ─▶ manifest

  This is where the **gate finally bites**: every prior module decides in
  isolation, but the loop is the first place a decision is held back from
  `auto_execute` unless it can *claim the repo*. Two layers of mutual exclusion:

    * **in-cycle** — at most one armed PR per repo per run (the second same-repo
      arm is deferred to `report_only`), so the actuator never gets two
      `auto_execute` entries for one repo whose base is about to move;
    * **cross-agent / cross-cycle** — `KinGate` acquires the persistent per-repo
      lease; if another holder (another agent, or a still-live previous cycle)
      holds it, the arm is deferred. Meta claims are refused at the gate (LE2) —
      defence in depth behind the Strategist's own meta⇒flag guard.

  The brain stays token-free: the loop only *reads* the store (observations the
  farm producer deposited, the bots' signed attestations, the repos' pool
  policies) and *writes* the decision manifest + lease records. The actuator
  (`.git-private-farm`) is a separate process that reads the manifest and merges;
  the loop never calls it.

  Store layout (all under `opts[:store]`):

    * `observations/*.json`        — per-PR observations (from `observe.sh`)
    * `pools/<owner__name>.json`   — `RepoPoolPolicy` per repo
    * `attestations/*.json`        — signed bot attestations
    * `leases/<owner__name>.json`  — `KinGate` leases (written here)
    * `merge-decisions.jsonl`      — the output manifest (written here)

  `plan/6` is pure given an injected `acquire` fn (the gate logic tests with no
  filesystem); `run/1` is the thin I/O shell. JSON codec injectable (Jason in
  prod) so the logic tests run dependency-free.
  """

  alias Hypatia.MergeOrchestration.{Sensor, Strategist, Dispatcher, KinCompetence, BatonEmitter}
  alias Hypatia.MergeOrchestration.KinGate.FileStore

  @doc """
  Run one cycle over the store. Opts: `:store` (required), `:holder` ("hypatia"),
  `:now`, `:trust` (GoT snapshot `%{bot => score}`; empty ⇒ uniform council),
  `:encode`/`:decode` (Jason), `:acquire` (defaults to `KinGate.FileStore` over
  `<store>/leases`).

  `:actuation` selects the actuation backend for the *armed* decisions:

    * `:manifest` (default) — write `merge-decisions.jsonl`; the `.git-private-farm`
      actuator polls it. Returns the plan summary plus `:manifest_path`.
    * `:baton` — submit one Bag-of-Actions Baton per armed entry via
      `BatonEmitter.emit/2` (capability-routed actuation, off GitHub minutes).
      Returns the plan summary plus `:batons`; no manifest is written.
    * `:both` — do both (manifest as the durable record/fallback, Batons as the
      live path). Returns the plan summary plus `:manifest_path` *and* `:batons`.

  For the Baton backends the `Bag.Mesh` call is late-bound (`apply/3`) so this
  module stays compile-decoupled from `bag-of-actions`; inject `:submit` (a
  `spec -> result` fn) in tests, and `:budget` (a `Bag.Budget`) in production.
  """
  def run(opts) do
    store = Keyword.fetch!(opts, :store)
    decode = Keyword.get(opts, :decode, &json_decode!/1)
    encode = Keyword.get(opts, :encode, &json_encode!/1)
    holder = Keyword.get(opts, :holder, "hypatia")
    now = Keyword.get(opts, :now, DateTime.utc_now())
    trust = Keyword.get(opts, :trust, %{})
    actuation = Keyword.get(opts, :actuation, :manifest)
    leases_dir = Path.join(store, "leases")

    acquire =
      Keyword.get(opts, :acquire, fn req ->
        FileStore.acquire(leases_dir, req, now: now, encode: encode, decode: decode)
      end)

    observations = read_observations(store, decode)
    {resolve_pool, resolve_attestations} = Sensor.store_resolvers(store, decode: decode)

    plan(observations, resolve_pool, resolve_attestations, trust, holder, acquire)
    |> actuate_manifest(store, encode, actuation)
    |> actuate_batons(opts, actuation)
  end

  # ── actuation backends (selected by :actuation) ──────────────────────────────

  defp actuate_manifest(result, store, encode, actuation) when actuation in [:manifest, :both],
    do: Map.put(result, :manifest_path, write_manifest(store, result.entries, encode))

  defp actuate_manifest(result, _store, _encode, _actuation), do: result

  defp actuate_batons(result, opts, actuation) when actuation in [:baton, :both] do
    # lazy: an injected `:submit` (tests) means `default_submit/1` — and thus any
    # reference to `Bag.*` — is never built.
    submit = Keyword.get_lazy(opts, :submit, fn -> default_submit(opts) end)
    Map.put(result, :batons, BatonEmitter.emit(result, submit: submit))
  end

  defp actuate_batons(result, _opts, _actuation), do: result

  # The live `Bag.Mesh.submit_planned(spec, budget)` call, late-bound via `apply/3`
  # so an unconfigured `:manifest`-only run never references `bag-of-actions`. The
  # default budget is also late-bound (`Bag.Budget.unlimited/0`).
  defp default_submit(opts) do
    budget = Keyword.get_lazy(opts, :budget, fn -> apply(Bag.Budget, :unlimited, []) end)
    fn spec -> apply(Bag.Mesh, :submit_planned, [spec, budget]) end
  end

  @doc """
  The pure planning core: sense → (weight) → decide → gate, over already-loaded
  observations. `acquire` is `(request -> {:ok,lease} | {:conflict,l} | {:refused,r})`.
  Returns `%{results, entries, leases, stats}`.
  """
  def plan(observations, resolve_pool, resolve_attestations, trust, holder, acquire) do
    contexts =
      observations
      |> Sensor.sense(resolve_pool, resolve_attestations)
      |> Enum.map(&maybe_weight(&1, trust))

    {rev_results, _armed_repos} =
      Enum.reduce(contexts, {[], MapSet.new()}, fn ctx, {acc, armed} ->
        {result, armed} = decide_one(ctx, holder, acquire, armed)
        {[result | acc], armed}
      end)

    rev_results |> Enum.reverse() |> summarise()
  end

  # ── per-PR: decide, then gate the armed ones ─────────────────────────────────

  defp decide_one(ctx, holder, acquire, armed) do
    decision = Strategist.decide(ctx)

    if decision.safety == :arm_auto do
      gate_armed(ctx, decision, holder, acquire, armed)
    else
      {result(decision, Dispatcher.to_manifest_entry(decision), :no_gate), armed}
    end
  end

  defp gate_armed(ctx, decision, holder, acquire, armed) do
    repo = decision.pr.repo

    cond do
      # in-cycle: another PR already armed this repo this run
      MapSet.member?(armed, repo) ->
        gate = {:deferred, %{"holder" => holder, "reason" => "same-repo-this-cycle"}}
        {result(decision, downgraded_entry(decision, "same-repo-this-cycle"), gate), armed}

      true ->
        case acquire.(gate_request(ctx, decision, holder)) do
          {:ok, lease} ->
            {result(decision, Dispatcher.to_manifest_entry(decision), {:armed, lease}),
             MapSet.put(armed, repo)}

          {:conflict, blocker} ->
            {result(
               decision,
               downgraded_entry(decision, "lease-held:#{blocker["holder"]}"),
               {:deferred, blocker}
             ), armed}

          {:refused, reason} ->
            {result(
               decision,
               downgraded_entry(decision, "gate-refused:#{reason}"),
               {:blocked, reason}
             ), armed}
        end
    end
  end

  defp result(decision, entry, gate), do: %{decision: decision, entry: entry, gate: gate}

  # a gate-deferred arm becomes report_only so the actuator does not execute it
  defp downgraded_entry(decision, reason) do
    %{decision | safety: :flag, rationale: decision.rationale <> " [gate:#{reason}]"}
    |> Dispatcher.to_manifest_entry()
  end

  defp gate_request(ctx, decision, holder) do
    %{
      repo: decision.pr.repo,
      holder: holder,
      territory: %{
        branch: Map.get(ctx, :branch, "main"),
        paths: Map.get(ctx, :paths, []),
        is_meta: decision.change_level == :meta
      },
      intent: "merge-orchestration arm",
      owner_authorized: false
    }
  end

  defp maybe_weight(ctx, trust) when map_size(trust) == 0, do: ctx
  defp maybe_weight(ctx, trust), do: KinCompetence.attach_weight(ctx, trust)

  # ── summary ──────────────────────────────────────────────────────────────────

  defp summarise(results) do
    %{
      results: results,
      entries: Enum.map(results, & &1.entry),
      leases: for(%{gate: {:armed, lease}} <- results, do: lease),
      stats: %{
        total: length(results),
        armed: count_gate(results, :armed),
        deferred: count_gate(results, :deferred),
        blocked: count_gate(results, :blocked),
        review: Enum.count(results, &(&1.decision.safety == :review)),
        flagged: Enum.count(results, &(&1.decision.safety == :flag))
      }
    }
  end

  defp count_gate(results, tag), do: Enum.count(results, &match?(%{gate: {^tag, _}}, &1))

  # ── store I/O ────────────────────────────────────────────────────────────────

  defp read_observations(store, decode) do
    dir = Path.join(store, "observations")

    case File.ls(dir) do
      {:ok, names} ->
        names
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.sort()
        |> Enum.map(&decode.(File.read!(Path.join(dir, &1))))

      _ ->
        []
    end
  end

  defp write_manifest(store, entries, encode) do
    path = Path.join(store, "merge-decisions.jsonl")
    body = Enum.map_join(entries, "\n", encode)
    File.write!(path, if(body == "", do: "", else: body <> "\n"))
    path
  end

  @doc false
  def json_encode!(term), do: apply(Jason, :encode!, [term])
  @doc false
  def json_decode!(body), do: apply(Jason, :decode!, [body])
end

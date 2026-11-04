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
  farm producer deposited, the bots' attestation documents, the repos' pool
  policies) and *writes* the decision manifest + lease records. The actuator
  (`.git-private-farm`) is a separate process that reads the manifest and merges;
  the loop never calls it.

  Store layout (all under `opts[:store]`):

    * `observations/.observation-batch.json` — complete batch membership + digests
    * `observations/*.json`        — per-PR observations named by that receipt
    * `pools/<owner__name>.json`   — `RepoPoolPolicy` per repo
    * `attestations/*.json`        — bot documents; signatures are not verified here
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
    * `:baton` — report explicit Baton refusals for any armed entries. Mutation
      is unavailable until an actual verifier/executor contract is qualified;
      descriptive metadata does not execute the farm's re-verification.
    * `:both` — write the manifest and report those refusals.

  Sensor-fed evidence is currently capped at review because no production
  signature verifier is present. Neither backend bypasses that cap.
  """
  def run(opts) do
    store = Keyword.fetch!(opts, :store)
    decode = Keyword.get(opts, :decode, &json_decode!/1)
    encode = Keyword.get(opts, :encode, &json_encode!/1)
    holder = Keyword.get(opts, :holder, "hypatia")
    now = Keyword.get(opts, :now, DateTime.utc_now())
    trust = Keyword.get(opts, :trust, %{})
    actuation = Keyword.get(opts, :actuation, :manifest)

    unless actuation in [:manifest, :baton, :both],
      do: raise(ArgumentError, "unknown actuation backend")

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
    Map.put(result, :batons, BatonEmitter.emit(result, opts))
  end

  defp actuate_batons(result, _opts, _actuation), do: result

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
    receipt_path = Path.join(dir, ".observation-batch.json")
    receipt_body = read_regular!(receipt_path)
    receipt = decode.(receipt_body)

    unless valid_batch?(receipt),
      do: unqualified_batch!("missing, incomplete or malformed receipt")

    observations =
      Enum.map(receipt["observations"], fn entry ->
        body = read_regular!(Path.join(dir, entry["file"]))
        digest = :crypto.hash(:sha256, body) |> Base.encode16(case: :lower)
        obs = decode.(body)

        unless digest == entry["sha256"] and Sensor.valid_observation?(obs) and
                 obs["repo"] == entry["repo"] and obs["number"] == entry["number"] and
                 obs["head_sha"] == entry["head_sha"] and obs["event_id"] == receipt["event_id"] and
                 obs["observed_at"] == receipt["observed_at"] do
          unqualified_batch!("observation digest, subject or event mismatch")
        end

        obs
      end)

    # A producer publishes `collecting` before changing files and `complete`
    # last. Re-read after the files so a concurrent collection cannot be mixed.
    unless read_regular!(receipt_path) == receipt_body,
      do: unqualified_batch!("receipt changed while reading")

    observations
  end

  defp valid_batch?(%{} = receipt) do
    repos = receipt["repos"]
    entries = receipt["observations"]

    receipt["schema_version"] == 1 and receipt["status"] == "complete" and
      Sensor.valid_event?(receipt["event_id"]) and is_binary(receipt["observed_at"]) and
      match?({:ok, _, _}, DateTime.from_iso8601(receipt["observed_at"])) and
      is_list(repos) and repos != [] and Enum.uniq(repos) == repos and
      Enum.all?(repos, &Sensor.valid_subject?(&1, 1, String.duplicate("0", 40))) and
      is_list(entries) and Enum.all?(entries, &valid_batch_entry?(&1, repos)) and
      Enum.uniq_by(entries, & &1["file"]) == entries
  end

  defp valid_batch?(_), do: false

  defp valid_batch_entry?(
         %{
           "repo" => repo,
           "number" => number,
           "head_sha" => head,
           "file" => file,
           "sha256" => digest
         },
         repos
       ) do
    Sensor.valid_subject?(repo, number, head) and repo in repos and
      file == String.replace(repo, "/", "__") <> "__" <> to_string(number) <> ".json" and
      is_binary(digest) and Regex.match?(~r/^[0-9a-f]{64}$/, digest)
  end

  defp valid_batch_entry?(_, _), do: false

  defp read_regular!(path) do
    case File.lstat(path) do
      {:ok, %File.Stat{type: :regular}} -> File.read!(path)
      _ -> unqualified_batch!("receipt/observation unavailable or not a regular file")
    end
  end

  defp unqualified_batch!(reason),
    do: raise(ArgumentError, "unqualified observation batch: #{reason}; no new manifest written")

  defp write_manifest(store, entries, encode) do
    path = Path.join(store, "merge-decisions.jsonl")
    body = Enum.map_join(entries, "\n", encode)
    temporary = path <> ".tmp-" <> to_string(System.unique_integer([:positive]))

    try do
      File.write!(temporary, if(body == "", do: "", else: body <> "\n"), [:exclusive])
      File.rename!(temporary, path)
    after
      File.rm(temporary)
    end

    path
  end

  @doc false
  def json_encode!(term), do: apply(Jason, :encode!, [term])
  @doc false
  def json_decode!(body), do: apply(Jason, :decode!, [body])
end

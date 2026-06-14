# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.KinGate do
  @moduledoc """
  The coordination authority — "one bot per repo at a time".

  The runtime tier `.machine_readable/` was missing: it is *declarative*
  (contractiles) and *validating* (k9) but had no record of who is *acting* on
  what right now. A **lease** (artifact-5 `CoordinationLease`) fills it, and
  `KinGate` is the atomic-acquire authority that mints and honours them —
  central, not per-repo lock files (which have the bootstrap race: two agents
  pull, both write the lock, both push, last-writer-wins, both think they hold
  it).

  `decide_acquire/3` is the pure core. Two schema invariants are enforced here,
  not merely validated after the fact:

    * **LE1** — a `held` lease always carries an `expires_at` (TTL). A crashed
      agent's claim self-expires; no immortal locks. `mint`/`renew` cannot
      produce a held lease without one.
    * **LE2** — a claim over **meta** territory (it would edit the oracle: CI,
      hypatia rules, `bot_directives`, contractiles, `standards`) is *refused*
      unless `owner_authorized`. The reflexivity guard at lease level: an agent
      cannot self-authorise a meta claim.

  Conflict granularity is the **repo** (the actuator merges one PR per repo at a
  time anyway — serialising avoids base-moved conflicts). A same-holder
  re-acquire **renews**; a different holder on a live lease **conflicts**; an
  expired lease is taken over. Leases are emitted in the on-disk a5 shape
  (string-keyed) so any tool — the brain, the farm hand — reads the same record.
  """

  @default_ttl 3600

  @type request :: %{
          required(:repo) => String.t(),
          required(:holder) => String.t(),
          required(:territory) => map(),
          optional(:intent) => String.t(),
          optional(:owner_authorized) => boolean(),
          optional(:ttl) => non_neg_integer(),
          optional(:lease_id) => String.t()
        }

  @doc """
  Decide whether `request` may acquire, given the `existing` leases (a5-shaped,
  string-keyed) and `now`. Returns `{:ok, lease}` (mint or renew), `{:conflict,
  blocking_lease}`, or `{:refused, reason}` (LE2).
  """
  def decide_acquire(existing, request, now \\ DateTime.utc_now()) when is_list(existing) do
    now = to_dt(now)
    repo = request.repo
    holder = request.holder
    live = Enum.filter(existing, &live?(&1, repo, now))

    cond do
      meta_unauthorised?(request) ->
        {:refused, "meta:owner-unauthorized"}

      own = Enum.find(live, &(&1["holder"] == holder)) ->
        {:ok, renew(own, now, ttl(request))}

      blocker = Enum.find(live, &(&1["holder"] != holder)) ->
        {:conflict, blocker}

      true ->
        {:ok, mint(request, now)}
    end
  end

  @doc "Mark a lease released (idempotent)."
  def release(lease), do: Map.put(lease, "status", "released")

  @doc "Is this held lease still live at `now` (held + not past its TTL)?"
  def live?(lease, repo, now) do
    lease["repo"] == repo and lease["status"] == "held" and not expired?(lease, now)
  end

  @doc "A lease is expired when its TTL is absent/unparseable or not in the future."
  def expired?(lease, now) do
    case parse(lease["expires_at"]) do
      {:ok, dt} -> DateTime.compare(dt, to_dt(now)) != :gt
      :error -> true
    end
  end

  # ── minting (LE1 holds by construction) ──────────────────────────────────────

  defp mint(request, now) do
    t = request.territory

    %{
      "lease_id" => Map.get(request, :lease_id, default_id(request.repo, now)),
      "holder" => request.holder,
      "repo" => request.repo,
      "territory" => %{
        "branch" => Map.get(t, :branch, Map.get(t, "branch", "main")),
        "paths" => Map.get(t, :paths, Map.get(t, "paths", [])),
        "is_meta" => is_meta?(t)
      },
      "intent" => Map.get(request, :intent, "merge-orchestration"),
      "status" => "held",
      "acquired_at" => iso(now),
      # LE1: a held lease always carries a TTL.
      "expires_at" => iso(DateTime.add(now, ttl(request), :second)),
      "owner_authorized" => !!Map.get(request, :owner_authorized, false)
    }
  end

  # renew keeps identity + original acquisition, pushes the TTL forward
  defp renew(lease, now, ttl_secs) do
    lease
    |> Map.put("status", "held")
    |> Map.put("expires_at", iso(DateTime.add(now, ttl_secs, :second)))
  end

  defp meta_unauthorised?(request) do
    is_meta?(request.territory) and not (!!Map.get(request, :owner_authorized, false))
  end

  defp is_meta?(t), do: Map.get(t, :is_meta, Map.get(t, "is_meta", false)) == true

  defp ttl(request), do: Map.get(request, :ttl, @default_ttl)
  defp default_id(repo, now), do: "lease-" <> flatten(repo) <> "-" <> stamp(now)
  defp flatten(repo), do: String.replace(repo, "/", "__")
  defp stamp(now), do: now |> DateTime.to_unix() |> Integer.to_string()

  defp iso(%DateTime{} = dt), do: DateTime.to_iso8601(dt)
  defp to_dt(%DateTime{} = dt), do: dt
  defp to_dt(s) when is_binary(s), do: elem(parse(s), 1)

  defp parse(nil), do: :error

  defp parse(s) when is_binary(s) do
    case DateTime.from_iso8601(s) do
      {:ok, dt, _} -> {:ok, dt}
      _ -> :error
    end
  end

  defp parse(%DateTime{} = dt), do: {:ok, dt}
  defp parse(_), do: :error
end

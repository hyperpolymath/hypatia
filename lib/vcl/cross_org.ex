# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.VCL.CrossOrg do
  @moduledoc """
  Cross-organisation federation with policy gates (M18).

  `Hypatia.VCL.RemoteExecutor` already federates queries across
  multiple verisim-data stores when they all belong to the same
  organisation (same trust level, same admin). This module adds the
  layer above: querying Hypatia instances belonging to OTHER
  organisations with explicit policy gates governing what their
  findings mean here.

  The threat model is that a federated peer is *not* a malicious
  actor (they're a partner org with a federation agreement) but they
  ARE running their own confidence model, severity catalog, and
  suppression rules — none of which automatically apply locally.
  Without policy gates, accepting their findings verbatim would
  effectively let them lower our severity thresholds.

  ## Policy DSL

  Per-peer policies live in app config:

      config :hypatia, :cross_org_policies, [
        %{
          peer_id: "rhodicorp-prod",
          base_url: "https://hypatia.rhodicorp.example",
          trust_level: :medium,
          accept_severities: [:critical, :high],
          remap_medium_to: :low,
          override_dismissals: true,
          max_age_days: 14,
          require_re_scan: true,
          bearer_token_env: "HYPATIA_PEER_RHODICORP_TOKEN"
        }
      ]

  ### Fields

    * `peer_id` — opaque label for logs / attribution
    * `base_url` — Hypatia's public surface; we hit /graphql and /api/*
    * `trust_level` — :high | :medium | :low; informs how much we
      downscale their confidence
    * `accept_severities` — only findings at these severities are
      adopted; everything else is dropped
    * `remap_medium_to` — translate their :medium to a different local
      severity (defensive against severity inflation)
    * `override_dismissals` — when true, ignore the peer's `dismissed`
      state and re-evaluate locally (their dismissal vocabulary may
      not match ours)
    * `max_age_days` — drop findings older than this; protects against
      ingesting stale data when a peer goes quiet
    * `require_re_scan` — when true, mark imported findings as
      `requires_local_verification = true` so the auto_execute gate
      refuses them until our scanner re-confirms
    * `bearer_token_env` — name of the env var holding the bearer for
      THAT peer's /api/* gate. Each peer can have a different token.

  ## Drift detection

  Policies can carry a `drift_threshold` (default 0.30). If a peer's
  confidence on a recipe-id differs from our local confidence by more
  than this fraction, an alert fires (`hypatia.cross_org.drift`) and
  the import is logged with the divergence. Operators decide whether
  the peer's confidence model has rotted, ours has, or the recipe
  itself is non-portable across orgs.

  ## What this does NOT do

    * Push our findings TO peer orgs. That's their inbound problem;
      they should configure their own CrossOrg policies pointing at us.
      (For one-way push, see the existing `Sinks.Peer` federation in
      the alerts pipeline.)
    * Reconcile recipe IDs across orgs. If we both have recipe IDs
      "recipe-foo" but they refer to different patterns, no mapping
      happens; drift_threshold will catch the divergence.
    * Replace VCL.RemoteExecutor. That stays the within-org tool.

  ## Example

      iex> Hypatia.VCL.CrossOrg.pull_findings(peer_id: "rhodicorp-prod",
      ...>   limit: 50)
      {:ok, [%{...}, ...], %{accepted: 47, dropped: 3, drift_alerts: 1}}
  """

  require Logger

  @doc """
  Pull recent findings from a named peer, gated by its policy.

  Returns `{:ok, accepted_findings, stats}` where stats is a map of
  counts (`:accepted`, `:dropped_severity`, `:dropped_age`,
  `:dropped_dismissed`, `:drift_alerts`).
  """
  def pull_findings(opts) do
    peer_id = Keyword.fetch!(opts, :peer_id)
    limit = Keyword.get(opts, :limit, 100)

    case find_policy(peer_id) do
      nil ->
        {:error, "unknown_peer_id: #{peer_id}"}

      policy ->
        token = policy_token(policy)

        case fetch_peer_recipes(policy, token, limit) do
          {:ok, rows} ->
            {accepted, stats} = apply_policy(rows, policy)

            Hypatia.Telemetry.cross_org_import(
              peer_id: peer_id,
              accepted: stats.accepted,
              dropped: stats.dropped_severity + stats.dropped_age + stats.dropped_dismissed
            )

            {:ok, accepted, stats}

          {:error, reason} ->
            {:error, "fetch failed: #{reason}"}
        end
    end
  end

  @doc """
  List configured peers (read-only view of the app config).
  """
  def peers do
    Application.get_env(:hypatia, :cross_org_policies, [])
  end

  @doc """
  Look up a peer policy by id.
  """
  def find_policy(peer_id) do
    Enum.find(peers(), fn p -> Map.get(p, :peer_id) == peer_id end)
  end

  # ─── Fetch ─────────────────────────────────────────────────────────────

  # Pulls peer recipes through their /graphql endpoint. We use GraphQL
  # rather than /api/recipes because:
  #   1. GraphQL is documented as the stable cross-org surface in our
  #      v8.0 roadmap;
  #   2. The peer's bearer-token gate still applies (their auth_gate
  #      plug is in front of /graphql by configuration);
  #   3. The response is self-describing (we don't have to pre-agree on
  #      a JSON shape with every peer).
  defp fetch_peer_recipes(policy, token, limit) do
    base = String.trim_trailing(policy.base_url, "/")
    url = "#{base}/graphql"
    query = "{ recipes }"

    args =
      [
        "-sS",
        "--max-time",
        "10",
        "-X",
        "POST",
        "-H",
        "Content-Type: application/json"
      ] ++ token_header(token) ++ [
        "-d",
        Jason.encode!(%{query: query, limit: limit}),
        url
      ]

    case System.cmd("curl", args, stderr_to_stdout: true) do
      {body, 0} ->
        case Jason.decode(body) do
          {:ok, %{"data" => %{"recipes" => %{"rows" => rows}}}} ->
            {:ok, rows}

          {:ok, %{"data" => %{"recipes" => rows}}} when is_list(rows) ->
            {:ok, rows}

          {:ok, %{"errors" => errors}} ->
            {:error, "peer returned errors: #{inspect(errors)}"}

          _ ->
            {:error, "peer response did not match expected shape"}
        end

      {error, _code} ->
        {:error, "curl: #{String.slice(error, 0, 200)}"}
    end
  end

  defp token_header(nil), do: []
  defp token_header(""), do: []
  defp token_header(token), do: ["-H", "Authorization: Bearer #{token}"]

  defp policy_token(policy) do
    case Map.get(policy, :bearer_token_env) do
      nil -> nil
      env_var -> System.get_env(env_var)
    end
  end

  # ─── Policy application ────────────────────────────────────────────────

  defp apply_policy(rows, policy) do
    accept_severities = Map.get(policy, :accept_severities, [:critical, :high, :medium, :low])
    max_age_days = Map.get(policy, :max_age_days, 30)
    override_dismissals = Map.get(policy, :override_dismissals, false)
    remap_medium_to = Map.get(policy, :remap_medium_to)
    require_re_scan = Map.get(policy, :require_re_scan, true)
    drift_threshold = Map.get(policy, :drift_threshold, 0.30)

    initial_stats = %{
      accepted: 0,
      dropped_severity: 0,
      dropped_age: 0,
      dropped_dismissed: 0,
      drift_alerts: 0
    }

    {accepted, stats} =
      Enum.reduce(rows, {[], initial_stats}, fn row, {acc, stats} ->
        severity = row_severity(row) |> remap_severity(remap_medium_to)
        age = row_age_days(row)
        dismissed? = not override_dismissals and row_dismissed?(row)

        cond do
          severity not in accept_severities and severity not in Enum.map(accept_severities, &to_string/1) ->
            {acc, %{stats | dropped_severity: stats.dropped_severity + 1}}

          age > max_age_days ->
            {acc, %{stats | dropped_age: stats.dropped_age + 1}}

          dismissed? ->
            {acc, %{stats | dropped_dismissed: stats.dropped_dismissed + 1}}

          true ->
            new_stats = maybe_alert_drift(stats, row, drift_threshold, policy)

            tagged =
              row
              |> Map.put("peer_id", policy.peer_id)
              |> Map.put("requires_local_verification", require_re_scan)
              |> Map.put("local_severity", severity)

            {[tagged | acc], %{new_stats | accepted: new_stats.accepted + 1}}
        end
      end)

    {Enum.reverse(accepted), stats}
  end

  defp maybe_alert_drift(stats, row, threshold, policy) do
    recipe_id = row_recipe_id(row)
    peer_conf = row_confidence(row)

    case local_confidence(recipe_id) do
      nil ->
        # Recipe not present locally — no drift to compute.
        stats

      local_conf when is_number(local_conf) and is_number(peer_conf) ->
        delta = abs(local_conf - peer_conf)

        if delta > threshold do
          Hypatia.Telemetry.cross_org_drift(
            peer_id: policy.peer_id,
            recipe_id: recipe_id,
            peer_confidence: peer_conf,
            local_confidence: local_conf,
            delta: delta
          )

          %{stats | drift_alerts: stats.drift_alerts + 1}
        else
          stats
        end

      _ ->
        stats
    end
  end

  # ─── Row accessors (defensive against schema drift) ────────────────────

  defp row_severity(row) do
    Map.get(row, "severity") || Map.get(row, "local_severity") || "medium"
  end

  defp row_age_days(row) do
    case Map.get(row, "last_seen") || Map.get(row, "created_at") do
      nil ->
        0

      iso when is_binary(iso) ->
        case DateTime.from_iso8601(iso) do
          {:ok, dt, _} -> DateTime.diff(DateTime.utc_now(), dt, :day)
          _ -> 0
        end

      _ ->
        0
    end
  end

  defp row_dismissed?(row) do
    state = Map.get(row, "state") || Map.get(row, "status")
    state in ["dismissed", "resolved", "ignored"]
  end

  defp row_recipe_id(row) do
    Map.get(row, "recipe_id") || Map.get(row, "id")
  end

  defp row_confidence(row) do
    Map.get(row, "confidence") || get_in(row, ["verification", "rate"])
  end

  defp remap_severity("medium", target) when not is_nil(target), do: target
  defp remap_severity(:medium, target) when not is_nil(target), do: target
  defp remap_severity(s, _), do: s

  # ─── Local confidence lookup ───────────────────────────────────────────

  defp local_confidence(nil), do: nil

  defp local_confidence(recipe_id) do
    case Hypatia.RecipeMatcher.get_recipe(recipe_id) do
      nil -> nil
      recipe -> Map.get(recipe, "confidence")
    end
  rescue
    _ -> nil
  catch
    _, _ -> nil
  end
end

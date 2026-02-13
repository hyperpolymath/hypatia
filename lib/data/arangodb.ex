# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Data.ArangoDB do
  @moduledoc """
  Elixir client for ArangoDB in the federated Hypatia architecture.

  Sits alongside verisimdb-data (git-backed canonical store) providing:
  - Graph traversal for trust propagation
  - Time-series queries for confidence history
  - Neural state persistence
  - Anomaly storage and querying
  - Aggregation queries for fleet intelligence

  Configuration via environment variables:
  - HYPATIA_ARANGO_URL (default: http://localhost:8529)
  - HYPATIA_ARANGO_DB (default: hypatia)
  - HYPATIA_ARANGO_USER (default: root)
  - HYPATIA_ARANGO_PASS (default: "")
  """

  use GenServer
  require Logger

  @default_url "http://localhost:8529"
  @default_db "hypatia"
  @sync_interval_ms 10 * 60 * 1_000  # Sync from verisimdb-data every 10 min

  # All collections in the extended schema
  @document_collections [
    "repos", "findings", "patterns", "bots", "recipes", "outcomes",
    "contributors", "confidence_history", "anomalies", "dispatch_batches",
    "neural_states", "sessions", "rulesets", "learning_data"
  ]

  @edge_collections [
    "trusts", "depends_on", "dispatches", "applies_to",
    "repo_has_finding", "finding_matches_pattern", "bot_executes_recipe",
    "recipe_has_outcome", "contributor_fixes"
  ]

  defstruct [
    :base_url, :database, :auth_header,
    connected: false, last_sync: nil, sync_timer: nil
  ]

  # --- GenServer API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    url = System.get_env("HYPATIA_ARANGO_URL", @default_url)
    db = System.get_env("HYPATIA_ARANGO_DB", @default_db)
    user = System.get_env("HYPATIA_ARANGO_USER", "root")
    pass = System.get_env("HYPATIA_ARANGO_PASS", "")

    auth = Base.encode64("#{user}:#{pass}")

    state = %__MODULE__{
      base_url: url,
      database: db,
      auth_header: "Basic #{auth}"
    }

    # Try to connect, but don't crash if ArangoDB isn't available
    state = try_connect(state)

    if state.connected do
      ensure_schema(state)
      timer = Process.send_after(self(), :sync_from_verisimdb, @sync_interval_ms)
      Logger.info("ArangoDB connected: #{url}/#{db}")
      {:ok, %{state | sync_timer: timer}}
    else
      Logger.warning("ArangoDB not available at #{url} — running in degraded mode (flat files only)")
      {:ok, state}
    end
  end

  # --- Public API ---

  @doc "Check if ArangoDB is connected"
  def connected? do
    GenServer.call(__MODULE__, :connected?)
  end

  @doc "Insert or update a document"
  def upsert(collection, key, document) do
    GenServer.call(__MODULE__, {:upsert, collection, key, document})
  end

  @doc "Get a document by key"
  def get(collection, key) do
    GenServer.call(__MODULE__, {:get, collection, key})
  end

  @doc "Execute an AQL query"
  def query(aql, bind_vars \\ %{}) do
    GenServer.call(__MODULE__, {:query, aql, bind_vars}, 30_000)
  end

  @doc "Insert a trust edge"
  def upsert_trust(from_collection, from_key, to_collection, to_key, trust_score, evidence_count) do
    edge = %{
      "_from" => "#{from_collection}/#{from_key}",
      "_to" => "#{to_collection}/#{to_key}",
      "trust_score" => trust_score,
      "evidence_count" => evidence_count,
      "last_updated" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
    upsert("trusts", "#{from_key}_#{to_key}", edge)
  end

  @doc "Record a confidence history entry"
  def record_confidence(recipe_id, confidence, event_type, delta, trigger) do
    ts = DateTime.utc_now() |> DateTime.to_iso8601()
    key = "#{recipe_id}_#{System.system_time(:millisecond)}"
    doc = %{
      "recipe_id" => recipe_id,
      "timestamp" => ts,
      "confidence" => confidence,
      "event_type" => event_type,
      "delta" => delta,
      "trigger" => trigger
    }
    upsert("confidence_history", key, doc)
  end

  @doc "Record an anomaly"
  def record_anomaly(event, deviation, source_network, severity) do
    ts = DateTime.utc_now() |> DateTime.to_iso8601()
    key = "anomaly_#{System.system_time(:millisecond)}"
    doc = %{
      "event" => event,
      "deviation" => deviation,
      "timestamp" => ts,
      "source_network" => to_string(source_network),
      "severity" => to_string(severity),
      "resolved" => false
    }
    upsert("anomalies", key, doc)
  end

  @doc "Save neural network state"
  def save_neural_state(network_name, state_data, cycle_count, accuracy_metrics) do
    key = to_string(network_name)
    doc = %{
      "network" => key,
      "state_data" => state_data,
      "cycle_count" => cycle_count,
      "last_updated" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "accuracy_metrics" => accuracy_metrics
    }
    upsert("neural_states", key, doc)
  end

  @doc "Load neural network state"
  def load_neural_state(network_name) do
    get("neural_states", to_string(network_name))
  end

  @doc "Get trust graph edges for PageRank computation"
  def get_trust_edges do
    query("""
    FOR e IN trusts
      RETURN {from: e._from, to: e._to, score: e.trust_score, evidence: e.evidence_count}
    """)
  end

  @doc "Get confidence history for a recipe (for ESN training)"
  def get_confidence_history(recipe_id, limit \\ 500) do
    query("""
    FOR h IN confidence_history
      FILTER h.recipe_id == @recipe_id
      SORT h.timestamp ASC
      LIMIT @limit
      RETURN {confidence: h.confidence, timestamp: h.timestamp, event: h.event_type}
    """, %{"recipe_id" => recipe_id, "limit" => limit})
  end

  @doc "Get repos with declining trust (AQL graph traversal)"
  def repos_with_declining_trust(threshold \\ 0.4) do
    query("""
    FOR r IN repos
      FILTER r.trust_score < @threshold AND r.status == 'active'
      SORT r.trust_score ASC
      RETURN {name: r.name, trust: r.trust_score, findings: r.finding_count, last_scanned: r.last_scanned}
    """, %{"threshold" => threshold})
  end

  @doc "Get unresolved anomalies"
  def get_unresolved_anomalies do
    query("""
    FOR a IN anomalies
      FILTER a.resolved == false
      SORT a.timestamp DESC
      RETURN a
    """)
  end

  @doc "Find cross-repo patterns (same pattern across multiple repos)"
  def cross_repo_patterns(min_repos \\ 5) do
    query("""
    FOR f IN findings
      FILTER f.status == 'open'
      COLLECT pattern = f.pattern_id INTO repos = f.repo
      LET unique_repos = LENGTH(UNIQUE(repos))
      FILTER unique_repos >= @min_repos
      SORT unique_repos DESC
      RETURN {pattern: pattern, repo_count: unique_repos, repos: UNIQUE(repos)}
    """, %{"min_repos" => min_repos})
  end

  @doc "Bot performance ranking with quarantine status"
  def bot_performance do
    query("""
    FOR b IN bots
      LET outcomes = (
        FOR o IN outcomes
          FILTER o.bot == b.name
          COLLECT outcome = o.outcome WITH COUNT INTO cnt
          RETURN {outcome, cnt}
      )
      RETURN {
        name: b.name,
        trust: b.trust_score,
        quarantined: b.quarantined,
        success_rate: b.success_rate,
        dispatches: b.total_dispatches,
        outcomes: outcomes
      }
    """)
  end

  @doc "Sync data from verisimdb-data flat files into ArangoDB"
  def sync_from_verisimdb do
    GenServer.cast(__MODULE__, :sync_from_verisimdb)
  end

  # --- GenServer Callbacks ---

  def handle_call(:connected?, _from, state) do
    {:reply, state.connected, state}
  end

  def handle_call({:upsert, collection, key, document}, _from, state) do
    if state.connected do
      result = do_upsert(state, collection, key, document)
      {:reply, result, state}
    else
      {:reply, {:error, :not_connected}, state}
    end
  end

  def handle_call({:get, collection, key}, _from, state) do
    if state.connected do
      result = do_get(state, collection, key)
      {:reply, result, state}
    else
      {:reply, {:error, :not_connected}, state}
    end
  end

  def handle_call({:query, aql, bind_vars}, _from, state) do
    if state.connected do
      result = do_query(state, aql, bind_vars)
      {:reply, result, state}
    else
      {:reply, {:error, :not_connected}, state}
    end
  end

  def handle_cast(:sync_from_verisimdb, state) do
    if state.connected do
      do_sync(state)
      {:noreply, %{state | last_sync: DateTime.utc_now()}}
    else
      {:noreply, state}
    end
  end

  def handle_info(:sync_from_verisimdb, state) do
    if state.connected do
      do_sync(state)
      timer = Process.send_after(self(), :sync_from_verisimdb, @sync_interval_ms)
      {:noreply, %{state | last_sync: DateTime.utc_now(), sync_timer: timer}}
    else
      timer = Process.send_after(self(), :sync_from_verisimdb, @sync_interval_ms)
      {:noreply, %{state | sync_timer: timer}}
    end
  end

  # --- HTTP Client ---

  defp try_connect(state) do
    url = "#{state.base_url}/_api/version"
    case http_get(url, state.auth_header) do
      {:ok, %{"server" => "arango"}} -> %{state | connected: true}
      _ -> %{state | connected: false}
    end
  end

  defp ensure_schema(state) do
    db_url = "#{state.base_url}/_db/#{state.database}"

    # Create database if it doesn't exist
    http_post("#{state.base_url}/_api/database", state.auth_header,
      %{"name" => state.database})

    # Create document collections
    Enum.each(@document_collections, fn name ->
      http_post("#{db_url}/_api/collection", state.auth_header,
        %{"name" => name, "type" => 2})
    end)

    # Create edge collections
    Enum.each(@edge_collections, fn name ->
      http_post("#{db_url}/_api/collection", state.auth_header,
        %{"name" => name, "type" => 3})
    end)

    # Create indices for common queries
    create_index(state, "confidence_history", ["recipe_id", "timestamp"])
    create_index(state, "outcomes", ["timestamp"])
    create_index(state, "outcomes", ["recipe_id"])
    create_index(state, "findings", ["repo", "severity"])
    create_index(state, "findings", ["pattern_id"])
    create_index(state, "anomalies", ["resolved", "timestamp"])
    create_index(state, "repos", ["trust_score"])
    create_index(state, "bots", ["quarantined"])

    # Create named graph
    http_post("#{db_url}/_api/gharial", state.auth_header, %{
      "name" => "hypatia_graph",
      "edgeDefinitions" => [
        %{"collection" => "trusts", "from" => ["repos", "bots", "recipes", "contributors"], "to" => ["repos", "bots", "recipes", "contributors"]},
        %{"collection" => "depends_on", "from" => ["repos"], "to" => ["repos"]},
        %{"collection" => "dispatches", "from" => ["findings"], "to" => ["bots"]},
        %{"collection" => "applies_to", "from" => ["recipes"], "to" => ["patterns"]},
        %{"collection" => "repo_has_finding", "from" => ["repos"], "to" => ["findings"]},
        %{"collection" => "finding_matches_pattern", "from" => ["findings"], "to" => ["patterns"]},
        %{"collection" => "bot_executes_recipe", "from" => ["bots"], "to" => ["recipes"]},
        %{"collection" => "recipe_has_outcome", "from" => ["recipes"], "to" => ["outcomes"]},
        %{"collection" => "contributor_fixes", "from" => ["contributors"], "to" => ["outcomes"]}
      ]
    })

    Logger.info("ArangoDB schema ensured: #{length(@document_collections)} document + #{length(@edge_collections)} edge collections")
  end

  defp create_index(state, collection, fields) do
    db_url = "#{state.base_url}/_db/#{state.database}"
    http_post("#{db_url}/_api/index?collection=#{collection}", state.auth_header,
      %{"type" => "persistent", "fields" => fields})
  end

  defp do_upsert(state, collection, key, document) do
    db_url = "#{state.base_url}/_db/#{state.database}"
    doc = Map.put(document, "_key", key)
    aql = """
    UPSERT {_key: @key}
    INSERT @doc
    UPDATE @doc
    IN @@collection
    RETURN NEW
    """
    do_query(state, aql, %{"key" => key, "doc" => doc, "@collection" => collection})
  end

  defp do_get(state, collection, key) do
    db_url = "#{state.base_url}/_db/#{state.database}"
    url = "#{db_url}/_api/document/#{collection}/#{key}"
    http_get(url, state.auth_header)
  end

  defp do_query(state, aql, bind_vars) do
    db_url = "#{state.base_url}/_db/#{state.database}"
    url = "#{db_url}/_api/cursor"
    body = %{"query" => aql, "bindVars" => bind_vars}
    case http_post(url, state.auth_header, body) do
      {:ok, %{"result" => result}} -> {:ok, result}
      {:ok, %{"error" => true, "errorMessage" => msg}} -> {:error, msg}
      error -> error
    end
  end

  # --- verisimdb-data Sync ---

  defp do_sync(state) do
    verisimdb_path = Path.expand("~/Documents/hyperpolymath-repos/verisimdb-data")

    sync_scans(state, verisimdb_path)
    sync_outcomes(state, verisimdb_path)
    sync_recipes(state, verisimdb_path)

    Logger.info("ArangoDB sync from verisimdb-data complete")
  rescue
    e -> Logger.error("ArangoDB sync failed: #{inspect(e)}")
  end

  defp sync_scans(state, base_path) do
    scans_dir = Path.join(base_path, "scans")
    case File.ls(scans_dir) do
      {:ok, repos} ->
        Enum.each(repos, fn repo_dir ->
          scan_path = Path.join([scans_dir, repo_dir, "latest.json"])
          case File.read(scan_path) do
            {:ok, content} ->
              case Jason.decode(content) do
                {:ok, scan} ->
                  findings = Map.get(scan, "findings", [])
                  do_upsert(state, "repos", repo_dir, %{
                    "name" => repo_dir,
                    "finding_count" => length(findings),
                    "last_scanned" => Map.get(scan, "timestamp", ""),
                    "status" => "active"
                  })
                  # Sync individual findings
                  Enum.each(findings, fn finding ->
                    f_key = "#{repo_dir}_#{:erlang.phash2(finding)}"
                    do_upsert(state, "findings", f_key, Map.merge(finding, %{
                      "repo" => repo_dir,
                      "status" => "open"
                    }))
                  end)
                _ -> :skip
              end
            _ -> :skip
          end
        end)
      _ -> :ok
    end
  end

  defp sync_outcomes(state, base_path) do
    outcomes_dir = Path.join(base_path, "outcomes")
    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.each(fn file ->
          path = Path.join(outcomes_dir, file)
          case File.read(path) do
            {:ok, content} ->
              content
              |> String.split("\n", trim: true)
              |> Enum.each(fn line ->
                case Jason.decode(line) do
                  {:ok, outcome} ->
                    key = "outcome_#{:erlang.phash2(outcome)}"
                    do_upsert(state, "outcomes", key, outcome)
                  _ -> :skip
                end
              end)
            _ -> :skip
          end
        end)
      _ -> :ok
    end
  end

  defp sync_recipes(state, base_path) do
    recipes_dir = Path.join(base_path, "recipes")
    case File.ls(recipes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.each(fn file ->
          path = Path.join(recipes_dir, file)
          case File.read(path) do
            {:ok, content} ->
              case Jason.decode(content) do
                {:ok, recipe} ->
                  key = Map.get(recipe, "id", Path.rootname(file))
                  do_upsert(state, "recipes", key, recipe)
                _ -> :skip
              end
            _ -> :skip
          end
        end)
      _ -> :ok
    end
  end

  # --- HTTP Helpers ---

  defp http_get(url, auth_header) do
    :inets.start()
    :ssl.start()

    headers = [
      {~c"Authorization", String.to_charlist(auth_header)},
      {~c"Accept", ~c"application/json"}
    ]

    case :httpc.request(:get, {String.to_charlist(url), headers}, [{:timeout, 10_000}], []) do
      {:ok, {{_, 200, _}, _, body}} -> Jason.decode(to_string(body))
      {:ok, {{_, status, _}, _, body}} -> {:error, {status, to_string(body)}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp http_post(url, auth_header, body) do
    :inets.start()
    :ssl.start()

    headers = [
      {~c"Authorization", String.to_charlist(auth_header)},
      {~c"Accept", ~c"application/json"}
    ]

    json_body = Jason.encode!(body)

    case :httpc.request(:post, {String.to_charlist(url), headers, ~c"application/json", json_body},
      [{:timeout, 10_000}], []) do
      {:ok, {{_, code, _}, _, resp_body}} when code in 200..299 ->
        Jason.decode(to_string(resp_body))
      {:ok, {{_, code, _}, _, resp_body}} when code in 300..499 ->
        Jason.decode(to_string(resp_body))
      {:ok, {{_, status, _}, _, resp_body}} ->
        {:error, {status, to_string(resp_body)}}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

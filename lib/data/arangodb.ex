# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Data.ArangoDB do
  @moduledoc """
  Elixir client for ArangoDB in the federated Hypatia architecture.

  Sits alongside verisim-data (git-backed canonical store) providing:
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
  # Sync from verisim-data every 10 min
  @sync_interval_ms 10 * 60 * 1_000

  # All collections in the extended schema
  @document_collections [
    "repos",
    "findings",
    "patterns",
    "bots",
    "recipes",
    "outcomes",
    "contributors",
    "confidence_history",
    "anomalies",
    "dispatch_batches",
    "neural_states",
    "sessions",
    "rulesets",
    "learning_data"
  ]

  @edge_collections [
    "trusts",
    "depends_on",
    "dispatches",
    "applies_to",
    "repo_has_finding",
    "finding_matches_pattern",
    "bot_executes_recipe",
    "recipe_has_outcome",
    "contributor_fixes"
  ]

  @graph_name "hypatia_graph"

  @graph_edge_definitions [
    %{
      "collection" => "trusts",
      "from" => ["repos", "bots", "recipes", "contributors"],
      "to" => ["repos", "bots", "recipes", "contributors"]
    },
    %{"collection" => "depends_on", "from" => ["repos"], "to" => ["repos"]},
    %{"collection" => "dispatches", "from" => ["findings"], "to" => ["bots"]},
    %{"collection" => "applies_to", "from" => ["recipes"], "to" => ["patterns"]},
    %{"collection" => "repo_has_finding", "from" => ["repos"], "to" => ["findings"]},
    %{"collection" => "finding_matches_pattern", "from" => ["findings"], "to" => ["patterns"]},
    %{"collection" => "bot_executes_recipe", "from" => ["bots"], "to" => ["recipes"]},
    %{"collection" => "recipe_has_outcome", "from" => ["recipes"], "to" => ["outcomes"]},
    %{"collection" => "contributor_fixes", "from" => ["contributors"], "to" => ["outcomes"]}
  ]

  defstruct [
    :base_url,
    :database,
    :auth_header,
    connected: false,
    last_sync: nil,
    sync_timer: nil,
    setup_timer: nil,
    setup_attempts: 0
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

    # Never crashes when ArangoDB is unavailable: setup/1 degrades and retries.
    {:ok, setup(state)}
  end

  # Connects and ensures the schema. The client counts as connected only once
  # both succeed; until then it stays in degraded mode and retries with an
  # exponential backoff (`:arango_setup_retry_base_ms` doubling up to
  # `:arango_setup_retry_max_ms`), so a transient failure such as a 503
  # during setup does not leave it disconnected for good. A retry repeats
  # every step; a collection or index created by an earlier attempt answers
  # 1207 and is accepted, and an existing graph (1925) is accepted once
  # check_existing_graph/1 has verified it. The sync timer starts only after
  # setup succeeds.
  defp setup(state) do
    result =
      if reachable?(state) do
        with {:error, reason} <- ensure_schema(state),
             do: {:error, "ArangoDB schema initialisation failed: #{inspect(reason)}"}
      else
        {:error, "ArangoDB not available at #{state.base_url}"}
      end

    case result do
      :ok ->
        timer = Process.send_after(self(), :sync_from_verisimdb, @sync_interval_ms)
        Logger.info("ArangoDB connected: #{state.base_url}/#{state.database}")
        %{state | connected: true, sync_timer: timer, setup_timer: nil, setup_attempts: 0}

      {:error, message} ->
        delay = setup_retry_delay(state.setup_attempts)
        level = if state.setup_attempts == 0, do: :warning, else: :debug

        Logger.log(
          level,
          "#{message} -- running in degraded mode (flat files only); retrying setup in #{delay} ms"
        )

        timer = Process.send_after(self(), :retry_setup, delay)

        %{
          state
          | connected: false,
            setup_timer: timer,
            setup_attempts: state.setup_attempts + 1
        }
    end
  end

  defp setup_retry_delay(attempts) do
    base = Application.get_env(:hypatia, :arango_setup_retry_base_ms, 5_000)
    cap = Application.get_env(:hypatia, :arango_setup_retry_max_ms, 5 * 60 * 1_000)
    min(base * Integer.pow(2, min(attempts, 20)), cap)
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
    query(
      """
      FOR h IN confidence_history
        FILTER h.recipe_id == @recipe_id
        SORT h.timestamp ASC
        LIMIT @limit
        RETURN {confidence: h.confidence, timestamp: h.timestamp, event: h.event_type}
      """,
      %{"recipe_id" => recipe_id, "limit" => limit}
    )
  end

  @doc "Get repos with declining trust (AQL graph traversal)"
  def repos_with_declining_trust(threshold \\ 0.4) do
    query(
      """
      FOR r IN repos
        FILTER r.trust_score < @threshold AND r.status == 'active'
        SORT r.trust_score ASC
        RETURN {name: r.name, trust: r.trust_score, findings: r.finding_count, last_scanned: r.last_scanned}
      """,
      %{"threshold" => threshold}
    )
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
    query(
      """
      FOR f IN findings
        FILTER f.status == 'open'
        COLLECT pattern = f.pattern_id INTO repos = f.repo
        LET unique_repos = LENGTH(UNIQUE(repos))
        FILTER unique_repos >= @min_repos
        SORT unique_repos DESC
        RETURN {pattern: pattern, repo_count: unique_repos, repos: UNIQUE(repos)}
      """,
      %{"min_repos" => min_repos}
    )
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

  @doc "Sync data from verisim-data flat files into ArangoDB"
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
      {:noreply, sync_and_record(state)}
    else
      {:noreply, state}
    end
  end

  # `last_sync` moves only on a run in which every upsert succeeded; after a
  # failed run it keeps the last successful timestamp.
  defp sync_and_record(state) do
    case do_sync(state) do
      :ok -> %{state | last_sync: DateTime.utc_now()}
      {:error, _failures} -> state
    end
  end

  def handle_info(:retry_setup, state) do
    {:noreply, setup(%{state | setup_timer: nil})}
  end

  def handle_info(:sync_from_verisimdb, state) do
    if state.connected do
      timer = Process.send_after(self(), :sync_from_verisimdb, @sync_interval_ms)
      {:noreply, %{sync_and_record(state) | sync_timer: timer}}
    else
      timer = Process.send_after(self(), :sync_from_verisimdb, @sync_interval_ms)
      {:noreply, %{state | sync_timer: timer}}
    end
  end

  # --- HTTP Client ---

  # Probes the target database first, so a user who administers only that
  # database (and cannot read `_system`) still connects. A 404 database-not-
  # found there means the database is not created yet: fall back to the
  # `_system` version probe, so that setup can try to create it.
  defp reachable?(state) do
    target = http_get("#{db_url(state)}/_api/version", state.auth_header)

    cond do
      arango_version?(target) ->
        true

      database_not_found?(target) ->
        arango_version?(http_get("#{state.base_url}/_api/version", state.auth_header))

      true ->
        false
    end
  end

  defp arango_version?({:ok, %{"server" => "arango"}}), do: true
  defp arango_version?(_), do: false

  # ArangoDB answers a request for a database that does not exist with a 404
  # and errorNum 1228 (ERROR_ARANGO_DATABASE_NOT_FOUND).
  defp database_not_found?({:error, {404, body}}) do
    match?({:ok, %{"errorNum" => 1228}}, Jason.decode(body))
  end

  defp database_not_found?(_), do: false

  defp db_url(state), do: "#{state.base_url}/_db/#{state.database}"

  defp ensure_schema(state) do
    # Every later step depends on the database, so stop here if it fails.
    database = ensure_database(state)

    if schema_result_ok?(database),
      do: ensure_collections_and_graph(state),
      else: {:error, database}
  end

  # Creates the database only when the probe says it does not exist (404,
  # errorNum 1228). Creating one needs Administrate on `_system`; a
  # least-privilege user that administers only a pre-provisioned target
  # database gets a 403 there. Any other probe failure (a timeout, a 3xx, a
  # 5xx) is no evidence of absence and is returned as it is.
  defp ensure_database(state) do
    probe = http_get("#{db_url(state)}/_api/database/current", state.auth_header)

    case probe do
      {:ok, %{} = current} ->
        {:ok, 200, current}

      _ ->
        if database_not_found?(probe) do
          http_post_status("#{state.base_url}/_api/database", state.auth_header, %{
            "name" => state.database
          })
        else
          probe
        end
    end
  end

  defp ensure_collections_and_graph(state) do
    db_url = db_url(state)

    # Create document collections
    documents =
      Enum.map(@document_collections, fn name ->
        http_post_status("#{db_url}/_api/collection", state.auth_header, %{
          "name" => name,
          "type" => 2
        })
      end)

    # Create edge collections
    edges =
      Enum.map(@edge_collections, fn name ->
        http_post_status("#{db_url}/_api/collection", state.auth_header, %{
          "name" => name,
          "type" => 3
        })
      end)

    # Create indices for common queries
    indices = [
      create_index(state, "confidence_history", ["recipe_id", "timestamp"]),
      create_index(state, "outcomes", ["timestamp"]),
      create_index(state, "outcomes", ["recipe_id"]),
      create_index(state, "findings", ["repo", "severity"]),
      create_index(state, "findings", ["pattern_id"]),
      create_index(state, "anomalies", ["resolved", "timestamp"]),
      create_index(state, "repos", ["trust_score"]),
      create_index(state, "bots", ["quarantined"])
    ]

    # Create named graph
    graph =
      case http_post_status("#{db_url}/_api/gharial", state.auth_header, %{
             "name" => @graph_name,
             "edgeDefinitions" => @graph_edge_definitions
           }) do
        {:ok, status, %{"errorNum" => 1925}} when status in 400..499 ->
          check_existing_graph(state)

        created ->
          created
      end

    results = documents ++ edges ++ indices ++ [graph]

    case Enum.find(results, &(not schema_result_ok?(&1))) do
      nil ->
        Logger.info(
          "ArangoDB schema ensured: #{length(@document_collections)} document + #{length(@edge_collections)} edge collections"
        )

        :ok

      failure ->
        {:error, failure}
    end
  end

  # A duplicate-graph answer (1925) says only that a graph of that name
  # exists, not that it is ours. It is accepted when, for every required
  # edge definition, the existing graph has an edge definition on the same
  # collection whose `from` and `to` vertex collections are exactly the
  # required ones (as sets: order and duplicates do not matter). Extra edge
  # definitions and orphan collections in the existing graph are allowed.
  # A mismatch is a schema failure; the graph is never replaced.
  defp check_existing_graph(state) do
    case http_get("#{db_url(state)}/_api/gharial/#{@graph_name}", state.auth_header) do
      {:ok, %{"graph" => %{"edgeDefinitions" => existing}} = body} when is_list(existing) ->
        case graph_mismatches(existing) do
          [] -> {:ok, 200, body}
          mismatches -> {:error, {:graph_mismatch, mismatches}}
        end

      other ->
        {:error, {:graph_unreadable, other}}
    end
  end

  defp graph_mismatches(existing) do
    by_collection = Map.new(existing, &{&1["collection"], &1})

    @graph_edge_definitions
    |> Enum.reject(&same_vertices?(&1, Map.get(by_collection, &1["collection"])))
    |> Enum.map(& &1["collection"])
  end

  defp same_vertices?(required, %{} = actual) do
    Enum.all?(["from", "to"], fn side ->
      is_list(actual[side]) and MapSet.new(actual[side]) == MapSet.new(required[side])
    end)
  end

  defp same_vertices?(_required, nil), do: false

  # A schema step succeeds on a 2xx, or on a 4xx that says the resource
  # already exists (1207 duplicate name), which happens on every
  # initialisation after the first. A duplicate graph (1925) is checked by
  # check_existing_graph/1 instead. Any other status fails, even with a JSON
  # body: a gateway's `{"message": "forbidden"}` is not a success.
  defp schema_result_ok?({:ok, status, _}) when status in 200..299, do: true

  defp schema_result_ok?({:ok, status, %{"errorNum" => 1207}}) when status in 400..499,
    do: true

  defp schema_result_ok?(_), do: false

  defp create_index(state, collection, fields) do
    db_url = "#{state.base_url}/_db/#{state.database}"

    http_post_status("#{db_url}/_api/index?collection=#{collection}", state.auth_header, %{
      "type" => "persistent",
      "fields" => fields
    })
  end

  defp do_upsert(state, collection, key, document) do
    _db_url = "#{state.base_url}/_db/#{state.database}"
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

  # --- verisim-data Sync ---

  # Returns :ok when every upsert succeeded, otherwise {:error, failures},
  # each failure `{collection, key, reason}` (or the exception that stopped
  # the run). A failed run is logged, never reported as complete.
  defp do_sync(state) do
    verisimdb_path = Application.get_env(:hypatia, :verisimdb_data_path, "data/verisim")

    failures =
      sync_scans(state, verisimdb_path) ++
        sync_outcomes(state, verisimdb_path) ++
        sync_recipes(state, verisimdb_path)

    case failures do
      [] ->
        Logger.info("ArangoDB sync from verisim-data complete")
        :ok

      [first | _] ->
        Logger.error(
          "ArangoDB sync from verisim-data failed (#{length(failures)} failed upserts, " <>
            "first: #{inspect(first)})"
        )

        {:error, failures}
    end
  rescue
    e ->
      Logger.error("ArangoDB sync failed: #{inspect(e)}")
      {:error, [e]}
  end

  # Upserts and returns the failure, if any, as a list for flat_map. A
  # cursor answer without a result list (an error body with no message) is a
  # failure too.
  defp sync_upsert(state, collection, key, document) do
    case do_upsert(state, collection, key, document) do
      {:ok, result} when is_list(result) -> []
      {:error, reason} -> [{collection, key, reason}]
      other -> [{collection, key, other}]
    end
  end

  defp sync_scans(state, base_path) do
    scans_dir = Path.join(base_path, "scans")

    case File.ls(scans_dir) do
      {:ok, repos} ->
        Enum.flat_map(repos, fn repo_dir ->
          scan_path = Path.join([scans_dir, repo_dir, "latest.json"])

          with {:ok, content} <- File.read(scan_path),
               {:ok, scan} <- Jason.decode(content) do
            findings = Map.get(scan, "findings", [])

            repo =
              sync_upsert(state, "repos", repo_dir, %{
                "name" => repo_dir,
                "finding_count" => length(findings),
                "last_scanned" => Map.get(scan, "timestamp", ""),
                "status" => "active"
              })

            # Sync individual findings
            repo ++
              Enum.flat_map(findings, fn finding ->
                f_key = "#{repo_dir}_#{:erlang.phash2(finding)}"

                sync_upsert(
                  state,
                  "findings",
                  f_key,
                  Map.merge(finding, %{"repo" => repo_dir, "status" => "open"})
                )
              end)
          else
            _ -> []
          end
        end)

      _ ->
        []
    end
  end

  defp sync_outcomes(state, base_path) do
    outcomes_dir = Path.join(base_path, "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn file ->
          case File.read(Path.join(outcomes_dir, file)) do
            {:ok, content} ->
              content
              |> String.split("\n", trim: true)
              |> Enum.flat_map(fn line ->
                case Jason.decode(line) do
                  {:ok, outcome} ->
                    sync_upsert(state, "outcomes", "outcome_#{:erlang.phash2(outcome)}", outcome)

                  _ ->
                    []
                end
              end)

            _ ->
              []
          end
        end)

      _ ->
        []
    end
  end

  defp sync_recipes(state, base_path) do
    recipes_dir = Path.join(base_path, "recipes")

    case File.ls(recipes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.flat_map(fn file ->
          with {:ok, content} <- File.read(Path.join(recipes_dir, file)),
               {:ok, recipe} <- Jason.decode(content) do
            key = Map.get(recipe, "id", Path.rootname(file))
            sync_upsert(state, "recipes", key, recipe)
          else
            _ -> []
          end
        end)

      _ ->
        []
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

    case :httpc.request(
           :get,
           {String.to_charlist(url), headers},
           [{:timeout, 10_000}, {:autoredirect, false}],
           []
         ) do
      {:ok, {{_, 200, _}, _, body}} -> Jason.decode(to_string(body))
      {:ok, {{_, status, _}, _, body}} -> {:error, {status, to_string(body)}}
      {:error, reason} -> {:error, reason}
    end
  end

  # Decoded body on a 2xx or 4xx (ArangoDB reports its errors as JSON 4xx);
  # `{:error, {status, body}}` on any other status, a 3xx included.
  defp http_post(url, auth_header, body) do
    case http_post_status(url, auth_header, body) do
      {:ok, _status, decoded} -> {:ok, decoded}
      error -> error
    end
  end

  # As http_post/3, but keeps the status, so a caller can tell a 2xx from a
  # 4xx whose body does not use ArangoDB's error shape.
  defp http_post_status(url, auth_header, body) do
    :inets.start()
    :ssl.start()

    headers = [
      {~c"Authorization", String.to_charlist(auth_header)},
      {~c"Accept", ~c"application/json"}
    ]

    json_body = Jason.encode!(body)

    case :httpc.request(
           :post,
           {String.to_charlist(url), headers, ~c"application/json", json_body},
           [{:timeout, 10_000}, {:autoredirect, false}],
           []
         ) do
      {:ok, {{_, code, _}, _, resp_body}} when code in 200..299 or code in 400..499 ->
        with {:ok, decoded} <- Jason.decode(to_string(resp_body)) do
          {:ok, code, decoded}
        end

      {:ok, {{_, status, _}, _, resp_body}} ->
        {:error, {status, to_string(resp_body)}}

      {:error, reason} ->
        {:error, reason}
    end
  end
end

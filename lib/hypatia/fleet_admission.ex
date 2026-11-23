# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.FleetAdmission do
  @moduledoc """
  Bounded fleet-attempt/1 admission from the configured fleet file.

  The outcome append commits an attempt. A later scan of a mutable checkout is
  correlated, unbound audit evidence; it grants neither success nor blame.
  The local import lock serializes this reader in one VM, not across VMs.
  """

  require Logger
  alias Hypatia.{EventId, LearningScheduler, OutcomeTracker, Paths, Receipt}

  @version "fleet-attempt/1"
  @log "shared-context/learning/fix-outcomes.jsonl"
  @assertion_fields ~w(attempt_contract event_id bot recipe_id pattern pattern_id match_kind repo repo_relative_path file line outcome verdict exit_code before_hits after_hits timestamp)

  @doc false
  def import(line_number, expected_source, expected_digest, data_root, persist)
      when is_integer(line_number) and line_number > 0 and is_function(persist, 6) do
    # Explicitly local nodes: no distributed exactly-once promise. The callback
    # is OutcomeTracker's private append path, not a caller-supplied trust flag.
    :global.trans(
      {{__MODULE__, :import}, self()},
      fn ->
        with :ok <- source_unchanged(expected_source, data_root),
             :ok <- distinct_source_ledger(expected_source, data_root),
             {:ok, bytes} <- source_line(expected_source, line_number),
             true <- sha(bytes) == expected_digest,
             {:ok, raw} <- Jason.decode(bytes),
             :ok <- valid_identity(raw) do
          source = %{
            "path" => expected_source,
            "log" => @log,
            "line" => line_number,
            "row_sha256" => expected_digest,
            "bot" => Map.get(raw, "bot", "unknown")
          }

          case committed(data_root, raw["event_id"]) do
            {:ok, row} ->
              if row["admission_version"] != @version and
                   not Map.has_key?(raw, "attempt_contract"),
                 do: {:replayed, row},
                 else: replay(data_root, row, source, bytes)

            :missing ->
              case validate(raw) do
                :ok -> admit(data_root, raw, source, persist)
                {:reject, reason} -> {:rejected, reason}
              end

            {:error, _} = error ->
              error
          end
        else
          false -> {:error, :source_row_changed}
          {:reject, reason} -> {:rejected, reason}
          {:error, _} = error -> error
        end
      end,
      [node()]
    )
  end

  defp valid_identity(raw) when is_map(raw) do
    if EventId.valid?(raw["event_id"]), do: :ok, else: {:reject, :invalid_event_id}
  end

  defp valid_identity(_), do: {:reject, :invalid_attempt_record}

  defp source_unchanged(source, data_root) do
    configured = Path.join(Path.expand(Paths.fleet()), @log)

    if source == configured and data_root == Path.expand(Paths.verisimdb_data()),
      do: :ok,
      else: {:error, :admission_store_or_source_changed}
  end

  defp source_line(source, line_number) do
    with {:ok, content} <- File.read(source) do
      lines = String.split(content, "\n", trim: false)

      if line_number < length(lines),
        do: {:ok, Enum.at(lines, line_number - 1)},
        else: {:error, :incomplete_source_row}
    end
  end

  defp distinct_source_ledger(source, data_root) do
    with {:ok, source_stat} <- File.stat(source) do
      alias? =
        for(
          directory <- ["outcomes", "receipts"],
          path <- Path.wildcard(Path.join([data_root, directory, "*.jsonl"])),
          do: path
        )
        |> Enum.any?(fn path -> same_file_stat?(source_stat, path) end)

      if alias?, do: {:error, :source_outcome_alias}, else: :ok
    end
  end

  @doc false
  def source_alias?(path) do
    case File.stat(Path.join(Path.expand(Paths.fleet()), @log)) do
      {:ok, source_stat} -> same_file_stat?(source_stat, path)
      _ -> false
    end
  end

  defp same_file_stat?(source_stat, path) do
    case File.stat(path) do
      {:ok, stat} ->
        {stat.major_device, stat.minor_device, stat.inode} ==
          {source_stat.major_device, source_stat.minor_device, source_stat.inode}

      _ ->
        false
    end
  end

  defp validate(%{"attempt_contract" => @version, "bot" => "dispatch-runner"} = raw) do
    recipe = raw["recipe_id"] || raw["pattern"]

    cond do
      not EventId.valid?(raw["event_id"]) -> {:reject, :invalid_event_id}
      not named?(recipe) or not named?(raw["repo"]) -> {:reject, :invalid_attempt_subject}
      true -> :ok
    end
  end

  defp validate(%{"attempt_contract" => @version}), do: {:reject, :invalid_source_bot}
  defp validate(_), do: {:reject, :unsupported_attempt_contract}
  defp named?(text), do: is_binary(text) and String.trim(text) != ""

  defp committed(data_root, id) do
    directory = Path.join(data_root, "outcomes")

    case File.ls(directory) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.sort()
        |> Enum.reduce_while({:ok, []}, fn name, {:ok, matches} ->
          with {:ok, bytes} <- File.read(Path.join(directory, name)),
               true <- bytes == "" or String.ends_with?(bytes, "\n"),
               {:ok, found} <- committed_rows(bytes, id) do
            {:cont, {:ok, matches ++ found}}
          else
            reason -> {:halt, {:error, {:admission_log_unreadable, name, reason}}}
          end
        end)
        |> case do
          {:ok, []} -> :missing
          {:ok, [row]} -> {:ok, row}
          {:ok, _} -> {:error, {:duplicate_committed_identity, id}}
          error -> error
        end

      {:error, :enoent} ->
        :missing

      {:error, reason} ->
        {:error, {:admission_log_unreadable, reason}}
    end
  end

  defp committed_rows(bytes, id) do
    bytes
    |> String.split("\n", trim: true)
    |> Enum.reduce_while({:ok, []}, fn line, {:ok, found} ->
      case Jason.decode(line) do
        {:ok, row} when is_map(row) ->
          if row["event_id"] == id or row["fleet_import_id"] == id,
            do: {:cont, {:ok, [row | found]}},
            else: {:cont, {:ok, found}}

        _ ->
          {:halt, {:error, :invalid_outcome_row}}
      end
    end)
  end

  defp replay(data_root, row, source, bytes) do
    original = Map.get(row, "producer_source", %{})

    if row["admission_version"] == @version and
         Map.take(original, ~w(path row_sha256 bot)) == Map.take(source, ~w(path row_sha256 bot)) do
      case ensure_receipt(data_root, row) do
        :ok -> {:replayed, row}
        {:error, reason} -> {:error, {:receipt_pending, row["event_id"], reason}}
      end
    else
      Logger.warning(
        "Fleet admission conflict for event_id=#{row["event_id"]}; no second admission"
      )

      with :ok <- preserve_conflict(data_root, row, source, bytes) do
        {:conflict, row}
      end
    end
  end

  defp preserve_conflict(data_root, row, source, bytes) do
    conflict = %{
      "attempt_event_id" => row["event_id"],
      "authorising" => false,
      "committed_source" => Map.get(row, "producer_source"),
      "source" => source,
      "source_row_sha256" => source["row_sha256"],
      "raw_source_bytes" => bytes,
      "observed_at" => DateTime.to_iso8601(DateTime.utc_now())
    }

    path =
      Path.join([data_root, "admission-conflicts", Date.to_iso8601(Date.utc_today()) <> ".jsonl"])

    with :ok <- source_unchanged(source["path"], data_root),
         :ok <- File.mkdir_p(Path.dirname(path)),
         :ok <- File.write(path, Jason.encode!(conflict) <> "\n", [:append]) do
      :ok
    else
      {:error, reason} -> {:error, {:conflict_evidence_write, reason}}
    end
  end

  defp admit(data_root, raw, source, persist) do
    {_kind, recipe, repo, file, outcome, verification} =
      LearningScheduler.classify_fleet_record(raw)

    metadata =
      %{
        "admission_version" => @version,
        "event_id" => raw["event_id"],
        "source_bot" => "dispatch-runner",
        "producer_assertion" => Map.take(raw, @assertion_fields),
        "producer_source" => source
      }
      |> Map.merge(
        Map.take(raw, ~w(verdict exit_code before_hits after_hits pattern_id match_kind))
      )
      |> Map.merge(observation(data_root, raw, recipe, file, source))

    with :ok <- source_unchanged(source["path"], data_root),
         {:ok, row} <- persist.(recipe, repo, file, outcome, verification, metadata) do
      # Count only the row actually committed, not the pre-scan routing label.
      case ensure_receipt(data_root, row) do
        :ok -> {:stored, row}
        {:error, reason} -> {:stored_pending, row, reason}
      end
    end
  end

  defp ensure_receipt(data_root, row) do
    if Receipt.exists?(data_root, row["event_id"]), do: :ok, else: Receipt.write(data_root, row)
  rescue
    error -> {:error, {:receipt_exception, Exception.message(error)}}
  end

  defp observation(data_root, raw, recipe, file, source) do
    # Rescans are useful only for otherwise eligible success assertions. They
    # cannot upgrade no_op/unverified/invalid claims, or blame a later checkout.
    eligible =
      match?(
        {:ok, _, _, _, :success, "verified"},
        LearningScheduler.classify_fleet_record(Map.delete(raw, "attempt_contract"))
      )

    if eligible do
      with {:ok, target} <- target(raw["repo_relative_path"], file),
           {:ok, category} <- category(data_root, recipe),
           {:ran, result} <-
             OutcomeTracker.observe_fleet_target(target, category, file, raw["line"]) do
        %{
          "rescan_observation" => %{
            "observation_id" => EventId.generate(),
            "attempt_event_id" => raw["event_id"],
            "source_row_sha256" => source["row_sha256"],
            "observer" => "hypatia",
            "binding" => "unbound",
            "observed_at" => DateTime.to_iso8601(DateTime.utc_now()),
            "result" => Atom.to_string(result),
            "target" => %{
              "repo_relative_path" => raw["repo_relative_path"],
              "repo_path" => target,
              "file" => file,
              "line" => raw["line"],
              "category" => category
            }
          }
        }
      else
        {:error, reason} -> %{"rescan_not_run" => Atom.to_string(reason)}
        {:not_run, reason} -> %{"rescan_not_run" => Atom.to_string(reason)}
      end
    else
      %{"rescan_not_run" => "not_eligible_success"}
    end
  end

  defp target(relative, file) do
    case System.get_env("HYPATIA_REPOS_DIR") do
      root when is_binary(root) and root != "" ->
        with true <- safe_relative?(relative),
             true <- file == "." or safe_relative?(file),
             root = Path.expand(root),
             :ok <- directory_without_symlinks(root),
             target = Path.join(root, relative),
             :ok <- directory_without_symlinks(target),
             :ok <- safe_file(target, file) do
          {:ok, target}
        else
          false -> {:error, :unsafe_relative_target}
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :repos_root_not_configured}
    end
  end

  defp safe_relative?(path) when is_binary(path) do
    path != "" and Path.type(path) == :relative and
      not String.contains?(path, ["\\", ":", <<0>>]) and
      Enum.all?(String.split(path, "/", trim: false), &(&1 not in ["", ".", ".."]))
  end

  defp safe_relative?(_), do: false

  # Rejecting every symlink (including internal links) is deliberately stricter
  # than lexical confinement. This preflight is not a post-state snapshot.
  defp directory_without_symlinks(path) do
    path
    |> Path.split()
    |> Enum.reduce_while({:ok, ""}, fn component, {:ok, parent} ->
      current = if parent == "", do: component, else: Path.join(parent, component)

      case File.lstat(current) do
        {:ok, %{type: :directory}} -> {:cont, {:ok, current}}
        {:ok, %{type: :symlink}} -> {:halt, {:error, :symlink_target}}
        _ -> {:halt, {:error, :target_unavailable}}
      end
    end)
    |> case do
      {:ok, _} -> :ok
      error -> error
    end
  end

  defp safe_file(_target, "."), do: :ok

  defp safe_file(target, file) do
    path = Path.join(target, file)

    with :ok <- directory_without_symlinks(Path.dirname(path)) do
      case File.lstat(path) do
        {:ok, %{type: :regular}} -> :ok
        {:ok, %{type: :symlink}} -> {:error, :symlink_target}
        _ -> {:error, :target_file_unavailable}
      end
    end
  end

  defp category(data_root, recipe) do
    directory = Path.join(data_root, "recipes")

    with {:ok, files} <- File.ls(directory) do
      files
      |> Enum.filter(&(String.starts_with?(&1, "recipe-") and String.ends_with?(&1, ".json")))
      |> Enum.reduce_while({:ok, []}, fn file, {:ok, found} ->
        with {:ok, bytes} <- File.read(Path.join(directory, file)),
             {:ok, item} when is_map(item) <- Jason.decode(bytes) do
          {:cont, {:ok, if(item["id"] == recipe, do: [item | found], else: found)}}
        else
          _ -> {:halt, {:error, :recipe_registry_unreadable}}
        end
      end)
      |> case do
        {:ok, []} ->
          {:error, :recipe_not_found}

        {:ok, [%{"target_categories" => [category]}]} when is_binary(category) ->
          if String.trim(category) != "", do: {:ok, category}, else: {:error, :category_missing}

        {:ok, [_]} ->
          {:error, :category_missing_or_ambiguous}

        {:ok, _} ->
          {:error, :recipe_mapping_ambiguous}

        error ->
          error
      end
    else
      _ -> {:error, :recipe_registry_unavailable}
    end
  end

  defp sha(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end

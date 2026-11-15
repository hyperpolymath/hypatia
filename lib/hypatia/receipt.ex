# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Receipt do
  @moduledoc """
  The E0 receipt (`docs/design/e0-receipt.adoc`, RULED 2026-11-15; contract
  `schema/receipt.ncl`).

  A receipt says: this event changed what a learner holds, and here is the exact
  evidence that it rested on. It is an index into the log that is kept: it names a
  row by its `event_id` and the digest of its bytes, never a count. It is
  append-only. A receipt is a pure function of the stored row (and of where the row
  sits in its log), so a missing receipt can be written again later, identically.

  ## Order of writes (design section 5.3)

  The log row is the commit of the event. A receipt is written only for a row that
  is in the log: `write/3` refuses a row it cannot find. A failed receipt write
  never hides a committed event from the caller; `OutcomeTracker.persist_outcome/6`
  logs `receipt_pending` and `repair/2` writes the receipt the next time the recipe
  is recomputed.

  ## Canonical form (design section 6)

  One receipt on each line of `receipts/YYYY-MM.jsonl` under the data root of the
  row. Keys sorted, no whitespace, one final LF, ASCII only, no JSON numbers: the
  profile of `scripts/proof-ledger.sh`, in which `jq -cS` is the canonical form. A
  field that holds text outside printable ASCII is written with `%XX` for each
  such byte (and for `%` itself), so that the receipt stays ASCII and can be
  decoded again.

  `receipt_sha256` is the SHA-256 (lowercase hex) of the canonical form of all the
  other fields, so no field can change without the digest changing.
  """

  require Logger

  alias Hypatia.EventId
  alias Hypatia.OutcomeTracker

  @chain_version "e0-receipt/1"
  @fleet_log "shared-context/learning/fix-outcomes.jsonl"
  @rescans ~w(verified still_present scan_failed scan_skipped)
  @qualifications ~w(verified failed incomplete still_present)

  # ── Building ───────────────────────────────────────────────────────────

  @doc """
  Build the receipt of a stored row.

  `row` is the row as the log holds it (string keys, with an `event_id`). `ctx` says
  where the evidence sits: `:kind`, `:log` (a path relative to the data root, or a
  label for a source that is not a file), `:line` (a decimal string) and
  `:row_sha256` (the digest of the exact evidence bytes).
  """
  @spec build(map(), map()) :: map()
  def build(row, ctx) when is_map(row) and is_map(ctx) do
    event_id = Map.fetch!(row, "event_id")
    kind = ctx.kind
    verdict = verdict_of(row)
    observations = observations(row, kind)

    proof_or_test_ref = %{"row_sha256" => ctx.row_sha256}

    provenance = %{
      "event_id_source" => event_id_source(row),
      "producer" => ascii(producer(row, kind)),
      "writer" => "hypatia"
    }

    present = fn map, key -> Map.has_key?(map, key) end

    missing =
      (["head_sha", "match_kind", "pattern_id", "subject_id", "subject_identity"] ++
         for(
           key <- ~w(after_hits before_hits exit_code rescan verdict),
           not present.(observations, key),
           do: key
         ) ++
         ["scan_digest", "test_digest", "tool_version"])
      |> Enum.sort()
      |> Enum.uniq()

    body = %{
      "assertion" => %{
        "claimed_outcome" => claimed_outcome(row),
        "file" => ascii(to_text(Map.get(row, "file", ""))),
        "recipe_id" => ascii(to_text(Map.get(row, "recipe_id", ""))),
        "repo" => ascii(to_text(Map.get(row, "repo", "")))
      },
      "chain_version" => @chain_version,
      "event_id" => event_id,
      "missing_evidence" => missing,
      "observations" => observations,
      "parent_event_ids" => [],
      "proof_or_test_ref" => proof_or_test_ref,
      "provenance" => provenance,
      "qualification" => qualification_of(row, verdict),
      "source" => %{"kind" => Atom.to_string(kind), "line" => ctx.line, "log" => ctx.log},
      "verdict" => verdict
    }

    {:ok, digest} = digest(body)
    Map.put(body, "receipt_sha256", digest)
  end

  # What the single gate does with the row.
  defp verdict_of(row) do
    case OutcomeTracker.qualified_outcome_counts([row]) do
      %{successes: 1} -> "credited_success"
      %{failures: 1} -> "credited_failure"
      %{false_positives: 1} -> "credited_false_positive"
      _ -> "audit_only"
    end
  end

  # The verification as the gate reads it. A rollback that the gate does not credit
  # (no batch fields) is "unqualified", so the verdict can be derived from the pair.
  defp qualification_of(row, verdict) do
    case Map.get(row, "verification") do
      v when v in @qualifications -> v
      "rollback_requested" when verdict == "credited_false_positive" -> "rollback_requested"
      _ -> "unqualified"
    end
  end

  defp claimed_outcome(row) do
    case Map.get(row, "outcome") do
      o when o in ["success", "failure", "false_positive"] -> o
      _ -> "unknown"
    end
  end

  # Only a non-negative integer is an observation; anything else is absent and is
  # named in missing_evidence.
  defp observations(row, kind) do
    hits =
      for key <- ~w(after_hits before_hits exit_code),
          value = Map.get(row, key),
          is_integer(value) and value >= 0,
          into: %{},
          do: {key, Integer.to_string(value)}

    base =
      case Map.get(row, "verdict") do
        v when is_binary(v) -> Map.put(hits, "verdict", ascii(v))
        _ -> hits
      end

    case {kind, Map.get(row, "verification")} do
      {:local_rescan, v} when v in @rescans -> Map.put(base, "rescan", v)
      _ -> base
    end
  end

  defp producer(row, :fleet_import), do: to_text(Map.get(row, "source_bot", "unknown"))
  defp producer(_row, _kind), do: "hypatia"

  defp event_id_source(row) do
    if Map.get(row, "event_id_source") == "producer", do: "producer", else: "hypatia"
  end

  defp to_text(value) when is_binary(value), do: value
  defp to_text(nil), do: ""
  defp to_text(value) when is_atom(value), do: Atom.to_string(value)
  defp to_text(value), do: inspect(value)

  # Printable ASCII stays; every other byte, and "%", becomes %XX.
  @doc false
  def ascii(text) when is_binary(text) do
    for <<byte <- text>>, into: "" do
      if byte >= 0x20 and byte <= 0x7E and byte != ?% do
        <<byte>>
      else
        "%" <> String.upcase(Base.encode16(<<byte>>))
      end
    end
  end

  # ── Encoding ───────────────────────────────────────────────────────────

  @doc """
  The canonical line of a receipt (no final LF): keys sorted, no whitespace, ASCII
  only, no numbers. `{:error, reason}` for a value that is not allowed.
  """
  @spec encode(map()) :: {:ok, binary()} | {:error, term()}
  def encode(value) do
    {:ok, encode!(value)}
  catch
    {:not_canonical, reason} -> {:error, {:not_canonical, reason}}
  end

  defp encode!(map) when is_map(map) do
    body =
      map
      |> Enum.map(fn {k, v} when is_binary(k) -> {k, encode!(v)} end)
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map_join(",", fn {k, v} -> string!(k) <> ":" <> v end)

    "{" <> body <> "}"
  end

  defp encode!(list) when is_list(list), do: "[" <> Enum.map_join(list, ",", &encode!/1) <> "]"
  defp encode!(text) when is_binary(text), do: string!(text)
  defp encode!(other), do: throw({:not_canonical, {:not_a_string_array_or_object, other}})

  defp string!(text) do
    escaped =
      for <<byte <- text>>, into: "" do
        case byte do
          ?" ->
            "\\\""

          ?\\ ->
            "\\\\"

          ?\b ->
            "\\b"

          ?\t ->
            "\\t"

          ?\n ->
            "\\n"

          ?\f ->
            "\\f"

          ?\r ->
            "\\r"

          b when b < 0x20 ->
            "\\u" <> String.downcase(String.pad_leading(Integer.to_string(b, 16), 4, "0"))

          b when b > 0x7E ->
            throw({:not_canonical, {:not_ascii, text}})

          b ->
            <<b>>
        end
      end

    "\"" <> escaped <> "\""
  end

  defp digest(body_without_digest) do
    with {:ok, line} <- encode(body_without_digest) do
      {:ok, :crypto.hash(:sha256, line) |> Base.encode16(case: :lower)}
    end
  end

  # ── Storage ────────────────────────────────────────────────────────────

  @doc "The receipt file of a month: `receipts/YYYY-MM.jsonl` under the data root."
  def path(data_root, %Date{year: year, month: month}) do
    month = String.pad_leading(Integer.to_string(month), 2, "0")
    Path.join([data_root, "receipts", "#{year}-#{month}.jsonl"])
  end

  @doc "Does a receipt for this `event_id` exist under the data root?"
  @spec exists?(String.t(), String.t()) :: boolean()
  def exists?(data_root, event_id) do
    needle = ~s("event_id":"#{event_id}")

    data_root
    |> receipt_files()
    |> Enum.any?(fn file ->
      file
      |> File.stream!()
      |> Enum.any?(fn line ->
        String.contains?(line, needle) and
          match?({:ok, %{"event_id" => ^event_id}}, Jason.decode(line))
      end)
    end)
  end

  @doc "All receipts under the data root, oldest file first."
  def all(data_root) do
    for file <- receipt_files(data_root),
        line <- file |> File.read!() |> String.split("\n", trim: true),
        do: Jason.decode!(line)
  end

  defp receipt_files(data_root) do
    dir = Path.join(data_root, "receipts")

    case File.ls(dir) do
      {:ok, names} ->
        names
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.sort()
        |> Enum.map(&Path.join(dir, &1))

      {:error, _} ->
        []
    end
  end

  @doc """
  Write the receipt of a stored row, once. A second call for the same `event_id`
  writes nothing and returns `:ok` (a replay writes no second receipt).

  `row` is the row as the log holds it. For a row of the local outcome log the row
  is looked up by its `event_id` (`{:error, :row_not_in_log}` if it is not there:
  a receipt is never written before its row). `opts` may carry `:ctx` for a source
  that is not in the log (the neural path).
  """
  @spec write(String.t(), map(), keyword()) :: :ok | {:error, term()}
  def write(data_root, row, opts \\ []) when is_binary(data_root) and is_map(row) do
    with {:ok, event_id} <- fetch_event_id(row),
         false <- exists?(data_root, event_id),
         {:ok, ctx} <- context(data_root, row, opts),
         receipt = build(row, ctx),
         {:ok, line} <- encode(receipt) do
      append(data_root, line)
    else
      true -> :ok
      {:error, _reason} = error -> error
    end
  end

  defp fetch_event_id(row) do
    case Map.get(row, "event_id") do
      id when is_binary(id) ->
        if EventId.valid?(id), do: {:ok, id}, else: {:error, :invalid_event_id}

      _ ->
        {:error, :no_event_id}
    end
  end

  defp append(data_root, line) do
    path = path(data_root, Date.utc_today())

    with :ok <- File.mkdir_p(Path.dirname(path)) do
      File.write(path, line <> "\n", [:append, :utf8])
    end
  end

  # ── Context: where the evidence sits ───────────────────────────────────

  defp context(data_root, row, opts) do
    case Keyword.fetch(opts, :ctx) do
      {:ok, ctx} -> {:ok, ctx}
      :error -> context_from_row(data_root, row)
    end
  end

  defp context_from_row(data_root, row) do
    case source_kind(row) do
      :fleet_import ->
        {:ok,
         %{
           kind: :fleet_import,
           log: @fleet_log,
           line: decimal(Map.get(row, "source_line")),
           row_sha256: digest_or_row(row, data_root)
         }}

      kind ->
        case locate(data_root, Map.fetch!(row, "event_id")) do
          {:ok, log, line, bytes} ->
            {:ok, %{kind: kind, log: log, line: Integer.to_string(line), row_sha256: sha(bytes)}}

          :error ->
            {:error, :row_not_in_log}
        end
    end
  end

  defp source_kind(row) do
    case Map.get(row, "source_kind") do
      "fleet_import" -> :fleet_import
      "local_rescan" -> :local_rescan
      "neural_path" -> :neural_path
      "local_caller" -> :local_caller
      _ -> if Map.get(row, "origin") == "gitbot-fleet", do: :fleet_import, else: :local_caller
    end
  end

  defp decimal(value) when is_binary(value) do
    if Regex.match?(~r/\A[0-9]+\z/, value), do: value, else: "0"
  end

  defp decimal(_), do: "0"

  # The digest of the SOURCE row if the import recorded it; else of the stored row.
  defp digest_or_row(row, data_root) do
    case Map.get(row, "source_row_sha256") do
      d when is_binary(d) and byte_size(d) == 64 ->
        d

      _ ->
        case locate(data_root, Map.fetch!(row, "event_id")) do
          {:ok, _log, _line, bytes} -> sha(bytes)
          :error -> sha(Jason.encode!(row))
        end
    end
  end

  defp sha(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)

  # Find the line of the outcome log that holds this event: {relative log path,
  # line number, exact line bytes}.
  @doc false
  def locate(data_root, event_id) do
    dir = Path.join(data_root, "outcomes")
    needle = ~s("event_id":"#{event_id}")

    case File.ls(dir) do
      {:ok, names} ->
        names
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.sort()
        |> Enum.find_value(:error, fn name ->
          dir
          |> Path.join(name)
          |> File.stream!()
          |> Stream.with_index(1)
          |> Enum.find_value(fn {line, number} ->
            line = String.trim_trailing(line, "\n")
            if String.contains?(line, needle), do: {:ok, "outcomes/" <> name, number, line}
          end)
        end)

      {:error, _} ->
        :error
    end
  end

  # ── Repair ─────────────────────────────────────────────────────────────

  @doc """
  Write the receipt of every row of a recipe that has an `event_id` and no receipt.
  Rows with no `event_id` (written before E0-09) get none: nothing is back-filled.
  Returns `{written, failed}`. A receipt whose `event_id` has no row in the log is
  not touched; `orphans/1` reports it.
  """
  @spec repair(String.t(), String.t()) :: {non_neg_integer(), non_neg_integer()}
  def repair(data_root, recipe_id) do
    rows_of_recipe(data_root, recipe_id)
    |> Enum.reduce({0, 0}, fn row, {written, failed} ->
      id = Map.get(row, "event_id")

      cond do
        not is_binary(id) ->
          {written, failed}

        exists?(data_root, id) ->
          {written, failed}

        true ->
          case write(data_root, row) do
            :ok ->
              {written + 1, failed}

            {:error, reason} ->
              Logger.warning("receipt_pending event_id=#{id} reason=#{inspect(reason)}")
              {written, failed + 1}
          end
      end
    end)
  end

  defp rows_of_recipe(data_root, recipe_id) do
    dir = Path.join(data_root, "outcomes")

    case File.ls(dir) do
      {:ok, names} ->
        for name <- names |> Enum.filter(&String.ends_with?(&1, ".jsonl")) |> Enum.sort(),
            line <- dir |> Path.join(name) |> File.stream!(),
            String.contains?(line, recipe_id),
            {:ok, %{"recipe_id" => ^recipe_id} = row} <- [
              Jason.decode(String.trim_trailing(line, "\n"))
            ],
            do: row

      {:error, _} ->
        []
    end
  end

  @doc "Event ids that have a receipt and no row in the outcome log (corrupt, reported loudly)."
  @spec orphans(String.t()) :: [String.t()]
  def orphans(data_root) do
    for %{"event_id" => id, "source" => %{"kind" => kind}} <- all(data_root),
        kind in ["local_rescan", "local_caller", "fleet_import"],
        locate(data_root, id) == :error,
        do: id
  end
end

# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Neural.SourceStore do
  @moduledoc """
  Source-store lifecycle checks for in-memory neural state.

  The identity is the expanded configured data-root pathname. It is not a
  content digest, an attestation, or an identity for an in-place replacement
  of that directory. A process keeps the identity captured at initialization.
  An observed root change blocks that lifecycle until the process is restarted.
  A runtime peer process outage refuses the operation without qualifying a
  mismatched source or permanently blocking an already initialized lifecycle.

  Snapshot labels preserve this binding; they do not authenticate filesystem
  contents or promote diagnostic trust-score snapshots to model authority.
  """

  defmodule Refusal do
    defexception [:reason]
    def message(error), do: "neural source refused: #{inspect(error.reason)}"
  end

  require Logger

  def current, do: Hypatia.Paths.verisimdb_data() |> Path.expand()

  def fields do
    %{source_store: current(), source_status: :ready, source_error: nil}
  end

  def check(state, requested) do
    source = Map.get(state, :source_store)
    current = current()

    cond do
      not is_binary(source) or source == "" or Path.expand(source) != source ->
        {:error, :unbound_source_store}

      Map.get(state, :source_status) != :ready ->
        {:error, {:source_lifecycle_blocked, Map.get(state, :source_error)}}

      source != current ->
        {:error, {:source_store_changed, source, current}}

      requested != source ->
        {:error, {:request_source_mismatch, requested, source}}

      true ->
        :ok
    end
  end

  def observe(state) do
    case check(state, Map.get(state, :source_store)) do
      :ok -> state
      {:error, reason} -> refuse(state, reason)
    end
  end

  def refuse(state, reason) do
    if Map.get(state, :source_status) == :blocked do
      state
    else
      status = if lifecycle_error?(reason), do: :blocked, else: Map.get(state, :source_status)
      state |> Map.put(:source_status, status) |> Map.put(:source_error, reason)
    end
  end

  defp lifecycle_error?(:unbound_source_store), do: true
  defp lifecycle_error?({:source_store_changed, _, _}), do: true
  # A live lifecycle may retry after a peer restarts or its call times out.
  # This exception does not qualify a foreign, unbound or malformed snapshot.
  defp lifecycle_error?({:cohort_source_mismatch, _, {:process_unavailable, _}}), do: false
  defp lifecycle_error?({:cohort_source_mismatch, _, _}), do: true
  defp lifecycle_error?({:source_identity_changed, _, _}), do: true
  defp lifecycle_error?({:source_lifecycle_blocked, _}), do: true
  defp lifecycle_error?(_), do: false

  @doc """
  Run one operation with entry and exit checks, retaining original model state
  on refusal. The operation must resolve its reads/writes against the captured
  source; these checks alone cannot pin filesystem reads.
  """
  def transact(state, requested, operation) when is_function(operation, 0) do
    with :ok <- check(state, requested) do
      try do
        {reply, updated} = operation.()

        case reply do
          {:error, reason} -> raise Refusal, reason: reason
          _ -> :ok
        end

        if Map.get(updated, :source_store) != Map.get(state, :source_store) do
          raise Refusal,
            reason:
              {:source_identity_changed, Map.get(state, :source_store),
               Map.get(updated, :source_store)}
        end

        case check(updated, requested) do
          :ok -> {:ok, reply, Map.put(updated, :source_error, nil)}
          {:error, reason} -> {:error, reason, refuse(state, reason)}
        end
      rescue
        error in Refusal ->
          {:error, error.reason, refuse(state, error.reason)}

        error ->
          reason = {:neural_operation_failed, Exception.message(error)}
          {:error, reason, refuse(state, reason)}
      catch
        :exit, reason ->
          failure = {:neural_process_unavailable, reason}
          {:error, failure, refuse(state, failure)}
      end
    else
      {:error, reason} -> {:error, reason, refuse(state, reason)}
    end
  end

  def reply(state, requested, operation) do
    case transact(state, requested, operation) do
      {:ok, result, updated} -> {:reply, result, updated}
      {:error, reason, refused} ->
        Logger.warning("Neural source refused: #{inspect(reason)}")
        {:reply, {:error, reason}, refused}
    end
  end

  def noreply(state, requested, operation) do
    case transact(state, requested, operation) do
      {:ok, _result, updated} -> {:noreply, updated}
      {:error, reason, refused} ->
        Logger.warning("Neural source refused: #{inspect(reason)}")
        {:noreply, refused}
    end
  end

  def accept!(:ok), do: :ok
  def accept!({:ok, value}), do: value
  def accept!(%{} = value), do: value
  def accept!({:error, reason}), do: raise(Refusal, reason: reason)
  def accept!(other), do: raise(Refusal, reason: {:unexpected_neural_reply, other})

  def call!(server, message) do
    server
    |> GenServer.call(message)
    |> accept!()
  end

  def tag(snapshot, state) do
    Map.merge(snapshot, %{
      "source_schema" => 1,
      "source_store" => state.source_store,
      "source_status" => Atom.to_string(state.source_status)
    })
  end

  def snapshot_matches?(snapshot, source) when is_map(snapshot) do
    snapshot["source_schema"] == 1 and snapshot["source_store"] == source and
      snapshot["source_status"] == "ready"
  end

  def snapshot_matches?(_, _), do: false

  @doc "Whether a returned report is bound to the current ready source."
  def ready_report?(%{source_store: source, source_status: :ready})
      when is_binary(source) do
    source == current()
  end

  def ready_report?(_), do: false

  def diagnostic(state) do
    %{
      source_store: Map.get(state, :source_store),
      current_store: current(),
      source_status: Map.get(state, :source_status),
      source_error: Map.get(state, :source_error),
      restart_required: Map.get(state, :source_status) == :blocked
    }
  end
end

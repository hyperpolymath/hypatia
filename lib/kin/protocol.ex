# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Kin.Protocol do
  @moduledoc """
  Kin Protocol — the coordination contract between Hypatia ecosystem components.

  Each kin sibling (panic-attacker, gitbot-fleet, auto-fix pipeline) reports its
  health via heartbeat files in `~/.hypatia/kin/`. Hypatia's Kin.Coordinator reads
  these heartbeats to maintain awareness of the whole system and coordinate responses.

  ## Heartbeat Schema

      %{
        "kin_id"    => "panic-attacker",
        "role"      => "scanner",
        "timestamp" => "2026-03-06T15:30:00Z",
        "status"    => "healthy" | "degraded" | "error",
        "version"   => "2.1.0",
        "last_run"  => %{ ... run-specific metrics ... },
        "errors"    => [],
        "capabilities" => ["scan", "assail", "assemblyline", "sarif"]
      }

  ## Kin Manifest Schema (kin.json in each repo root)

      %{
        "kin_id"       => "panic-attacker",
        "role"         => "scanner",
        "capabilities" => ["scan", "assail", "assemblyline"],
        "depends_on"   => [],
        "depended_by"  => ["hypatia", "gitbot-fleet"],
        "health_check" => "panic-attack diagnose --json",
        "heartbeat_path" => "~/.hypatia/kin/panic-attacker.heartbeat.json"
      }
  """

  @kin_dir Path.expand("~/.hypatia/kin")
  @stale_threshold_seconds 24 * 60 * 60  # 24 hours for CLI tools
  @critical_stale_seconds 7 * 24 * 60 * 60  # 7 days = definitely dead

  @known_kin %{
    "hypatia" => %{
      role: :coordinator,
      capabilities: [:neural, :triangle, :dispatch, :learning, :vcl],
      health_check: :internal
    },
    "panic-attacker" => %{
      role: :scanner,
      capabilities: [:scan, :assail, :assemblyline, :sarif, :kanren],
      health_check: "panic-attack diagnose --json"
    },
    "gitbot-fleet" => %{
      role: :executor,
      capabilities: [:dispatch, :pr_create, :commit, :review],
      health_check: nil
    },
    "auto-fix" => %{
      role: :fixer,
      capabilities: [:formulaic_fix, :spdx_fix, :shell_quote_fix],
      health_check: nil
    }
  }

  def kin_dir, do: @kin_dir
  def stale_threshold, do: @stale_threshold_seconds
  def critical_stale_threshold, do: @critical_stale_seconds
  def known_kin, do: @known_kin

  @doc "Path to a sibling's heartbeat file."
  def heartbeat_path(kin_id) do
    Path.join(@kin_dir, "#{kin_id}.heartbeat.json")
  end

  @doc "Read and parse a sibling's heartbeat."
  def read_heartbeat(kin_id) do
    path = heartbeat_path(kin_id)

    with {:ok, content} <- File.read(path),
         {:ok, data} <- Jason.decode(content) do
      {:ok, data}
    else
      {:error, :enoent} -> {:error, :no_heartbeat}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Write hypatia's own heartbeat."
  def write_heartbeat(status_map) do
    File.mkdir_p!(@kin_dir)
    path = heartbeat_path("hypatia")

    heartbeat = Map.merge(status_map, %{
      "kin_id" => "hypatia",
      "role" => "coordinator",
      "timestamp" => DateTime.to_iso8601(DateTime.utc_now()),
      "version" => Mix.Project.config()[:version] || "0.1.0",
      "capabilities" => ["neural", "triangle", "dispatch", "learning", "vcl"]
    })

    case Jason.encode(heartbeat, pretty: true) do
      {:ok, json} -> File.write!(path, json <> "\n")
      {:error, _} -> :ok
    end
  end

  @doc "Check if a heartbeat is stale (older than threshold)."
  def stale?(heartbeat_data) do
    case Map.get(heartbeat_data, "timestamp") do
      nil -> true
      ts ->
        case DateTime.from_iso8601(ts) do
          {:ok, dt, _} ->
            age = DateTime.diff(DateTime.utc_now(), dt, :second)
            age > @stale_threshold_seconds
          _ -> true
        end
    end
  end

  @doc "Check if a heartbeat is critically stale (7+ days)."
  def critically_stale?(heartbeat_data) do
    case Map.get(heartbeat_data, "timestamp") do
      nil -> true
      ts ->
        case DateTime.from_iso8601(ts) do
          {:ok, dt, _} ->
            age = DateTime.diff(DateTime.utc_now(), dt, :second)
            age > @critical_stale_seconds
          _ -> true
        end
    end
  end

  @doc "Age of heartbeat in human-readable form."
  def heartbeat_age(heartbeat_data) do
    case Map.get(heartbeat_data, "timestamp") do
      nil -> "never"
      ts ->
        case DateTime.from_iso8601(ts) do
          {:ok, dt, _} ->
            age = DateTime.diff(DateTime.utc_now(), dt, :second)
            cond do
              age < 60 -> "#{age}s ago"
              age < 3600 -> "#{div(age, 60)}m ago"
              age < 86400 -> "#{div(age, 3600)}h ago"
              true -> "#{div(age, 86400)}d ago"
            end
          _ -> "unknown"
        end
    end
  end
end

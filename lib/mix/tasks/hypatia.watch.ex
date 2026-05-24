# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.Watch do
  @moduledoc """
  Terminal dashboard for the live `Hypatia.Watcher` state.

  Refreshes every `--interval` seconds (default 2). No external deps —
  uses IO.ANSI for cursor positioning, so it works over plain SSH
  without any TUI library, terminfo, or curses dependency.

  Two operating modes:

    Local mode (default)   talks to a Watcher GenServer in THIS BEAM.
                          Useful for `iex -S mix` + `mix hypatia.watch`
                          in another terminal that shares the node.

    Remote mode (--url)    polls a running Hypatia's HTTP /api/status
                          endpoint. The default URL is
                          http://127.0.0.1:9090/api/status (loopback
                          only — to watch a different host, set up an
                          SSH tunnel first).

  Press Ctrl+C to exit (the terminal will be reset to a clean state by
  the alternate-screen restore).

  ## Options

      --interval SECONDS   refresh rate (default 2)
      --url URL            poll the /api/status endpoint here
                          instead of attaching to a local Watcher
      --once              render once and exit (good for cron / scripts)
      --plain             disable ANSI cursor positioning (logs append
                          rather than refresh-in-place — for CI logs
                          or anywhere you'd pipe the output)

  ## Examples

      mix hypatia.watch                       # local Watcher, refresh 2s
      mix hypatia.watch --interval 5          # slower refresh
      mix hypatia.watch --url http://localhost:9090/api/status
      mix hypatia.watch --once --plain        # one-shot dump for logs
  """

  use Mix.Task

  @shortdoc "Live terminal dashboard for the Hypatia Watcher"

  @switches [
    interval: :integer,
    url: :string,
    once: :boolean,
    plain: :boolean
  ]

  @impl Mix.Task
  def run(argv) do
    Mix.Task.run("app.start")

    {opts, _, _} = OptionParser.parse(argv, switches: @switches)

    interval_ms = Keyword.get(opts, :interval, 2) * 1000
    url = Keyword.get(opts, :url)
    plain? = Keyword.get(opts, :plain, false)
    once? = Keyword.get(opts, :once, false)

    fetch = fetch_fn(url)

    if once? do
      render(fetch.(), plain?, header_only: true)
    else
      if not plain?, do: IO.write([IO.ANSI.clear(), IO.ANSI.cursor(1, 1)])
      loop(fetch, interval_ms, plain?)
    end
  end

  defp loop(fetch, interval_ms, plain?) do
    snapshot = fetch.()
    render(snapshot, plain?)
    Process.sleep(interval_ms)
    loop(fetch, interval_ms, plain?)
  end

  defp fetch_fn(nil) do
    fn ->
      case Hypatia.Watcher.snapshot() do
        %{status: :unavailable} -> :unavailable
        snap -> snap
      end
    end
  end

  defp fetch_fn(url) do
    fn ->
      case System.cmd("curl", ["-sf", "--max-time", "3", url], stderr_to_stdout: true) do
        {body, 0} ->
          case Jason.decode(body) do
            {:ok, snap} -> snap
            _ -> :unavailable
          end

        _ ->
          :unavailable
      end
    end
  end

  @doc """
  Render a snapshot to stdout. Public so the unit test can call it
  directly without going through the run-loop + Mix.Task plumbing.
  Set `plain? = true` to suppress ANSI cursor positioning (append-style
  output suitable for piping to a log).
  """
  def render(snap, plain?, opts \\ [])

  def render(:unavailable, plain?, _opts) do
    if not plain?, do: IO.write([IO.ANSI.clear(), IO.ANSI.cursor(1, 1)])

    IO.puts(
      "[hypatia.watch] Watcher unavailable. Is Hypatia running? " <>
        "(Try `iex -S mix` or pass --url http://host:9090/api/status)"
    )
  end

  def render(snap, plain?, _opts) do
    if not plain?, do: IO.write([IO.ANSI.clear(), IO.ANSI.cursor(1, 1)])

    counts = snap[:counts] || snap["counts"] || %{}

    IO.puts(bold("Hypatia Watcher  ") <> dim("(refresh every 2s — Ctrl+C to exit)"))
    IO.puts(dim(String.duplicate("─", 78)))
    IO.puts(format_uptime(snap))
    IO.puts("")

    IO.puts(bold("Events / 5min"))
    render_counts(get_window(counts, :m5))
    IO.puts("")

    IO.puts(bold("Events / 1hr"))
    render_counts(get_window(counts, :h1))
    IO.puts("")

    IO.puts(bold("GenServer queue depths"))
    render_queue_depths(snap[:queue_depths] || snap["queue_depths"] || %{})
    IO.puts("")

    dropped = snap[:dropped_events] || snap["dropped_events"] || 0

    if dropped > 0 do
      IO.puts(red("⚠ Dropped #{dropped} telemetry event(s) under load"))
    end

    IO.puts(dim("Last updated: #{snap[:generated_at] || snap["generated_at"]}"))
  end

  defp render_counts(counts) when map_size(counts) == 0 do
    IO.puts("  (no events)")
  end

  defp render_counts(counts) do
    counts
    |> Enum.sort_by(fn {_event, count} -> -count end)
    |> Enum.each(fn {event, count} ->
      IO.puts(
        "  " <> String.pad_trailing(format_event(event), 36) <> dim(Integer.to_string(count))
      )
    end)
  end

  defp render_queue_depths(map) when map_size(map) == 0 do
    IO.puts("  (no supervised processes visible)")
  end

  defp render_queue_depths(map) do
    map
    |> Enum.sort()
    |> Enum.each(fn {name, depth} ->
      depth_str =
        case depth do
          nil -> dim("—")
          n when n > 100 -> red(Integer.to_string(n))
          n when n > 10 -> yellow(Integer.to_string(n))
          n -> Integer.to_string(n)
        end

      IO.puts("  " <> String.pad_trailing(to_string(name), 40) <> depth_str)
    end)
  end

  defp get_window(counts, atom) do
    counts[atom] || counts[Atom.to_string(atom)] || %{}
  end

  defp format_event(event) when is_binary(event), do: event
  defp format_event(event) when is_list(event), do: Enum.join(event, ".")
  defp format_event(event), do: inspect(event)

  defp format_uptime(snap) do
    seconds = snap[:uptime_seconds] || snap["uptime_seconds"] || 0
    "Uptime: #{format_seconds(seconds)}"
  end

  defp format_seconds(s) when s < 60, do: "#{s}s"
  defp format_seconds(s) when s < 3600, do: "#{div(s, 60)}m #{rem(s, 60)}s"

  defp format_seconds(s) do
    "#{div(s, 3600)}h #{div(rem(s, 3600), 60)}m"
  end

  defp bold(text), do: IO.ANSI.bright() <> text <> IO.ANSI.normal()
  defp dim(text), do: IO.ANSI.faint() <> text <> IO.ANSI.normal()
  defp red(text), do: IO.ANSI.red() <> text <> IO.ANSI.reset()
  defp yellow(text), do: IO.ANSI.yellow() <> text <> IO.ANSI.reset()
end

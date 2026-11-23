# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.ReviewCollect do
  @moduledoc """
  Read-only collection of PR review observations from a logged-in gh session.

  Usage:

      mix hypatia.review_collect --repo OWNER/REPO --pr NUMBER

  Requires `gh auth login` (or a suitably scoped GitHub App token in
  `GH_TOKEN`/`GITHUB_TOKEN`). The `gh api` calls use GET only. The output
  reports coverage limits and admitted *assertions*, never approval.

  This does not write JSON or evidence receipts. Converting collected maps
  to durable artifacts requires the separately qualified I-JSON/JCS pipeline.
  """

  use Mix.Task

  alias Hypatia.ReviewGithubCollector

  @shortdoc "Collect non-authorising GitHub bot review observations for a PR"
  @switches [repo: :string, pr: :integer, page_budget: :integer]

  @impl Mix.Task
  def run(argv) do
    {opts, rest, invalid} = OptionParser.parse(argv, strict: @switches)

    if rest != [] or invalid != [] or is_nil(opts[:repo]) or is_nil(opts[:pr]) do
      Mix.raise("Usage: mix hypatia.review_collect --repo OWNER/REPO --pr NUMBER")
    end

    budget = if is_nil(opts[:page_budget]), do: [], else: [page_budget: opts[:page_budget]]

    case ReviewGithubCollector.collect(opts[:repo], opts[:pr], budget) do
      {:ok, report} ->
        # No raw bot body, token or unvalidated JSON emitted.
        IO.puts("Review intake: #{report["repository"]}##{report["pull_request"]}")
        IO.puts("Head: #{report["head_sha"]}")
        IO.puts("Coverage: #{report["coverage"]} (not a clean-review certificate)")
        IO.puts("Admitted assertions: #{length(report["observations"])}")
        IO.puts("Ignored source events: #{report["ignored_source_events"]}")

        Enum.each(report["observations"], fn observation ->
          source = observation["source"]

          IO.puts(
            "  #{source["event_id"]}: #{source["provider"]}/#{source["scope"]} " <>
              "#{observation["assessment"]} [unqualified, no merge authority]"
          )
        end)

      {:error, reason} ->
        Mix.shell().error("Review intake unavailable; not a clean result: #{inspect(reason)}")
        exit({:shutdown, 2})
    end
  end
end

# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.ReviewCheckCollect do
  @moduledoc """
  Read-only summary of GitHub Check Runs emitted by pre-commit.ci.

      mix hypatia.review_check_collect --repo OWNER/REPO --pr NUMBER

  Requires authenticated read access to GitHub through gh CLI. There is no
  check-run re-request, auto-fix, bot dispatch, issue comment, learning
  event, merge approval, or receipt write. Missing provider events are
  UNKNOWN, never a clean review.
  """

  use Mix.Task

  alias Hypatia.ReviewCheckRunCollector

  @shortdoc "Read one PR's pre-commit.ci check-run assertions (not merge approval)"
  @switches [repo: :string, pr: :integer, page_budget: :integer]

  @impl Mix.Task
  def run(argv) do
    {opts, rest, invalid} = OptionParser.parse(argv, strict: @switches)

    if rest != [] or invalid != [] or is_nil(opts[:repo]) or is_nil(opts[:pr]) do
      Mix.raise("Usage: mix hypatia.review_check_collect --repo OWNER/REPO --pr NUMBER")
    end

    budget = if is_nil(opts[:page_budget]), do: [], else: [page_budget: opts[:page_budget]]

    case ReviewCheckRunCollector.collect(opts[:repo], opts[:pr], budget) do
      {:ok, report} ->
        IO.puts("External Check Run intake: #{report["repository"]}##{report["pull_request"]}")
        IO.puts("Head: #{report["head_sha"]}")
        IO.puts("Provider: pre-commit.ci")
        IO.puts("Matching current-head PR checks: #{report["matching_checks"]}")
        IO.puts("Coverage verdict: #{report["coverage_verdict"]} (not a clean certificate)")

        if not report["provider_observed"] do
          IO.puts("State: NOT OBSERVED; not proof of pass, absence, or installation")
        end

        Enum.each(report["observations"], fn item ->
          IO.puts(
            "  #{item["source"]["event_id"]}: " <>
              "status=#{item["check_status"]} conclusion=#{item["check_conclusion"]} " <>
              "[external assertion, unqualified, no merge authority]"
          )
        end)

      {:error, reason} ->
        Mix.shell().error("Check Run intake unavailable; not a clean result: #{inspect(reason)}")
        exit({:shutdown, 2})
    end
  end
end

# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ReviewObservation do
  @moduledoc """
  Admission boundary for *external* pull-request review observations.

  The input is a structured snapshot supplied by a future GitHub collector,
  not an instruction extracted from a comment. This module makes no API calls,
  executes no suggested patch, and grants no proof, approval, dispatch tier,
  Bayesian credit or merge authority.

  A completed reviewer reporting zero findings is a report about that
  reviewer's scope at one commit, not a verified absence of defects. A quota
  failure, missing finding summary or stale head is never a clean review.
  Source event identifiers must come from the producer/API, not from hashing
  equal review bodies (two attempts may have identical content).

  The result is an in-memory interchange map. Persisted JSON/JSONL must be
  independently validated and canonicalised via the estate ijson-jcs profile;
  Jason encoding alone is not a JCS receipt. Authenticity and finding
  verification belong to separate future gates.
  """

  @schema "hypatia.external-review-observation/1"
  @scopes %{
    "codex" => ["code", "security"],
    "coderabbit" => ["code"],
    "pre-commit.ci" => ["hooks"]
  }
  @statuses ~w(completed running failed quota_exhausted cancelled unavailable)
  @sha ~r/\A[0-9a-f]{40}\z/i
  @repo ~r/\A[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+\z/
  @event_id ~r/\A[A-Za-z0-9][A-Za-z0-9_.:\/-]{0,255}\z/

  @doc """
  Admit one structured external-review snapshot relative to the *current* PR
  head. Returns a non-authorising observation or a list of invalid fields.

  Required string-keyed input:
  provider, scope, repo, pr_number, reviewed_sha, source_event_id,
  source_url, status, has_findings (boolean or nil).
  """
  @spec admit(map(), String.t()) :: {:ok, map()} | {:error, {:invalid, [String.t()]}}
  def admit(row, current_head) when is_map(row) do
    errors = invalid_fields(row, current_head)

    if errors == [] do
      reviewed_sha = String.downcase(row["reviewed_sha"])
      head = String.downcase(current_head)
      same_head? = reviewed_sha == head
      status = row["status"]
      signal = row["has_findings"]

      {:ok,
       %{
         "schema" => @schema,
         "subject" => %{
           "repo" => row["repo"],
           "pull_request" => row["pr_number"],
           "reviewed_sha" => reviewed_sha,
           "current_head_sha" => head
         },
         "source" => %{
           "provider" => row["provider"],
           "scope" => row["scope"],
           "event_id" => row["source_event_id"],
           "url" => row["source_url"]
         },
         "review_status" => status,
         "finding_signal" => finding_signal(status, signal),
         "assessment" => assessment(status, signal, same_head?),
         "evidence_class" => "external_assertion",
         "authenticity" => "unverified",
         "qualification" => "unqualified",
         "learning_eligible" => false,
         "merge_approval" => false,
         "missing_evidence" => missing_evidence(status, signal, same_head?)
       }}
    else
      {:error, {:invalid, errors}}
    end
  end

  def admit(_row, _head), do: {:error, {:invalid, ["observation"]}}

  @doc "The accepted provider/scope combinations, not evidence that an app is installed."
  def supported_scopes, do: @scopes

  defp invalid_fields(row, current_head) do
    provider = row["provider"]
    repo = row["repo"]
    number = row["pr_number"]

    checks = [
      {"provider", is_binary(provider) and Map.has_key?(@scopes, provider)},
      {"scope", row["scope"] in Map.get(@scopes, provider, [])},
      {"repo", valid_repo?(repo)},
      {"pr_number", is_integer(number) and number > 0},
      {"reviewed_sha", valid_sha?(row["reviewed_sha"])},
      {"current_head_sha", valid_sha?(current_head)},
      {"source_event_id", valid_event_id?(row["source_event_id"])},
      {"source_url", valid_url?(row["source_url"], repo, number)},
      {"status", row["status"] in @statuses},
      {"has_findings",
       Map.has_key?(row, "has_findings") and
         (is_boolean(row["has_findings"]) or is_nil(row["has_findings"]))}
    ]

    for {field, false} <- checks, do: field
  end

  defp valid_sha?(sha), do: is_binary(sha) and Regex.match?(@sha, sha)

  defp valid_repo?(repo) when is_binary(repo) do
    Regex.match?(@repo, repo) and
      Enum.all?(String.split(repo, "/"), &(&1 not in [".", ".."]))
  end

  defp valid_repo?(_), do: false

  defp valid_event_id?(id), do: is_binary(id) and Regex.match?(@event_id, id)

  # The URL is a GitHub provenance locator, not an authenticity attestation.
  # Require the PR permalink path itself; review comment links use fragments.
  # This rejects malformed ports, PR suffix tricks and dot-segment traversal
  # (including encoded segments) without interpreting untrusted URL paths.
  defp valid_url?(url, repo, number)
       when is_binary(url) and is_binary(repo) and is_integer(number) and number > 0 do
    expected = String.downcase("/" <> repo <> "/pull/" <> Integer.to_string(number))

    case URI.new(url) do
      {:ok, uri} ->
        uri.scheme == "https" and uri.authority == "github.com" and
          String.downcase(uri.path || "") == expected

      {:error, _reason} ->
        false
    end
  end

  defp valid_url?(_url, _repo, _number), do: false

  # A failed reviewer's stale finding flag does not establish any finding state.
  defp finding_signal("completed", true), do: "reported_present"
  defp finding_signal("completed", false), do: "reported_absent"
  defp finding_signal(_status, _signal), do: "unknown"

  defp assessment(_status, _signal, false), do: "stale_head"
  defp assessment("quota_exhausted", _signal, true), do: "review_unavailable_quota"
  defp assessment("completed", true, true), do: "reported_findings"
  defp assessment("completed", false, true), do: "reported_no_findings"
  defp assessment("completed", nil, true), do: "finding_summary_missing"
  defp assessment("running", _signal, true), do: "review_pending"
  defp assessment(_status, _signal, true), do: "review_unavailable"

  defp missing_evidence(status, signal, same_head?) do
    ["producer_authentication", "independent_finding_verification"]
    |> maybe_missing(status != "completed", "completed_review")
    |> maybe_missing(not same_head?, "review_at_current_head")
    |> maybe_missing(status == "completed" and is_nil(signal), "finding_summary")
  end

  defp maybe_missing(items, true, item), do: items ++ [item]
  defp maybe_missing(items, false, _item), do: items
end

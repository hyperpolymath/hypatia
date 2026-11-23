# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ReviewEvidence do
  @moduledoc """
  Restricted, *non-authorising* evidence envelope for external review intake.

  This is the S-03 boundary after ReviewGithubCollector and ReviewObservation,
  not Hypatia's E0 learning receipt. It validates the admitted observation
  against ReviewObservation again and constructs a minimal ASCII/no-JSON-number
  body for Hypatia.Receipt.encode/1. No raw bot text, prompts, or patches enter.

  The E0 encoder's restricted profile is suitable for these deterministic
  bytes; nevertheless a future pinned ijson-jcs differential gate is REQUIRED
  before representing this output as a fully qualified estate JCS receipt.
  The SHA-256 is a content checksum, NOT a producer signature or proof.

  No file, API, bot, learning state or merge state is modified by this module.
  """

  alias Hypatia.Receipt
  alias Hypatia.ReviewObservation

  @schema "hypatia.external-review-evidence/1"
  @snapshot_keys Enum.sort(
                   ~w(collection coverage head_sha ignored_source_events observations pull_request repository source transport_provenance)
                 )
  @observation_keys Enum.sort(
                      ~w(assessment authenticity evidence_class finding_signal learning_eligible merge_approval missing_evidence qualification review_status schema source subject)
                    )
  @sha ~r/\A[0-9a-f]{40}\z/
  @event_id ~r/\A[A-Za-z0-9][A-Za-z0-9_.:\/-]{0,255}\z/

  @doc """
  Return a validated, deterministic **in-memory** envelope.

  collector_revision must be a lowercase 40-hex commit identifying the
  collector source. It does not establish that source was deployed or signed.
  """
  @spec build(map(), String.t()) :: {:ok, map()} | {:error, atom()}
  def build(snapshot, collector_revision)
      when is_map(snapshot) and is_binary(collector_revision) do
    with :ok <- check_snapshot(snapshot, collector_revision),
         {:ok, observations} <- check_observations(snapshot["observations"], snapshot) do
      {:ok,
       %{
         "schema" => @schema,
         "canonical_profile" => "ascii_no_json_numbers_subset/1",
         "collector_revision" => collector_revision,
         "collection" => %{
           "mode" => "repeated_stable_observation",
           "source" => "github_rest_via_gh",
           "coverage" => "bot_comment_and_review_submissions_only",
           "ignored_source_events" => Integer.to_string(snapshot["ignored_source_events"]),
           "observation_count" => Integer.to_string(length(observations))
         },
         "subject" => %{
           "repo" => snapshot["repository"],
           "pull_request" => Integer.to_string(snapshot["pull_request"]),
           "current_head_sha" => snapshot["head_sha"]
         },
         "assurance" => %{
           "source_authenticity" => "unverified",
           "learning_credit" => "none",
           "merge_authority" => "none",
           "coverage_verdict" => "not_established"
         },
         "observations" => observations
       }}
    end
  end

  def build(_snapshot, _collector_revision), do: {:error, :invalid_input}

  @doc """
  Produce a deterministic hash of the exact no-final-LF JSON bytes and a
  separate single-LF file record. Does not write to disk or sign anything.
  """
  @spec canonical(map(), String.t()) :: {:ok, map()} | {:error, term()}
  def canonical(snapshot, collector_revision) do
    with {:ok, envelope} <- build(snapshot, collector_revision),
         {:ok, bytes} <- Receipt.encode(envelope) do
      {:ok,
       %{
         "body" => envelope,
         "library_bytes" => bytes,
         "file_record_bytes" => bytes <> "\n",
         "library_sha256" =>
           :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower),
         "canonicaliser" => "Hypatia.Receipt.restricted_encoder",
         "qualification" => "requires_ijson_jcs_differential"
       }}
    end
  end

  defp check_snapshot(snapshot, revision) do
    cond do
      Enum.sort(Map.keys(snapshot)) != @snapshot_keys ->
        {:error, :unexpected_snapshot_fields}

      not Regex.match?(@sha, revision) ->
        {:error, :invalid_collector_revision}

      not (is_binary(snapshot["head_sha"]) and Regex.match?(@sha, snapshot["head_sha"])) ->
        {:error, :invalid_head_sha}

      not (is_binary(snapshot["repository"]) and
             Regex.match?(~r/\A[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+\z/, snapshot["repository"])) ->
        {:error, :invalid_repository}

      not (is_integer(snapshot["pull_request"]) and snapshot["pull_request"] > 0) ->
        {:error, :invalid_pull_request}

      not (is_integer(snapshot["ignored_source_events"]) and
             snapshot["ignored_source_events"] >= 0) ->
        {:error, :invalid_ignored_count}

      snapshot["collection"] != "repeated_stable_observation" ->
        {:error, :unsupported_collection}

      snapshot["source"] != "github_rest_via_gh" or
          snapshot["transport_provenance"] != "github_api_observed" or
          snapshot["coverage"] != "bot_comment_and_review_submissions_only" ->
        {:error, :unsupported_source}

      not is_list(snapshot["observations"]) ->
        {:error, :invalid_observations}

      true ->
        :ok
    end
  end

  defp check_observations(observations, snapshot) do
    with {:ok, records} <- check_each(observations, snapshot) do
      ids = Enum.map(records, &get_in(&1, ["source", "event_id"]))

      if Enum.uniq(ids) == ids do
        {:ok, Enum.sort_by(records, &get_in(&1, ["source", "event_id"]))}
      else
        {:error, :duplicate_source_event_id}
      end
    end
  end

  defp check_each(records, snapshot) do
    Enum.reduce_while(records, {:ok, []}, fn observation, {:ok, accepted} ->
      case validated_observation(observation, snapshot) do
        {:ok, record} -> {:cont, {:ok, [record | accepted]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, reversed} -> {:ok, Enum.reverse(reversed)}
      error -> error
    end
  end

  defp validated_observation(observation, snapshot) when is_map(observation) do
    source = observation["source"]
    subject = observation["subject"]

    cond do
      Enum.sort(Map.keys(observation)) not in
          [@observation_keys, Enum.sort(["provider_run_status" | @observation_keys])] ->
        {:error, :unexpected_observation_fields}

      not (is_map(source) and is_map(subject)) ->
        {:error, :invalid_observation_shape}

      Map.has_key?(observation, "provider_run_status") and
          observation["provider_run_status"] != "unknown" ->
        {:error, :unverified_provider_completion}

      not (is_binary(source["event_id"]) and Regex.match?(@event_id, source["event_id"])) ->
        {:error, :invalid_source_event_id}

      subject["repo"] != snapshot["repository"] or
          subject["pull_request"] != snapshot["pull_request"] or
          subject["current_head_sha"] != snapshot["head_sha"] ->
        {:error, :subject_mismatch}

      true ->
        revalidate(observation, snapshot)
    end
  end

  defp validated_observation(_invalid, _snapshot), do: {:error, :invalid_observation_shape}

  defp revalidate(observation, snapshot) do
    source = observation["source"]
    subject = observation["subject"]

    reported_findings =
      case observation["finding_signal"] do
        "reported_present" -> true
        "reported_absent" -> false
        "unknown" -> nil
        _ -> :invalid
      end

    input = %{
      "provider" => source["provider"],
      "scope" => source["scope"],
      "repo" => subject["repo"],
      "pr_number" => subject["pull_request"],
      "reviewed_sha" => subject["reviewed_sha"],
      "source_event_id" => source["event_id"],
      "source_url" => source["url"],
      "status" => observation["review_status"],
      "has_findings" => reported_findings
    }

    case ReviewObservation.admit(input, snapshot["head_sha"]) do
      {:ok, expected} ->
        if expected == Map.delete(observation, "provider_run_status") do
          {:ok,
           %{
             "source" => %{
               "provider" => source["provider"],
               "scope" => source["scope"],
               "event_id" => source["event_id"],
               "url" => source["url"]
             },
             "reviewed_sha" => subject["reviewed_sha"],
             "assessment" => observation["assessment"],
             "review_status" => observation["review_status"],
             "finding_signal" => observation["finding_signal"],
             "missing_evidence" => observation["missing_evidence"],
             "provider_run_status" => "unknown",
             "qualification" => "unqualified",
             "learning_credit" => "none",
             "merge_authority" => "none"
           }}
        else
          {:error, :observation_contract_mismatch}
        end

      {:error, _} ->
        {:error, :invalid_admitted_observation}
    end
  end
end

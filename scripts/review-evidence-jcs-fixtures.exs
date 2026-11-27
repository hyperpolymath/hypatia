# SPDX-License-Identifier: MPL-2.0
# Review evidence ASCII/JCS differential corpus. Writes fixtures only in the
# caller-supplied scratch directory; no GitHub API, bots or learning changes.

defmodule Hypatia.ReviewEvidenceJcsFixtures do
  alias Hypatia.ReviewEvidence
  alias Hypatia.ReviewObservation

  @repo "stuff-of-awe/awesome-zig"
  @current String.duplicate("a", 40)
  @historical String.duplicate("b", 40)
  @collector String.duplicate("c", 40)

  defp observation(id, provider, scope, reviewed_sha, status, findings) do
    row = %{
      "provider" => provider,
      "scope" => scope,
      "repo" => @repo,
      "pr_number" => 20,
      "reviewed_sha" => reviewed_sha,
      "source_event_id" => "github:review_comment:#{id}",
      "source_url" =>
        "https://github.com/stuff-of-awe/awesome-zig/pull/20#discussion_r#{id}",
      "status" => status,
      "has_findings" => findings
    }

    {:ok, admitted} = ReviewObservation.admit(row, @current)
    Map.put(admitted, "provider_run_status", "unknown")
  end

  defp snapshot(observations, ignored) do
    %{
      "repository" => @repo,
      "pull_request" => 20,
      "head_sha" => @current,
      "collection" => "repeated_stable_observation",
      "source" => "github_rest_via_gh",
      "transport_provenance" => "github_api_observed",
      "coverage" => "bot_comment_and_review_submissions_only",
      "ignored_source_events" => ignored,
      "observations" => observations
    }
  end

  def emit(directory) do
    File.mkdir_p!(directory)

    fixtures = [
      {"empty_unknown", snapshot([], 0)},
      {"current_code",
       snapshot([observation(101, "codex", "code", @current, "completed", true)], 0)},
      {"stale_historical",
       snapshot([observation(102, "coderabbit", "code", @historical, "completed", true)], 1)},
      {"security_scope",
       snapshot([observation(103, "codex", "security", @current, "completed", true)], 0)},
      {"unavailable_review",
       snapshot([observation(104, "codex", "security", @current, "unavailable", nil)], 2)},
      {"mixed_sorted",
       snapshot(
         [
           observation(107, "codex", "security", @current, "completed", true),
           observation(105, "coderabbit", "code", @current, "completed", true),
           observation(106, "codex", "code", @historical, "completed", true)
         ],
         3
       )}
    ]

    records =
      Enum.map(fixtures, fn {name, data} ->
        {:ok, result} = ReviewEvidence.canonical(data, @collector)
        body = result["body"]
        bytes = result["library_bytes"]
        file_record = result["file_record_bytes"]

        unless body["assurance"]["merge_authority"] == "none" and
                 body["assurance"]["learning_credit"] == "none" and
                 body["assurance"]["coverage_verdict"] == "not_established" and
                 result["qualification"] == "requires_ijson_jcs_differential" and
                 file_record == bytes <> "\n" do
          raise "source admission assurance changed"
        end

        File.write!(Path.join(directory, "library_#{name}.json"), bytes)
        File.write!(Path.join(directory, "estate_#{name}.json"), file_record)
        bytes
      end)

    [first, second | _] = records
    File.write!(Path.join(directory, "records_ok.jsonl"), first <> "\n" <> second <> "\n")
    File.write!(Path.join(directory, "records_missing_lf.jsonl"), first <> "\n" <> second)
    File.write!(Path.join(directory, "records_blank.jsonl"), first <> "\n\n" <> second <> "\n")
    File.write!(Path.join(directory, "records_not_sorted.jsonl"), "{ \"a\":1}\n")

    # These are intentionally *text* fixtures: parsing to a generic map first
    # would erase duplicate keys and corrupt the test of strict I-JSON parsing.
    negative = [
      {"duplicate_keys", ~S({"a":1,"a":2})},
      {"unsafe_integer", ~S({"x":9007199254740992})},
      {"lone_surrogate", ~S({"x":"\uD800"})},
      {"noncharacter", ~S({"x":"\uFDD0"})},
      {"malformed", ~S({"x":})}
    ]

    Enum.each(negative, fn {name, bytes} ->
      File.write!(Path.join(directory, "invalid_#{name}.json"), bytes)
    end)

    IO.puts("REVIEW_JCS_FIXTURES=#{length(fixtures)}")
  end
end

case System.argv() do
  [directory] ->
    Hypatia.ReviewEvidenceJcsFixtures.emit(directory)

  _ ->
    raise "usage: mix run --no-start scripts/review-evidence-jcs-fixtures.exs SCRATCH_DIR"
end

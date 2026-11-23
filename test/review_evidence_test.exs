# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.ReviewEvidenceTest do
  use ExUnit.Case, async: true

  alias Hypatia.ReviewEvidence
  alias Hypatia.ReviewObservation

  @repo "stuff-of-awe/awesome-zig"
  @sha String.duplicate("a", 40)
  @older String.duplicate("b", 40)
  @revision String.duplicate("c", 40)

  defp observation(id, opts \\ []) do
    {:ok, admitted} =
      ReviewObservation.admit(
        %{
          "provider" => Keyword.get(opts, :provider, "codex"),
          "scope" => Keyword.get(opts, :scope, "code"),
          "repo" => @repo,
          "pr_number" => 20,
          "reviewed_sha" => Keyword.get(opts, :reviewed_sha, @sha),
          "source_event_id" => "github:review_comment:#{id}",
          "source_url" => "https://github.com/stuff-of-awe/awesome-zig/pull/20#discussion_r#{id}",
          "status" => Keyword.get(opts, :status, "completed"),
          "has_findings" => Keyword.get(opts, :has_findings, true)
        },
        @sha
      )

    Map.put(admitted, "provider_run_status", "unknown")
  end

  defp snapshot(observations \\ [observation(301)]) do
    %{
      "repository" => @repo,
      "pull_request" => 20,
      "head_sha" => @sha,
      "collection" => "repeated_stable_observation",
      "source" => "github_rest_via_gh",
      "transport_provenance" => "github_api_observed",
      "coverage" => "bot_comment_and_review_submissions_only",
      "ignored_source_events" => 0,
      "observations" => observations
    }
  end

  test "encodes a non-authorising envelope without changing E0 receipt semantics" do
    assert {:ok, result} = ReviewEvidence.canonical(snapshot(), @revision)
    assert %{"schema" => "hypatia.external-review-evidence/1"} = result["body"]
    assert result["qualification"] == "requires_ijson_jcs_differential"
    assert result["body"]["assurance"]["merge_authority"] == "none"
    assert result["body"]["assurance"]["learning_credit"] == "none"
    assert result["body"]["assurance"]["coverage_verdict"] == "not_established"
    assert result["body"]["observations"] |> length() == 1

    bytes = result["library_bytes"]
    assert String.starts_with?(bytes, "{\"assurance\":")
    assert result["file_record_bytes"] == bytes <> "\n"
    refute String.ends_with?(bytes, "\n")
    assert byte_size(result["library_sha256"]) == 64
    assert result["library_sha256"] ==
             (:crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower))
    refute String.contains?(bytes, "Potential bad lock")
  end

  test "sorts source identities deterministically independently of API page order" do
    first = snapshot([observation(302), observation(301)])
    second = snapshot([observation(301), observation(302)])

    assert {:ok, a} = ReviewEvidence.canonical(first, @revision)
    assert {:ok, b} = ReviewEvidence.canonical(second, @revision)
    assert a["library_bytes"] == b["library_bytes"]

    assert Enum.map(a["body"]["observations"], &get_in(&1, ["source", "event_id"])) ==
             ["github:review_comment:301", "github:review_comment:302"]
  end

  test "empty observation collection is never a clean-review certificate" do
    assert {:ok, result} = ReviewEvidence.canonical(snapshot([]), @revision)
    assert result["body"]["collection"]["observation_count"] == "0"
    assert result["body"]["observations"] == []
    assert result["body"]["assurance"]["coverage_verdict"] == "not_established"
  end

  test "retains exact historical reviewed SHA and stale verdict" do
    historical = observation(303, reviewed_sha: @older)

    assert {:ok, result} = ReviewEvidence.build(snapshot([historical]), @revision)
    assert [record] = result["observations"]
    assert record["reviewed_sha"] == @older
    assert record["assessment"] == "stale_head"
  end

  test "rejects a manufactured merger approval or learning qualification" do
    for changed <- [
          Map.put(observation(301), "merge_approval", true),
          Map.put(observation(301), "learning_eligible", true),
          Map.put(observation(301), "qualification", "verified"),
          Map.put(observation(301), "evidence_class", "proof"),
          Map.put(observation(301), "assessment", "reported_no_findings")
        ] do
      assert {:error, :observation_contract_mismatch} =
               ReviewEvidence.build(snapshot([changed]), @revision)
    end
  end

  test "refuses unverified provider execution completion and unknown fields" do
    changed = Map.put(observation(301), "provider_run_status", "completed")

    assert {:error, :unverified_provider_completion} =
             ReviewEvidence.build(snapshot([changed]), @revision)

    injected = Map.put(observation(301), "fix_command", "delete all checks")

    assert {:error, :unexpected_observation_fields} =
             ReviewEvidence.build(snapshot([injected]), @revision)
  end

  test "refuses malformed or mismatched collector snapshots" do
    for broken <- [
          Map.put(snapshot(), "collection", "complete_bounded_snapshot"),
          Map.put(snapshot(), "source", "unknown"),
          Map.put(snapshot(), "transport_provenance", "unauthenticated"),
          Map.put(snapshot(), "head_sha", @older),
          Map.put(snapshot(), "ignored_source_events", -1),
          Map.put(snapshot(), "new_unverified_flag", true),
          Map.put(snapshot(), "observations", nil)
        ] do
      assert {:error, _} = ReviewEvidence.build(broken, @revision)
    end
  end

  test "refuses duplicate event IDs, malformed provenance and wrong subject" do
    assert {:error, :duplicate_source_event_id} =
             ReviewEvidence.build(snapshot([observation(301), observation(301)]), @revision)

    changed_source = put_in(observation(301), ["source", "url"], "https://evil.invalid")

    assert {:error, :invalid_admitted_observation} =
             ReviewEvidence.build(snapshot([changed_source]), @revision)

    changed_subject = put_in(observation(301), ["subject", "repo"], "other/thing")

    assert {:error, :subject_mismatch} =
             ReviewEvidence.build(snapshot([changed_subject]), @revision)
  end

  test "bad source revision cannot pose as an immutable collector identity" do
    assert {:error, :invalid_collector_revision} =
             ReviewEvidence.build(snapshot(), "moving-main")

    assert {:error, :invalid_collector_revision} =
             ReviewEvidence.build(snapshot(), String.upcase(@revision))

    assert {:error, :invalid_input} = ReviewEvidence.build(nil, @revision)
  end

  test "non-ASCII in the permitted source locator fails the restricted encoder" do
    changed =
      put_in(
        observation(301),
        ["source", "url"],
        "https://github.com/stuff-of-awe/awesome-zig/pull/20#caf\u00E9"
      )

    # If the upstream URI parser admits this fragment, the restricted output
    # still rejects Unicode rather than claiming the ASCII-only JCS subset.
    case ReviewEvidence.canonical(snapshot([changed]), @revision) do
      {:error, {:not_canonical, {:not_ascii, _}}} -> :ok
      {:error, :invalid_admitted_observation} -> :ok
      other -> flunk("Expected rejection of a non-ASCII locator, got #{inspect(other)}")
    end
  end
end

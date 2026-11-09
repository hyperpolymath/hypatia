# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.MergeOrchestration.TestObservationStore do
  @moduledoc false
  # Synthetic test evidence. These helpers do not claim a valid signature or a
  # real GitHub run; even matching approvals must remain review-only at runtime.
  @head String.duplicate("a", 40)
  @event "test:observation:1"
  @at "2026-11-04T00:00:00Z"

  def head, do: @head
  def event, do: @event
  def at, do: @at

  def observation(overrides \\ %{}) do
    files = Map.get(overrides, "files", ["Cargo.lock"])

    Map.merge(
      %{
        "schema_version" => 1,
        "complete" => true,
        "event_id" => @event,
        "observed_at" => @at,
        "repo" => "hyperpolymath/a",
        "number" => 1,
        "head_sha" => @head,
        "base_sha" => String.duplicate("b", 40),
        "branch" => "dependabot/cargo/x",
        "title" => "Bump x from 1 to 2",
        "files" => files,
        "labels" => [],
        "state" => "open",
        "changed_file_count" => length(files)
      },
      overrides
    )
  end

  def attestation(obs, overrides \\ %{}) do
    Map.merge(
      %{
        "subject" => Map.take(obs, ["repo", "number", "head_sha"]),
        "bot" => "ci",
        "verdict" => "approve",
        "confidence" => 0.99
      },
      overrides
    )
  end

  def seed(opts \\ []) do
    dir = Path.join(System.tmp_dir!(), "merge-store-#{System.unique_integer([:positive])}")
    encode = Keyword.get(opts, :encode, &Jason.encode!/1)
    observations = Keyword.get(opts, :observations, [observation()])
    for sub <- ~w(observations pools attestations leases), do: File.mkdir_p!(Path.join(dir, sub))

    write_batch(dir, observations, encode)

    for {obs, index} <- Enum.with_index(observations) do
      pool_file = String.replace(obs["repo"], "/", "__") <> ".json"
      File.write!(Path.join([dir, "pools", pool_file]), encode.(%{"pool" => "P2"}))
      File.write!(Path.join([dir, "attestations", "#{index}.json"]), encode.(attestation(obs)))
    end

    dir
  end

  def write_batch(dir, observations, encode \\ &Jason.encode!/1) do
    entries =
      Enum.map(observations, fn obs ->
        name = String.replace(obs["repo"], "/", "__") <> "__#{obs["number"]}.json"
        body = encode.(obs)
        File.write!(Path.join([dir, "observations", name]), body)

        Map.merge(Map.take(obs, ["repo", "number", "head_sha"]), %{
          "file" => name,
          "sha256" => :crypto.hash(:sha256, body) |> Base.encode16(case: :lower)
        })
      end)

    repos =
      if observations == [],
        do: ["hyperpolymath/a"],
        else: Enum.uniq(Enum.map(observations, & &1["repo"]))

    receipt = %{
      "schema_version" => 1,
      "status" => "complete",
      "event_id" => @event,
      "observed_at" => @at,
      "repos" => repos,
      "observations" => entries
    }

    File.write!(receipt_path(dir), encode.(receipt))
    receipt
  end

  def receipt_path(dir), do: Path.join([dir, "observations", ".observation-batch.json"])
end

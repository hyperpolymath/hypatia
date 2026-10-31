# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.MovedRepositories do
  @moduledoc """
  Versioned records of confirmed repository moves relevant to reusable workflows.

  Add records only after a move has occurred and its identity has been checked.
  The numeric GitHub repository ID is stable across a rename or transfer.
  Evidence records the observed workflow behaviour, not a promise that every
  possible caller or ref is otherwise valid.
  """

  @version 1
  @entries [
    %{
      former: "hyperpolymath/standards",
      current: "metadatastician/standards",
      repo_id: 1_116_521_501,
      moved: "2026-10-25",
      evidence: "https://github.com/hyperpolymath/hypatia/actions/runs/40424594888",
      additional_evidence: "https://github.com/hyperpolymath/hypatia/actions/runs/40424740139"
    }
  ]

  @doc "Registry schema version."
  def version, do: @version

  @doc "Confirmed moves."
  def entries, do: @entries
end

# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.BatonEmitter do
  @moduledoc """
  Containment boundary for the proposed Bag-of-Actions merge backend.

  The former spec emitted `gh pr merge` directly. Its `verifier` map only named
  the farm's gate: this module never executed that gate, bound the assessed head,
  or used its durable intent/uncertainty ledger. A non-nil map is not evidence of
  independent verification. The downstream executor is not qualified here.

  Until that actual contract exists, `to_spec/2` returns an explicit error and
  `emit/2` records refusals without invoking a submit function. The manifest
  backend remains available, dry-run farm actuation stays separate, and no
  alternate executor can silently bypass its checks.
  """

  @doc "Whether a qualified merge executor is available; performs no submission."
  def availability, do: {:error, :qualified_merge_executor_unavailable}

  @doc "No mutating spec can be constructed without a qualified merge executor."
  def to_spec(_decision, _lease \\ %{}), do: availability()

  @doc "Return explicit refusals for armed candidates; never submit a mutation."
  def emit(%{results: results}, _opts \\ []) do
    for %{gate: {:armed, lease}, decision: decision} <- results do
      %{pr: decision.pr, spec: nil, result: to_spec(decision, lease)}
    end
  end
end

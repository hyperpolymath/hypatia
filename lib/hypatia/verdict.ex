# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Verdict do
  @moduledoc """
  Every finding names its support (HYP-01).

  The JSON mirror of `src/Hypatia/ABI/Verdict.idr`, which is normative, and of
  `schema/findings.ncl`, the contract CI validates scan output against. Each
  finding carries three fields:

    * `support` — what its evidence supports:
      `%{kind: "pattern"}` (a rule matched),
      `%{kind: "observed", observations: n}` (a fleet count; rules.a2ml's
      thresholds are belief formation, not soundness), or
      `%{kind: "mechanical", ...}` (a prover re-check, with both digests);
    * `evidence` — the tokens the finding was issued on: rule, rule version,
      span, observation count;
    * `level` — derived here and nowhere else: `"sound"` only for a
      mechanical support whose re-check confirms; `"epi"` for any other
      support with evidence; `"belief"` with no evidence at all.

  Elixir rules carry the tag only. No rule is proved sound, so the honest
  default for every rule-module finding is `pattern` / `epi`. The only way to
  `"sound"` is `attach_recheck/3`, which refuses a re-check whose target is
  not the finding's file or whose target digest does not match the bytes on
  disk. It cannot re-run the prover; `scripts/recheck-lean-hole.sh` is the
  pattern for checking the artefact digest against a fresh run.
  """

  @rule_version Mix.Project.config()[:version]

  @provers ~w(lean4 idris2 agda coq)
  @results ~w(confirms refutes)

  @type level :: String.t()

  @doc "The rule version stamped on evidence tokens (the hypatia version)."
  @spec rule_version() :: String.t()
  def rule_version, do: @rule_version

  @doc """
  Attach `support`, `evidence` and the derived `level` to a finding.

  An existing `support` or `evidence` is kept; a `level` is always
  recomputed, so a rule cannot assert one.
  """
  @spec annotate(map()) :: map()
  def annotate(finding) when is_map(finding) do
    support = Map.get(finding, :support) || %{kind: "pattern"}
    evidence = Map.get(finding, :evidence) || [token(finding)]

    finding
    |> Map.put(:support, support)
    |> Map.put(:evidence, evidence)
    |> Map.put(:level, level(support, evidence))
  end

  @doc """
  The evidence token a rule-module finding offers: the rule that fired, at
  this hypatia version, over the finding's line (`0` when the finding is
  file- or repository-level). `observations` is this scan's own single
  observation; fleet-wide counts belong in an `observed` support.
  """
  @spec token(map()) :: map()
  def token(finding) do
    line =
      case Map.get(finding, :line) do
        n when is_integer(n) and n >= 0 -> n
        _ -> 0
      end

    %{
      rule: "#{Map.get(finding, :rule_module, "unknown")}/#{Map.get(finding, :type, "unknown")}",
      rule_version: @rule_version,
      span: %{file: to_string(Map.get(finding, :file, "")), start_line: line, end_line: line},
      observations: 1
    }
  end

  @doc """
  Derive the level from support and evidence (`Verdict.idr` `level`;
  `schema/findings.ncl` `derive_level`).
  """
  @spec level(map(), list()) :: level()
  def level(%{kind: "mechanical", result: "confirms"}, _evidence), do: "sound"
  def level(_support, [_ | _]), do: "epi"
  def level(_support, []), do: "belief"

  @doc """
  Replace a finding's support with a prover re-check, if the re-check is
  about this finding's file and its target digest matches the file under
  `repo_path`.

  `recheck` has string values for `:prover`, `:prover_version`, `:target`,
  `:target_sha256`, `:artefact_sha256` and `:result`. A re-check that
  `"refutes"` is attached as recorded and derives `"epi"`, not `"sound"`.
  """
  @spec attach_recheck(map(), map(), String.t()) :: {:ok, map()} | {:error, term()}
  def attach_recheck(finding, recheck, repo_path) do
    with :ok <- check_string(:target, recheck[:target]),
         :ok <- check_string(:prover_version, recheck[:prover_version]),
         :ok <- check_member(:prover, recheck[:prover], @provers),
         :ok <- check_member(:result, recheck[:result], @results),
         :ok <- check_digest(:target_sha256, recheck[:target_sha256]),
         :ok <- check_digest(:artefact_sha256, recheck[:artefact_sha256]),
         :ok <- check_same_target(finding, recheck),
         {:ok, digest} <- file_sha256(Path.join(repo_path, recheck.target)),
         :ok <- check_target_digest(digest, recheck.target_sha256) do
      support =
        recheck
        |> Map.take([:prover, :prover_version, :target, :target_sha256, :artefact_sha256, :result])
        |> Map.put(:kind, "mechanical")

      {:ok, finding |> Map.put(:support, support) |> annotate()}
    end
  end

  @doc """
  One-line rendering of a finding's level and support, for text and
  GitHub-annotation output. Derived from the record, never stored on it.
  """
  @spec describe(map()) :: String.t()
  def describe(finding) do
    support = Map.get(finding, :support) || %{kind: "pattern"}
    evidence = Map.get(finding, :evidence) || []

    basis =
      case support do
        %{kind: "mechanical"} = s -> "#{s.prover} #{s.prover_version} re-check #{s.result}"
        %{kind: "observed", observations: n} -> "observed #{n}x"
        %{kind: kind} -> to_string(kind)
      end

    "#{level(support, evidence)}: #{basis}"
  end

  @doc "Count findings by level, every level present (for the scan summary)."
  @spec level_counts([map()]) :: %{level() => non_neg_integer()}
  def level_counts(findings) do
    Enum.reduce(findings, %{"sound" => 0, "epi" => 0, "belief" => 0}, fn f, acc ->
      Map.update(acc, Map.get(f, :level, "belief"), 1, &(&1 + 1))
    end)
  end

  defp check_string(field, value) do
    if is_binary(value) and value != "", do: :ok, else: {:error, {:invalid, field, value}}
  end

  defp check_member(field, value, allowed) do
    if value in allowed, do: :ok, else: {:error, {:invalid, field, value}}
  end

  defp check_digest(field, value) do
    if is_binary(value) and value =~ ~r/\A[0-9a-f]{64}\z/,
      do: :ok,
      else: {:error, {:invalid, field, value}}
  end

  defp check_same_target(finding, recheck) do
    if Path.expand(to_string(Map.get(finding, :file, "")), "/") ==
         Path.expand(recheck.target, "/"),
       do: :ok,
       else: {:error, {:target_mismatch, Map.get(finding, :file), recheck.target}}
  end

  defp file_sha256(path) do
    case File.read(path) do
      {:ok, bytes} -> {:ok, :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)}
      {:error, reason} -> {:error, {:unreadable_target, path, reason}}
    end
  end

  defp check_target_digest(actual, recorded) do
    if actual == recorded,
      do: :ok,
      else: {:error, {:target_digest_mismatch, actual, recorded}}
  end
end

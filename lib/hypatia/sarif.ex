# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.SARIF do
  @moduledoc """
  SARIF 2.1.0 exposition for Hypatia findings.

  Lets the CLI emit findings in a format IDEs and code-scanning
  surfaces ingest directly — VS Code's Code Scanning view, IntelliJ
  Qodana plugin, GitHub Security tab, etc. Replaces the inline Node.js
  converter that lived in `hypatia-scan.yml` (still kept as a backstop
  for stale-escript scenarios; the workflow can call this format
  directly instead of post-processing JSON output).

  Mirrors the existing converter's mapping exactly so a switch is a
  no-op for SARIF consumers:

    - severity → SARIF level: critical|high → "error", medium →
      "warning", everything else → "note"
    - rule id: `hypatia/<rule_module>/<rule_type>`
    - artifactLocation.uri: repo-relative POSIX (absolute paths
      stripped against repo_root; "." for repo-level findings)
    - region.startLine: integer line or 1 if missing
    - partialFingerprints: sha256(ruleId|uri|type|reason) so a moved
      finding in the same file/rule stays one alert across scans
  """

  @schema_uri "https://json.schemastore.org/sarif-2.1.0.json"
  @version "2.1.0"

  # Rule modules whose findings are lenses over GitHub's own alert
  # surfaces. Uploading them as SARIF turns each finding into a new
  # code-scanning alert that the next scan re-observes (self-echo) --
  # the boj-server post-mortem in `code_scanning_alerts.ex` describes
  # alerts 357-386 accumulated this way, and the 2026-05-27 estate
  # audit found 7,724 such alerts across 310 repos. The fetch-time
  # filter in `code_scanning_alerts.ex` stops the loop going forward
  # for any single scanner instance, but does not help when the SARIF
  # is uploaded by one runner and re-observed by the next. Filtering
  # at SARIF render time closes the loop for good.
  #
  # Findings still flow through the Elixir pipeline (PatternAnalyzer,
  # TriangleRouter, FleetDispatcher) -- only the public GitHub surface
  # is suppressed.
  @meta_rule_modules ~w(code_scanning_alerts)

  @doc """
  Build a complete SARIF document from a finding list. Pass `repo_root`
  if findings carry absolute paths so they can be relativised; defaults
  to the CWD which is appropriate when called from a scan rooted there.
  """
  def from_findings(findings, repo_root \\ File.cwd!()) do
    {results, rules} =
      findings
      |> Enum.reject(&meta_rule_finding?/1)
      |> build_results_and_rules(repo_root)

    %{
      "$schema" => @schema_uri,
      "version" => @version,
      "runs" => [
        %{
          "tool" => %{
            "driver" => %{
              "name" => "Hypatia",
              "informationUri" => "https://github.com/hyperpolymath/hypatia",
              "rules" => Map.values(rules)
            }
          },
          "results" => results
        }
      ]
    }
  end

  @doc """
  Render to a JSON string. Pretty-printed for human inspection;
  the SARIF spec doesn't care about whitespace.
  """
  def render(findings, repo_root \\ File.cwd!()) do
    findings
    |> from_findings(repo_root)
    |> Jason.encode!(pretty: true)
  end

  # ─── Internals ─────────────────────────────────────────────────────────

  defp meta_rule_finding?(finding) do
    mod = Map.get(finding, :rule_module) || Map.get(finding, "rule_module")
    stringify(mod) in @meta_rule_modules
  end

  defp build_results_and_rules(findings, repo_root) do
    Enum.reduce(findings, {[], %{}}, fn finding, {results, rules} ->
      mod = stringify(Map.get(finding, :rule_module) || Map.get(finding, "rule_module") || "hypatia")
      type = stringify(Map.get(finding, :type) || Map.get(finding, "type") || "finding")
      sev = stringify(Map.get(finding, :severity) || Map.get(finding, "severity") || "")
      file = Map.get(finding, :file) || Map.get(finding, "file") || ""
      reason = Map.get(finding, :reason) || Map.get(finding, "reason") || type
      line = Map.get(finding, :line) || Map.get(finding, "line")

      rule_id = "hypatia/#{mod}/#{type}"
      level = level_for(sev)
      uri = rel_uri(file, repo_root)

      start_line =
        case line do
          n when is_integer(n) and n > 0 -> n
          _ -> 1
        end

      fingerprint =
        :crypto.hash(:sha256, "#{rule_id}|#{uri}|#{type}|#{reason}")
        |> Base.encode16(case: :lower)

      result = %{
        "ruleId" => rule_id,
        "level" => level,
        "message" => %{"text" => to_string(reason)},
        "locations" => [
          %{
            "physicalLocation" => %{
              "artifactLocation" => %{"uri" => uri},
              "region" => %{"startLine" => start_line}
            }
          }
        ],
        "partialFingerprints" => %{"hypatiaFindingHash/v1" => fingerprint}
      }

      rules =
        Map.put_new(rules, rule_id, %{
          "id" => rule_id,
          "name" => "#{mod}.#{type}",
          "shortDescription" => %{"text" => "Hypatia #{mod}: #{type}"},
          "defaultConfiguration" => %{"level" => level}
        })

      {[result | results], rules}
    end)
    |> then(fn {results, rules} -> {Enum.reverse(results), rules} end)
  end

  defp level_for("critical"), do: "error"
  defp level_for("high"), do: "error"
  defp level_for("medium"), do: "warning"
  defp level_for(_), do: "note"

  defp rel_uri("", _root), do: "."
  defp rel_uri(nil, _root), do: "."

  defp rel_uri(file, root) when is_binary(file) do
    f =
      if Path.type(file) == :absolute do
        rel = Path.relative_to(file, root)

        if rel == file or String.starts_with?(rel, ".."),
          do: Path.basename(file),
          else: rel
      else
        file
      end

    f
    |> String.replace("\\", "/")
    |> String.replace_prefix("./", "")
    |> case do
      "" -> "."
      cleaned -> cleaned
    end
  end

  defp rel_uri(file, root), do: rel_uri(to_string(file), root)

  defp stringify(v) when is_binary(v), do: v
  defp stringify(v) when is_atom(v), do: Atom.to_string(v)
  defp stringify(v), do: to_string(v)
end

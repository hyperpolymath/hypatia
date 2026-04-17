# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.Dogfooding do
  @moduledoc """
  Dogfooding compliance rules -- HYP-DOG-001 through HYP-DOG-010.

  These rules detect silent failures in the hyperpolymath tool ecosystem.
  Derived from the 2026-04-04 estate-wide dogfooding audit which found
  that tools were "deployed but not running" across 93+ repos.

  Each rule maps to a DOG-XX check in the Testing Taxonomy (Part VII)
  and produces findings compatible with the panic-attack JSON format.

  Reference: standards/testing-and-benchmarking/TESTING-TAXONOMY.adoc
  """

  # ─── Stale tool names that have been renamed ──────────────────────────

  @stale_names %{
    "panic-attacker" => {"panic-attack", "renamed 2026-02-08"},
    "xray" => {"assail", "subcommand renamed 2026-02-08"},
    "AGPL-3.0" => {"PMPL-1.0-or-later", "license changed -- except idaptik and airborne-submarine-squadron"}
  }

  # ─── Template placeholders that must be resolved ─────────────────────

  @template_placeholders [
    "{{OWNER}}",
    "{{REPO}}",
    "{{FORGE}}",
    "{{CURRENT_YEAR}}",
    "{{AUTHOR}}",
    "{{AUTHOR_EMAIL}}",
    "[YOUR-REPO-NAME]",
    "[YOUR-PROJECT]"
  ]

  @doc """
  Run all dogfooding rules against a repo path.

  Returns a list of `%{rule: String, severity: String, file: String | nil,
  line: integer | nil, description: String}` findings.
  """
  def check(repo_path) when is_binary(repo_path) do
    []
    |> check_template_placeholders(repo_path)
    |> check_stale_names(repo_path)
    |> check_groove_port_mismatch(repo_path)
    |> check_docker_usage(repo_path)
    |> check_consumer_field_names(repo_path)
    |> check_silent_skip(repo_path)
    |> check_groove_missing(repo_path)
    |> check_verisimdb_missing(repo_path)
    |> check_manifest_placeholders(repo_path)
  end

  # ─── HYP-DOG-001: Unfilled template placeholders in workflows ────────

  defp check_template_placeholders(findings, repo_path) do
    workflow_dir = Path.join(repo_path, ".github/workflows")

    if File.dir?(workflow_dir) do
      workflow_dir
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".yml"))
      |> Enum.flat_map(fn file ->
        filepath = Path.join(workflow_dir, file)
        content = File.read!(filepath)

        @template_placeholders
        |> Enum.filter(&String.contains?(content, &1))
        |> Enum.map(fn placeholder ->
          line = find_line_number(content, placeholder)

          %{
            rule: "HYP-DOG-001",
            severity: "critical",
            file: filepath,
            line: line,
            description:
              "Unfilled template placeholder '#{placeholder}' -- CI tools will silently skip"
          }
        end)
      end)
      |> Kernel.++(findings)
    else
      findings
    end
  end

  # ─── HYP-DOG-003: Renamed tool references ────────────────────────────

  defp check_stale_names(findings, repo_path) do
    # Check workflow files and shell scripts
    search_extensions = [".yml", ".sh", ".just"]

    files_to_check =
      walk_files(repo_path, search_extensions, 3)
      |> Enum.reject(&String.contains?(&1, "/.git/"))

    Enum.flat_map(files_to_check, fn filepath ->
      content = File.read!(filepath)

      @stale_names
      |> Enum.flat_map(fn {old_name, {new_name, reason}} ->
        # Skip AGPL check for idaptik and airborne-submarine-squadron
        if old_name == "AGPL-3.0" and
             (String.contains?(filepath, "idaptik") or
                String.contains?(filepath, "airborne-submarine")) do
          []
        else
          if String.contains?(content, old_name) do
            # Check if it's in an executable context (not just a comment/doc)
            line = find_line_number(content, old_name)

            [
              %{
                rule: "HYP-DOG-003",
                severity: "high",
                file: filepath,
                line: line,
                description:
                  "Stale reference '#{old_name}' -- now '#{new_name}' (#{reason})"
              }
            ]
          else
            []
          end
        end
      end)
    end)
    |> Kernel.++(findings)
  end

  # ─── HYP-DOG-004: Groove port mismatches ─────────────────────────────

  defp check_groove_port_mismatch(findings, repo_path) do
    manifest_path = Path.join(repo_path, ".well-known/groove/manifest.json")

    if File.exists?(manifest_path) do
      case File.read!(manifest_path) |> Jason.decode() do
        {:ok, manifest} ->
          # Check if endpoints URLs contain port numbers
          endpoints = Map.get(manifest, "endpoints", %{})

          port_findings =
            Enum.flat_map(endpoints, fn {_name, url} ->
              case Regex.run(~r/:(\d+)/, url || "") do
                [_, port_str] ->
                  port = String.to_integer(port_str)
                  service_id = Map.get(manifest, "service_id", "unknown")

                  # Check against known canonical ports
                  canonical = canonical_port(service_id)

                  if canonical && canonical != port do
                    [
                      %{
                        rule: "HYP-DOG-004",
                        severity: "high",
                        file: manifest_path,
                        line: nil,
                        description:
                          "Groove manifest advertises port #{port} but canonical port for '#{service_id}' is #{canonical}"
                      }
                    ]
                  else
                    []
                  end

                _ ->
                  []
              end
            end)

          port_findings ++ findings

        _ ->
          findings
      end
    else
      findings
    end
  end

  # ─── HYP-DOG-005: Docker/Dockerfile in non-third-party code ──────────

  defp check_docker_usage(findings, repo_path) do
    docker_findings =
      walk_files(repo_path, [".yml", ".sh", ".just"], 3)
      |> Enum.reject(&String.contains?(&1, "/.git/"))
      |> Enum.reject(&String.contains?(&1, "/node_modules/"))
      |> Enum.reject(&String.contains?(&1, "/external_corpora/"))
      |> Enum.reject(&String.contains?(&1, "/deps/"))
      |> Enum.flat_map(fn filepath ->
        content = File.read!(filepath)

        cond do
          # Check for docker CLI usage (not just the word "docker" in docs)
          Regex.match?(~r/\bdocker\s+(build|run|push|pull|tag|exec)\b/, content) ->
            line = find_line_number(content, "docker ")

            [
              %{
                rule: "HYP-DOG-005",
                severity: "medium",
                file: filepath,
                line: line,
                description:
                  "Uses 'docker' CLI -- policy requires 'podman'. Replace docker commands with podman equivalents."
              }
            ]

          true ->
            []
        end
      end)

    # Check for Dockerfile (should be Containerfile)
    dockerfile_findings =
      if File.exists?(Path.join(repo_path, "Dockerfile")) do
        [
          %{
            rule: "HYP-DOG-005",
            severity: "medium",
            file: Path.join(repo_path, "Dockerfile"),
            line: nil,
            description:
              "File named 'Dockerfile' -- policy requires 'Containerfile'. Rename the file."
          }
        ]
      else
        []
      end

    docker_findings ++ dockerfile_findings ++ findings
  end

  # ─── HYP-DOG-006: Consumer field name mismatches ─────────────────────

  defp check_consumer_field_names(findings, repo_path) do
    # Check if any jq expressions in workflows reference .file when panic-attack
    # uses .location (the old pattern -- now fixed, but check for the wrong field)
    walk_files(repo_path, [".yml", ".sh"], 3)
    |> Enum.reject(&String.contains?(&1, "/.git/"))
    |> Enum.flat_map(fn filepath ->
      content = File.read!(filepath)

      if String.contains?(content, "panic-attack") or
           String.contains?(content, "panic-attacker") do
        # Check for .location.file pattern (treats string as object -- always null)
        if String.contains?(content, ".location.file") do
          line = find_line_number(content, ".location.file")

          [
            %{
              rule: "HYP-DOG-006",
              severity: "high",
              file: filepath,
              line: line,
              description:
                "Parses '.location.file' but panic-attack's location is a string, not object. Use '.file' instead."
            }
          ]
        else
          []
        end
      else
        []
      end
    end)
    |> Kernel.++(findings)
  end

  # ─── HYP-DOG-007: Silent skip patterns ───────────────────────────────

  defp check_silent_skip(findings, repo_path) do
    workflow_dir = Path.join(repo_path, ".github/workflows")

    if File.dir?(workflow_dir) do
      workflow_dir
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".yml"))
      |> Enum.flat_map(fn file ->
        filepath = Path.join(workflow_dir, file)
        content = File.read!(filepath)

        # Detect pattern: continue-on-error: true followed by "installed=false" or "ready=false"
        if String.contains?(content, "continue-on-error: true") and
             (String.contains?(content, "installed=false") or
                String.contains?(content, "ready=false")) do
          line = find_line_number(content, "continue-on-error: true")

          [
            %{
              rule: "HYP-DOG-007",
              severity: "medium",
              file: filepath,
              line: line,
              description:
                "Silent skip pattern: continue-on-error may mask a broken tool install. Verify the tool actually runs."
            }
          ]
        else
          []
        end
      end)
      |> Kernel.++(findings)
    else
      findings
    end
  end

  # ─── HYP-DOG-008: HTTP server without Groove ─────────────────────────

  defp check_groove_missing(findings, repo_path) do
    has_groove =
      File.exists?(Path.join(repo_path, ".well-known/groove/manifest.json"))

    # Simple heuristic: check for HTTP server patterns
    has_server =
      walk_files(repo_path, [".rs", ".ex", ".zig", ".js", ".ts"], 4)
      |> Enum.reject(&String.contains?(&1, "/.git/"))
      |> Enum.reject(&String.contains?(&1, "/target/"))
      |> Enum.reject(&String.contains?(&1, "/node_modules/"))
      |> Enum.any?(fn filepath ->
        content = File.read!(filepath)

        String.contains?(content, "axum::serve") or
          String.contains?(content, "TcpListener") or
          String.contains?(content, "Plug.Cowboy") or
          String.contains?(content, "Bandit") or
          String.contains?(content, "Phoenix.Endpoint") or
          String.contains?(content, "Deno.serve")
      end)

    if has_server and not has_groove do
      [
        %{
          rule: "HYP-DOG-008",
          severity: "low",
          file: nil,
          line: nil,
          description:
            "HTTP server detected but no Groove manifest. Run `groove init` to generate .well-known/groove/manifest.json"
        }
        | findings
      ]
    else
      findings
    end
  end

  # ─── HYP-DOG-009: Stateful repo without VeriSimDB ───────────────────

  defp check_verisimdb_missing(findings, repo_path) do
    has_verisimdb =
      walk_files(repo_path, [".rs", ".ex", ".js", ".ts", ".toml", ".json"], 3)
      |> Enum.reject(&String.contains?(&1, "/.git/"))
      |> Enum.reject(&String.contains?(&1, "/node_modules/"))
      |> Enum.any?(fn filepath ->
        content = File.read!(filepath)
        String.contains?(content, "verisim") or String.contains?(content, "VeriSimDB")
      end)

    # Check for state storage patterns
    has_state =
      walk_files(repo_path, [".rs", ".ex"], 4)
      |> Enum.reject(&String.contains?(&1, "/.git/"))
      |> Enum.reject(&String.contains?(&1, "/target/"))
      |> Enum.any?(fn filepath ->
        content = File.read!(filepath)

        String.contains?(content, "rusqlite") or
          String.contains?(content, "Ecto.Repo") or
          String.contains?(content, "ArangoXEcto") or
          String.contains?(content, ":ets.new") or
          String.contains?(content, "Mnesia")
      end)

    if has_state and not has_verisimdb do
      [
        %{
          rule: "HYP-DOG-009",
          severity: "low",
          file: nil,
          line: nil,
          description:
            "State storage detected (SQLite/ETS/Ecto/Mnesia) but no VeriSimDB integration. Add VeriSimDB for durability and queryability."
        }
        | findings
      ]
    else
      findings
    end
  end

  # ─── HYP-DOG-010: Unfilled manifest placeholders ────────────────────

  defp check_manifest_placeholders(findings, repo_path) do
    manifest_path = Path.join(repo_path, "0-AI-MANIFEST.a2ml")

    if File.exists?(manifest_path) do
      content = File.read!(manifest_path)

      if String.contains?(content, "[YOUR-REPO-NAME]") or
           String.contains?(content, "[YOUR-PROJECT]") do
        line = find_line_number(content, "[YOUR")

        [
          %{
            rule: "HYP-DOG-010",
            severity: "medium",
            file: manifest_path,
            line: line,
            description:
              "Unfilled template placeholder in AI manifest. Replace [YOUR-REPO-NAME] with the actual project name."
          }
          | findings
        ]
      else
        findings
      end
    else
      findings
    end
  end

  # ─── Helpers ──────────────────────────────────────────────────────────

  @doc false
  defp find_line_number(content, pattern) do
    content
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.find_value(fn {line, num} ->
      if String.contains?(line, pattern), do: num
    end)
  end

  @doc false
  defp walk_files(root, extensions, max_depth) do
    if File.dir?(root) do
      do_walk(root, extensions, 0, max_depth)
    else
      []
    end
  end

  defp do_walk(_dir, _extensions, depth, max_depth) when depth > max_depth, do: []

  defp do_walk(dir, extensions, depth, max_depth) do
    case File.ls(dir) do
      {:ok, entries} ->
        Enum.flat_map(entries, fn entry ->
          path = Path.join(dir, entry)

          cond do
            File.dir?(path) and entry not in [".git", "node_modules", "target", "_build", "deps", ".deno", "external_corpora", ".lake"] ->
              do_walk(path, extensions, depth + 1, max_depth)

            File.regular?(path) ->
              ext = Path.extname(path)
              if ext in extensions, do: [path], else: []

            true ->
              []
          end
        end)

      _ ->
        []
    end
  end

  @doc false
  defp canonical_port(service_id) do
    %{
      "burble" => 6473,
      "vext" => 6480,
      "panic-attack" => 7600,
      "conflow" => 7700,
      "rpa-elysium" => 7800,
      "panll" => 8000,
      "verisim" => 8080,
      "gitbot-fleet" => 8080,
      "echidna" => 9000,
      "echidnabot" => 9001,
      "hypatia" => 9090
    }
    |> Map.get(service_id)
  end
end

# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules do
  @moduledoc """
  Facade module for all Hypatia rules, absorbed from the Logtalk engine.

  This replaces the need for Logtalk rule evaluation by providing
  all policy rules, security checks, and learning operations directly
  in Elixir. The Logtalk files in engine/ remain as documentation of
  the original rule declarations.

  ## Submodules

  - `SecurityErrors` -- Error categories, SHA pins, secret detection, CodeQL, CWE mappings
  - `CicdRules` -- Repo requirements, commit blocking, waste detection, license validation
  - `CodeSafety` -- Language-specific dangerous pattern detection
  - `MigrationRules` -- ReScript v12→v13 migration, merge conflict resolution
  - `Learning` -- Fix outcome tracking, confidence, pattern promotion (GenServer)
  - `ForgeAdapters` -- Multi-forge operations with input validation
  """

  alias Hypatia.Rules.SecurityErrors
  alias Hypatia.Rules.CicdRules
  alias Hypatia.Rules.CodeSafety
  alias Hypatia.Rules.MigrationRules

  @doc """
  Run a comprehensive scan on a file's content given its path and language.
  Returns a list of all findings across all rule categories.
  """
  def scan_file(content, file_path, language) do
    findings = []

    # Code safety patterns
    findings = findings ++ CodeSafety.scan_content(content, language)

    # Secret detection
    secrets = SecurityErrors.detect_secrets(content)
    findings = findings ++ Enum.map(secrets, fn label ->
      %{rule: "secret_detected", severity: :critical, description: "Secret found: #{label}"}
    end)

    # SPDX header check
    findings =
      if not String.contains?(content, "SPDX-License-Identifier:") and
           file_path not in [".gitignore", "LICENSE", "LICENSE.txt"] do
        [%{rule: "missing_spdx", severity: :medium,
           description: "Missing SPDX-License-Identifier header"} | findings]
      else
        findings
      end

    # License check (if SPDX present)
    # Extract repo name from file path for AGPL exception handling.
    # Matches repo name from paths like /mnt/eclipse/repos/<repo>/ or */<repo>/src/*.
    repo_name = extract_repo_name(file_path)
    findings =
      case Regex.run(~r/SPDX-License-Identifier:\s*(.+)/, content) do
        [_, spdx_id] ->
          case CicdRules.validate_license(String.trim(spdx_id), repo_name) do
            {:error, :wrong_license, bad} ->
              [%{rule: "wrong_license", severity: :high,
                 description: "Wrong license #{bad} -- should be PMPL-1.0-or-later"} | findings]
            _ -> findings
          end
        _ -> findings
      end

    # Deprecated ReScript API check
    findings =
      if language == "rescript" do
        findings ++ Enum.map(MigrationRules.scan_deprecated_usage(content), fn dep ->
          %{rule: "deprecated_api", severity: dep.severity,
            description: "#{dep.api} deprecated -- use #{dep.replacement} (#{dep.count} occurrences)"}
        end)
      else
        findings
      end

    # Stub/placeholder crypto detection (all languages)
    findings = findings ++ CodeSafety.scan_stub_crypto(content)

    # Web security patterns for JS/TS files not already covered by language patterns
    findings =
      if language not in ["javascript", "typescript"] and
           (String.ends_with?(file_path, ".js") or
            String.ends_with?(file_path, ".ts") or
            String.ends_with?(file_path, ".mjs")) do
        findings ++ CodeSafety.scan_web_security(content)
      else
        findings
      end

    # Container code patterns for Containerfiles and shell scripts
    findings =
      if String.ends_with?(file_path, "Containerfile") or
           String.ends_with?(file_path, "Dockerfile") or
           String.ends_with?(file_path, ".sh") do
        findings ++ CodeSafety.scan_container_code(content)
      else
        findings
      end

    # Cargo.lock vulnerability check
    findings =
      if String.ends_with?(file_path, "Cargo.lock") do
        # Detect vulnerable glib versions (e.g., 0.18.5)
        # Fixed in: 0.18.6, 0.19.10, 0.20.7, 0.22.3
        findings =
          if Regex.match?(~r/name = "glib"\s+version = "0\.(15|16|17|18\.[0-5]|19\.[0-9])"/, content) do
            [%{rule: "glib-variantstriter-unsoundness", severity: :medium,
               description: "Vulnerable glib version detected in Cargo.lock -- update to >= 0.18.6, 0.19.10, 0.20.7 or 0.22.3"} | findings]
          else
            findings
          end

        # Detect vulnerable crossbeam-utils versions (AtomicCell unsoundness)
        # Fixed in: 0.8.7
        findings =
          if Regex.match?(~r/name = "crossbeam-utils"\s+version = "0\.(6\.|7\.|8\.[0-6])"/, content) do
            [%{rule: "crossbeam-utils-atomiccell-unsoundness", severity: :high,
               description: "Vulnerable crossbeam-utils version detected in Cargo.lock -- update to >= 0.8.7"} | findings]
          else
            findings
          end

        # Detect vulnerable lock_api versions (Data races)
        # Fixed in: 0.4.2
        findings =
          if Regex.match?(~r/name = "lock_api"\s+version = "0\.(1\.|2\.|3\.|4\.[0-1])"/, content) do
            [%{rule: "lock-api-data-race", severity: :medium,
               description: "Vulnerable lock_api version detected in Cargo.lock -- update to >= 0.4.2"} | findings]
          else
            findings
          end

        # Detect vulnerable crossbeam-queue versions (SegQueue unsoundness)
        # Fixed in: 0.2.3 (for 0.2 series) or 0.1.x is vulnerable
        findings =
          if Regex.match?(~r/name = "crossbeam-queue"\s+version = "0\.(1\.|2\.[0-2])"/, content) do
            [%{rule: "crossbeam-queue-segqueue-unsoundness", severity: :medium,
               description: "Vulnerable crossbeam-queue version detected in Cargo.lock -- update to >= 0.2.3"} | findings]
          else
            findings
          end

        # Detect vulnerable protobuf versions (Recursion crash)
        # Fixed in: 3.7.2
        findings =
          if Regex.match?(~r/name = "protobuf"\s+version = "(2\.|3\.[0-6]\.|3\.7\.[01])"/, content) do
            [%{rule: "protobuf-recursion-crash", severity: :medium,
               description: "Vulnerable protobuf version detected in Cargo.lock -- update to >= 3.7.2"} | findings]
          else
            findings
          end

        # Detect vulnerable idna versions (Punycode mishandling)
        # Fixed in: 1.0.0
        findings =
          if Regex.match?(~r/name = "idna"\s+version = "0\.[0-4]\."/, content) do
            [%{rule: "idna-punycode-mishandling", severity: :medium,
               description: "Vulnerable idna version detected in Cargo.lock -- update to >= 1.0.0"} | findings]
          else
            findings
          end

        # Detect vulnerable jsonwebtoken versions (Type confusion)
        # Fixed in: 10.3.0
        findings =
          if Regex.match?(~r/name = "jsonwebtoken"\s+version = "[0-9]\."/, content) do
            [%{rule: "jsonwebtoken-type-confusion", severity: :medium,
               description: "Vulnerable jsonwebtoken version detected in Cargo.lock -- update to >= 10.3.0"} | findings]
          else
            findings
          end

        # Detect vulnerable lru versions (Stacked Borrows violation)
        # Fixed in: 0.16.3
        findings =
          if Regex.match?(~r/name = "lru"\s+version = "0\.(9\.|1[0-5]\.|16\.[0-2])"/, content) do
            [%{rule: "lru-itermut-stacked-borrows", severity: :low,
               description: "Vulnerable lru version detected in Cargo.lock -- update to >= 0.16.3"} | findings]
          else
            findings
          end

        # Detect vulnerable ring versions (AES overflow panic)
        # Fixed in: 0.17.12
        findings =
          if Regex.match?(~r/name = "ring"\s+version = "0\.17\.(?:[0-9]|1[01])"/, content) do
            [%{rule: "ring-aes-overflow-panic", severity: :high,
               description: "Vulnerable ring version detected in Cargo.lock -- update to >= 0.17.12"} | findings]
          else
            findings
          end

        # Detect vulnerable yamux versions (remote panic)
        # Fixed in: 0.13.10 (and 0.13.9 for GHSA-4w32-2493-32g7)
        findings =
          if Regex.match?(~r/name = "yamux"\s+version = "(0\.12\.[0-9]+|0\.13\.[0-9])"/, content) do
            [%{rule: "yamux-remote-panic", severity: :high,
               description: "Vulnerable yamux version detected in Cargo.lock -- update to >= 0.13.10"} | findings]
          else
            findings
          end

        # Detect vulnerable atty versions (potential unaligned read)
        # No upstream patched version available for GHSA-g98v-hv3f-hcfr
        findings =
          if Regex.match?(~r/name = "atty"\s+version = "0\.2\.(?:[0-9]|1[0-4])"/, content) do
            [%{rule: "atty-unaligned-read", severity: :medium,
               description: "Vulnerable atty version detected in Cargo.lock -- replace atty usage or migrate to is-terminal"} | findings]
          else
            findings
          end

        findings
      else
        findings
      end

    # yarn.lock vulnerability check (Yarn Berry/v1)
    findings =
      if String.ends_with?(file_path, "yarn.lock") do
        # serialize-javascript RCE/XSS
        findings =
          if Regex.match?(~r/serialize-javascript@npm:[^:]+\s+version: ([0-5]\.|6\.0\.[01])/, content) do
            [%{rule: "npm-serialize-javascript-vulnerability", severity: :high,
               description: "Vulnerable serialize-javascript version detected in yarn.lock -- update to >= 6.0.2 or 7.0.0"} | findings]
          else
            findings
          end

        # minimatch ReDoS
        findings =
          if Regex.match?(~r/minimatch@npm:[^:]+\s+version: ([0-8]\.|9\.0\.[0-4])/, content) do
            [%{rule: "npm-minimatch-vulnerability", severity: :high,
               description: "Vulnerable minimatch version detected in yarn.lock -- update to >= 9.0.5 or 10.0.0"} | findings]
          else
            findings
          end

        # glob command injection
        findings =
          if Regex.match?(~r/glob@npm:[^:]+\s+version: ([0-9]\.|10\.[0-5]\.0)/, content) do
            [%{rule: "npm-glob-vulnerability", severity: :high,
               description: "Vulnerable glob version detected in yarn.lock -- update to >= 11.0.0"} | findings]
          else
            findings
          end

        # js-yaml prototype pollution
        findings =
          if Regex.match?(~r/js-yaml@npm:[^:]+\s+version: ([0-2]\.|3\.(1[0-3]\.[0]|14\.[01]))/, content) do
            [%{rule: "npm-js-yaml-vulnerability", severity: :medium,
               description: "Vulnerable js-yaml version detected in yarn.lock -- update to >= 3.14.2 or 4.1.1"} | findings]
          else
            findings
          end

        # h3 vulnerabilities
        findings =
          if Regex.match?(~r/h3@npm:[^:]+\s+version: 2\.0\.1-rc\.( [0-9]|1[0-4])/, content) do
            [%{rule: "npm-h3-vulnerability", severity: :high,
               description: "Vulnerable h3 version detected in yarn.lock -- update to >= 2.0.1-rc.15"} | findings]
          else
            findings
          end

        findings
      else
        findings
      end

    # mix.lock vulnerability check (Elixir/BEAM ecosystem)
    findings =
      if String.ends_with?(file_path, "mix.lock") do
        # Plug (HTTP adapter) -- arbitrary code execution via malformed multipart
        findings =
          if Regex.match?(~r/"plug":\s*\{[^}]*"version":\s*"(1\.[0-9]\.|1\.1[0-5]\.)/, content) do
            [%{rule: "hex-plug-vulnerability", severity: :high,
               description: "Vulnerable plug version in mix.lock -- update to >= 1.16.0 for multipart fixes"} | findings]
          else
            findings
          end

        # HTTPoison / Hackney -- TLS cert verification issues
        findings =
          if Regex.match?(~r/"hackney":\s*\{[^}]*"version":\s*"1\.(1[0-7]\.)/, content) do
            [%{rule: "hex-hackney-vulnerability", severity: :medium,
               description: "Vulnerable hackney version in mix.lock -- update to >= 1.18.0 for TLS improvements"} | findings]
          else
            findings
          end

        # Poison (JSON parser) -- prototype-style atom exhaustion via keys
        findings =
          if Regex.match?(~r/"poison":\s*\{[^}]*"version":\s*"[1-4]\."/, content) do
            [%{rule: "hex-poison-atom-risk", severity: :medium,
               description: "Poison < 5.0 in mix.lock may create atoms from JSON keys -- update to >= 5.0 or switch to Jason"} | findings]
          else
            findings
          end

        # ex_doc -- older versions have XSS in generated docs
        findings =
          if Regex.match?(~r/"ex_doc":\s*\{[^}]*"version":\s*"0\.(2[0-9]\.|30\.[0-5])"/, content) do
            [%{rule: "hex-ex-doc-xss", severity: :low,
               description: "ex_doc < 0.31.0 may generate docs with XSS -- update to >= 0.31.0"} | findings]
          else
            findings
          end

        # nimble_parsec -- stack overflow on deeply nested input
        findings =
          if Regex.match?(~r/"nimble_parsec":\s*\{[^}]*"version":\s*"0\."/, content) do
            [%{rule: "hex-nimble-parsec-stack-overflow", severity: :medium,
               description: "nimble_parsec 0.x may stack-overflow on deeply nested input -- update to >= 1.0.0"} | findings]
          else
            findings
          end

        findings
      else
        findings
      end

    findings
  end

  @doc """
  Check a workflow YAML file for common issues.
  Returns a list of findings.
  """
  def scan_workflow(content) do
    findings = []

    # Unpinned actions
    unpinned = Regex.scan(~r/uses:\s*([^\s]+@v\d+)/, content)
    findings = findings ++ Enum.map(unpinned, fn [_full, action_ref] ->
      suggestion = case SecurityErrors.pin_action(action_ref) do
        {:ok, pinned} -> " -- fix: #{pinned}"
        _ -> ""
      end
      %{rule: "unpinned_action", severity: :high,
        description: "Unpinned action: #{action_ref}#{suggestion}"}
    end)

    # Missing permissions
    findings =
      if not Regex.match?(~r/^permissions:/m, content) do
        [%{rule: "missing_permissions", severity: :high,
           description: "Workflow missing permissions declaration -- add permissions: read-all"} | findings]
      else
        findings
      end

    findings
  end

  @doc """
  Get the SHA pin for a GitHub Action reference, or nil if unknown.
  """
  defdelegate pin_action(action_ref), to: SecurityErrors

  @doc """
  Check if an issue type can be auto-fixed.
  """
  defdelegate auto_fixable?(issue_type), to: SecurityErrors

  @doc """
  Get the fix suggestion for an issue type.
  """
  defdelegate fix_suggestion(issue_type), to: SecurityErrors

  @doc """
  Get the prevention workflow for an issue type.
  """
  defdelegate prevention_workflow(issue_type), to: SecurityErrors

  @doc """
  Validate a CodeQL language matrix against repo languages.
  """
  defdelegate validate_codeql_matrix(repo_languages), to: SecurityErrors

  @doc """
  Check repo requirements (SECURITY.md, dependabot.yml, etc.)
  """
  defdelegate check_repo_requirements(repo_info), to: CicdRules

  @doc """
  Detect CI/CD waste patterns.
  """
  defdelegate detect_waste(repo_info), to: CicdRules

  # ---------------------------------------------------------------------------
  # Private Helpers
  # ---------------------------------------------------------------------------

  # Extract repo name from a file path. Checks for known repo root markers
  # (e.g., /repos/<name>/) and falls back to the first non-empty path segment.
  defp extract_repo_name(file_path) when is_binary(file_path) do
    parts = Path.split(file_path)

    # Look for "repos" directory marker -- repo name is the next segment
    case Enum.find_index(parts, &(&1 == "repos")) do
      nil ->
        # Fallback: use basename of parent dirs (best effort)
        Enum.find(Enum.reverse(Path.split(Path.dirname(file_path))), & &1 != "")

      idx ->
        Enum.at(parts, idx + 1)
    end
  end

  defp extract_repo_name(_), do: nil

  @doc """
  Run repo-level dogfooding compliance checks (HYP-DOG-001 through HYP-DOG-010).

  Unlike `scan_file/3` which checks individual files, this checks the repo
  as a whole for dogfooding compliance -- template placeholders, Groove presence,
  VeriSimDB wiring, stale tool names, etc.

  Returns a list of findings in the standard format.
  """
  def scan_repo_dogfooding(repo_path) when is_binary(repo_path) do
    Hypatia.Rules.Dogfooding.check(repo_path)
  end
end

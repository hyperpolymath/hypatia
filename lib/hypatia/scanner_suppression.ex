# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ScannerSuppression do
  @moduledoc """
  Provenance-aware suppression for the Hypatia scanner.

  The scanner has no innate concept of "this file is a fixture / training
  corpus / known-safe pattern reference," so secret-detection and other
  pattern rules re-discover their own training data and fixtures as findings
  (e.g. `.audittraining/` documents, `lib/rules/security_errors.ex` where the
  patterns themselves live, `scripts/fix-scripts/fix-hardcoded-secrets.sh`).

  This module centralises:

    * Built-in default exemption globs for rules that are content-pattern
      rather than logic-driven (i.e. secret detection, hardcoded credential
      detection). These are deliberately narrow — they suppress only the
      rules whose pattern *is* the training corpus, not all rules.

    * Optional repo-level `.hypatia-ignore` overrides (per-rule paths).

    * Inline `# hypatia: allow <rule_module>/<type> -- <reason>` directives
      recognised on the matching line or the line immediately above. This
      replaces the `// nosemgrep` mechanism for hypatia's native rules and
      gives a uniform, auditable suppression signal across languages.

  Anything not pattern-rule-noise should NOT be added here; real findings
  belong in `.hypatia-baseline.json` so they're tracked and can be reviewed
  during a follow-up PR.
  """

  # ─── Built-in path exemptions, by (rule_module, rule_type) ─────────────
  #
  # Two-level map: the outer key is the rule module (e.g. "security_errors"),
  # the inner key is the rule type (e.g. "secret_detected"). The list of
  # glob-style suffixes / substrings is matched against the file's repo-
  # relative path.
  #
  # `:any` as inner key applies to every rule type in that module.

  # Path fragments that are *by definition* a rule's training corpus:
  # rule-definition modules (the regex set lives here — flagging it is
  # self-recursion), remediation scripts (they document the bad pattern
  # they fix), training corpora, and test fixtures. Any content-pattern
  # rule (`secret_detected`, `shell_download_then_run`, `elixir_*`, etc.)
  # firing in these paths is provenance noise, not signal.
  @training_corpus_paths [
    ".audittraining/",
    "lib/rules/",
    "scripts/fix-scripts/",
    "test/",
    "tests/",
    "integration/fixtures/",
    "integration/run-tests.sh"
  ]

  @default_exemptions %{
    # `:any` key under a module: applies to every rule type in that module.
    "security_errors" => %{
      :any => @training_corpus_paths ++
                [".github/workflows/integration.yml"]
    },
    "code_safety" => %{
      :any => @training_corpus_paths
    }
  }

  # Default exemption directory globs that suppress *every* content-pattern
  # rule (not logic rules) — used for vendored / generated / .git dirs.
  @universal_excludes [
    ".git/", "node_modules/", "_build/", "deps/", "target/", "dist/"
  ]

  @doc """
  Return true if (rule_module, rule_type) is suppressed for `file` by either
  the built-in defaults or the repo's `.hypatia-ignore`.

  `file` is matched as a normalised relative path. Absolute paths are
  normalised by stripping the leading repo_path when supplied.
  """
  def suppressed?(file, rule_module, rule_type, opts \\ []) do
    repo_path = Keyword.get(opts, :repo_path, nil)
    rel = relative(file, repo_path)

    universal_excluded?(rel) or
      match_exemptions?(rel, @default_exemptions, rule_module, rule_type) or
      match_exemptions?(rel, user_exemptions(repo_path), rule_module, rule_type)
  end

  @doc """
  Return true if `line` is itself a non-finding even when the pattern matches
  (e.g. a GitHub Actions `${{ secrets.X }}` reference is *not* a leaked
  secret — it is a reference to the secret store). Centralised so future
  rules can opt in via the same predicate.
  """
  def context_safe_line?("secret_detected", line) do
    Regex.match?(gha_secret_ref_re(), line) or
      Regex.match?(gha_vars_ref_re(), line) or
      Regex.match?(shell_param_expansion_re(), line) or
      Regex.match?(env_default_re(), line) or
      Regex.match?(rust_test_literal_re(), line)
  end

  def context_safe_line?(_rule_type, _line), do: false

  @doc """
  Return true if an inline `hypatia: allow` directive on `line` or
  `prev_line` covers `(rule_module, rule_type)`.

  Recognised forms (case-insensitive, leading comment marker `#`, `//`, `--`,
  `;` accepted):

      # hypatia: allow security_errors/secret_detected -- example for docs
      // hypatia: allow code_safety/believe_me -- intentional proof bypass
      -- hypatia: allow code_safety/believe_me

  Either the bare rule type or the fully qualified `module/type` form match.
  A wildcard `*` in either position matches everything (use sparingly).
  """
  def inline_allowed?(line, prev_line, rule_module, rule_type) do
    Enum.any?([line, prev_line], fn l ->
      is_binary(l) and directive_matches?(l, rule_module, rule_type)
    end)
  end

  @doc """
  File-level allow directive: any of the first `:max_header_lines` (default
  20) of the file may include a `hypatia: allow <module>/<type>` directive
  that suppresses *every* matching finding in the file. Used for files like
  `src/abi/RuleEngine.idr` which contain intentional `believe_me` usage and
  want to declare the allowance once at the top.
  """
  def file_allowed?(content, rule_module, rule_type, opts \\ []) do
    max = Keyword.get(opts, :max_header_lines, 20)

    content
    |> String.split("\n", parts: max + 1)
    |> Enum.take(max)
    |> Enum.any?(fn line -> directive_matches?(line, rule_module, rule_type) end)
  end

  # ─── Internals ──────────────────────────────────────────────────────────

  defp gha_secret_ref_re,
    do: ~r/\$\{\{\s*secrets\.[A-Za-z_][A-Za-z0-9_]*\s*\}\}/

  defp gha_vars_ref_re,
    do: ~r/\$\{\{\s*vars\.[A-Za-z_][A-Za-z0-9_]*\s*\}\}/

  # Shell parameter expansion — `"${VAR:-default}"`, `"$VAR"`, `"$(cmd)"`,
  # etc. The value is dereferenced at runtime, so the line isn't a
  # hardcoded secret. Matches both braced (`${X}`) and bare (`$X`) forms.
  defp shell_param_expansion_re,
    do: ~r/(?i)(?:password|secret|api[_-]?key|token)\s*[:=]\s*["']?\$(?:\{|\(|[A-Za-z_])/

  # `PASSWORD="${PASSWORD:-}"` style env-default binding — the variable is
  # exported only if already set in the environment; the literal here is the
  # empty-default sentinel, not a secret.
  defp env_default_re,
    do: ~r/(?i)(?:password|secret|api[_-]?key|token)\s*=\s*"\$\{[A-Za-z_][A-Za-z0-9_]*:-[^}]*\}"/

  # Rust test fixtures — `let secret = "test-secret"` and similar `test-`
  # / `dummy-` / `fake-` prefixed literals appearing in `#[test]` bodies.
  # The line-level check is a heuristic (the secret scanner is line-local
  # and can't tell #[cfg(test)] context); the `test-` prefix is the strong
  # signal that this is fixture data, not a real credential.
  defp rust_test_literal_re,
    do: ~r/(?i)(?:password|secret|api[_-]?key|token)\s*[:=]\s*["'](?:test|dummy|fake|example|placeholder)[-_].*?["']/

  defp directive_re,
    do: ~r/(?:^|[\s#\/\-;])hypatia:\s*allow\s+([A-Za-z0-9_\*]+)(?:\/([A-Za-z0-9_\*]+))?/i

  defp directive_matches?(line, rule_module, rule_type) do
    case Regex.run(directive_re(), line) do
      nil -> false
      [_full, declared_mod] ->
        # No slash: bare type form `hypatia: allow secret_detected`
        declared_mod in [rule_type, rule_module, "*"]
      [_full, declared_mod, declared_type] ->
        (declared_mod in [rule_module, "*"]) and
          (declared_type in [rule_type, "*"])
    end
  end

  defp universal_excluded?(rel) do
    Enum.any?(@universal_excludes, &String.contains?(rel, &1))
  end

  defp match_exemptions?(rel, exemptions, rule_module, rule_type) do
    rule_map =
      Map.merge(
        Map.get(exemptions, rule_module, %{}),
        Map.get(exemptions, "*", %{}),
        fn _k, v1, v2 -> v1 ++ v2 end
      )

    paths =
      Map.get(rule_map, rule_type, []) ++
        Map.get(rule_map, "*", []) ++
        Map.get(rule_map, :any, [])

    Enum.any?(paths, fn fragment -> String.contains?(rel, fragment) end)
  end

  defp user_exemptions(nil), do: %{}

  defp user_exemptions(repo_path) do
    case File.read(Path.join(repo_path, ".hypatia-ignore")) do
      {:ok, contents} -> parse_user_exemptions(contents)
      _ -> %{}
    end
  end

  # `.hypatia-ignore` format (one per line, # for comments):
  #   <rule_module>/<rule_type>:<path-fragment>
  #   <rule_module>/*:<path-fragment>          (any type in module)
  #   <path-fragment>                           (any rule, any module)
  defp parse_user_exemptions(contents) do
    contents
    |> String.split("\n", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(fn l -> l == "" or String.starts_with?(l, "#") end)
    |> Enum.reduce(%{}, fn line, acc ->
      case String.split(line, ":", parts: 2) do
        [rule_spec, path] ->
          add_user_exemption(acc, rule_spec, String.trim(path))

        [path] ->
          add_user_exemption(acc, "*/*", String.trim(path))
      end
    end)
  end

  defp add_user_exemption(acc, rule_spec, path) do
    case String.split(rule_spec, "/", parts: 2) do
      [mod, type] ->
        update_in(acc, [Access.key(mod, %{}), Access.key(type, [])], &[path | &1])

      [mod] ->
        update_in(acc, [Access.key(mod, %{}), Access.key(:any, [])], &[path | &1])
    end
  end

  defp relative(file, nil), do: file
  defp relative(file, repo_path) do
    cond do
      String.starts_with?(file, repo_path <> "/") ->
        String.replace_prefix(file, repo_path <> "/", "")
      String.starts_with?(file, repo_path) ->
        String.replace_prefix(file, repo_path, "")
      true ->
        file
    end
  end
end

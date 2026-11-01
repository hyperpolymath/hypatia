# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.AuditPrevention do
  @moduledoc """
  Audit-training prevention rules AP001–AP015, transcribed from Logtalk.

  These fifteen rules were the last rule-bearing Logtalk text in this
  repository: the `Prevention Rules (Logtalk)` blocks embedded in
  `.audittraining/security-errors/*.adoc` (2025-12-29). They are recorded
  here verbatim as data (`rules/0` — functor, name, effect, description,
  conditions, action, origin file) so the transcription can be diffed
  against the originals preserved in `docs/rules/logtalk-migration.adoc`.

  Every rule names its canonical Elixir equivalent. Where the estate
  already had a live checker (workflow permissions, unpinned actions,
  Rust panic patterns, RSR self-compliance), this module points at it and
  does not duplicate it. Where no live checker existed, this module
  implements the missing check as a pure function over caller-supplied
  evidence (file lists, file contents, alert/tag/review/score facts).
  Callers opt in with that evidence; `check_all/1` runs every check whose
  evidence is present and stays silent on the rest.

  Two operationalisations are recorded here rather than hidden:

  * The Logtalk records carry an effect and an action but no severity.
    Findings therefore derive severity from the action
    (`block_merge_with_escalation` is `:critical`; other `block_*`
    actions are `:high`; `warn_*`/`alert_*` are `:medium`;
    `warn_and_log` is `:low`). `Hypatia.Rules.SecurityErrors` stays
    canonical for alert-id severities.
  * The property-injection rule's `nearby_pattern` condition is
    operationalised as *same file*: both access patterns must occur in
    one file with no allowlist marker anywhere in that file.
  """

  @rules [
    %{
      id: "AP001",
      rule: "parser_input_limits",
      logtalk_functor: "parser_rule",
      logtalk_name: "input_limits",
      effect: :preventive,
      description: "Config parsers must have input size limits",
      conditions: [
        "crate_type(parser)",
        ~S{not(pattern("MAX.*SIZE|size.*limit"))}
      ],
      action: "warn_reviewer",
      origin: ".audittraining/security-errors/bunsenite.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_parser_input_limits/1"
    },
    %{
      id: "AP002",
      rule: "wasm_memory_limits",
      logtalk_functor: "wasm_rule",
      logtalk_name: "memory_limits",
      effect: :preventive,
      description: "WASM crates must limit memory allocation",
      conditions: [
        ~S{feature("wasm")},
        ~S{not(pattern("memory.*limit|max.*alloc"))}
      ],
      action: "warn_reviewer",
      origin: ".audittraining/security-errors/bunsenite.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_wasm_memory_limits/1"
    },
    %{
      id: "AP003",
      rule: "rust_missing_audit",
      logtalk_functor: "rust_rule",
      logtalk_name: "missing_audit",
      effect: :preventive,
      description: "Rust repos must run cargo audit in CI",
      conditions: [
        ~S{file_exists("Cargo.toml")},
        ~S{not(workflow_contains("cargo audit"))}
      ],
      action: "block_release",
      origin: ".audittraining/security-errors/czech-file-knife.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_missing_cargo_audit/2"
    },
    %{
      id: "AP004",
      rule: "rust_workspace_vuln",
      logtalk_functor: "rust_rule",
      logtalk_name: "workspace_vuln",
      effect: :curative,
      description: "Check all workspace members for vulnerabilities",
      conditions: [
        ~S{file_exists("Cargo.toml")},
        ~S{contains("[workspace]")},
        "any_member_has_vuln"
      ],
      action: "alert_maintainer",
      origin: ".audittraining/security-errors/czech-file-knife.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_workspace_vuln/2"
    },
    %{
      id: "AP005",
      rule: "project_missing_ci_tests",
      logtalk_functor: "project_rule",
      logtalk_name: "missing_ci_tests",
      effect: :preventive,
      description: "Repos must have automated test workflow",
      conditions: [
        ~S{not(file_exists(".github/workflows/*test*.yml"))},
        "has_source_code"
      ],
      action: "block_release",
      origin: ".audittraining/security-errors/gitvisor.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_missing_ci_tests/1"
    },
    %{
      id: "AP006",
      rule: "project_missing_sast",
      logtalk_functor: "project_rule",
      logtalk_name: "missing_sast",
      effect: :preventive,
      description: "Multi-language repos must have SAST configured",
      conditions: [
        "language_count > 1",
        ~S{not(file_exists(".github/workflows/codeql.yml"))}
      ],
      action: "warn_maintainer",
      origin: ".audittraining/security-errors/gitvisor.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_missing_sast/2"
    },
    %{
      id: "AP007",
      rule: "security_property_injection",
      logtalk_functor: "security_rule",
      logtalk_name: "property_injection",
      effect: :preventive,
      description: "Reject dynamic property access without validation",
      conditions: [
        ~S{pattern("\[\s*\w+Input\s*\]")},
        ~S{pattern("\[\s*req\.(body|query|params)")},
        ~S{not(nearby_pattern("allowedProps|whitelist|ALLOWED"))}
      ],
      action: "block_commit",
      origin: ".audittraining/security-errors/polyglot-i18n.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_property_injection/1"
    },
    %{
      id: "AP008",
      rule: "code_quality_unused_vars",
      logtalk_functor: "code_quality_rule",
      logtalk_name: "unused_vars",
      effect: :curative,
      description: "Flag files with unused variables for cleanup",
      conditions: [
        ~S{pattern("is declared but its value is never read")}
      ],
      action: "warn_and_log",
      origin: ".audittraining/security-errors/polyglot-i18n.adoc",
      canonical:
        "Hypatia.Rules.SecurityErrors metadata for \"unused-local-variable\"; the condition is a compiler-diagnostic string, so the language compiler/linter remains the detector"
    },
    %{
      id: "AP009",
      rule: "crypto_enhanced_review",
      logtalk_functor: "crypto_rule",
      logtalk_name: "enhanced_review",
      effect: :preventive,
      description: "Crypto repos require 2+ reviewers",
      conditions: [
        "repo_tag(cryptography)",
        "pr_reviews < 2"
      ],
      action: "block_merge",
      origin: ".audittraining/security-errors/proof-of-work.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_crypto_enhanced_review/2"
    },
    %{
      id: "AP010",
      rule: "crypto_require_fuzzing",
      logtalk_functor: "crypto_rule",
      logtalk_name: "require_fuzzing",
      effect: :preventive,
      description: "Crypto repos must have fuzz targets",
      conditions: [
        "repo_tag(cryptography)",
        ~S{not(dir_exists("fuzz/"))}
      ],
      action: "block_release",
      origin: ".audittraining/security-errors/proof-of-work.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_crypto_require_fuzzing/2"
    },
    %{
      id: "AP011",
      rule: "crypto_no_panic",
      logtalk_functor: "crypto_rule",
      logtalk_name: "no_panic",
      effect: :preventive,
      description: "Crypto code must not panic",
      conditions: [
        "repo_tag(cryptography)",
        ~S{pattern("panic!|unwrap\(\)|expect\(")}
      ],
      action: "warn_reviewer",
      origin: ".audittraining/security-errors/proof-of-work.adoc",
      canonical:
        "Hypatia.Rules.CodeSafety.scan_content/2 Rust patterns :panic_macro, :unwrap_without_check and :expect_in_hot_path (a superset: they fire in every Rust repo, not only cryptography-tagged ones)"
    },
    %{
      id: "AP012",
      rule: "meta_rsr_self_compliance",
      logtalk_functor: "meta_rule",
      logtalk_name: "rsr_self_compliance",
      effect: :preventive,
      description: "RSR tooling repos must pass all RSR checks",
      conditions: [
        "repo_name(supernorma)",
        "has_rsr_violation"
      ],
      action: "block_merge_with_escalation",
      origin: ".audittraining/security-errors/supernorma.adoc",
      canonical:
        "Hypatia.Rules.RsrConformance (the RSR v2.0 oracle) with Hypatia.Rules.Dogfooding self-application checks; generalised from the hard-coded repo_name(supernorma) to every RSR-tooling repo"
    },
    %{
      id: "AP013",
      rule: "template_exemplary_standards",
      logtalk_functor: "template_rule",
      logtalk_name: "exemplary_standards",
      effect: :diagnostic,
      description: "Template/tooling repos should exceed normal standards",
      conditions: [
        "repo_purpose(tooling)",
        "scorecard_score < 8"
      ],
      action: "alert_maintainer",
      origin: ".audittraining/security-errors/supernorma.adoc",
      canonical: "Hypatia.Rules.AuditPrevention.check_exemplary_standards/2"
    },
    %{
      id: "AP014",
      rule: "workflow_missing_permissions",
      logtalk_functor: "workflow_rule",
      logtalk_name: "missing_permissions",
      effect: :preventive,
      description: "Reject workflows without permissions declaration",
      conditions: [
        ~S{file_type(".github/workflows/*.yml")},
        ~S{not(contains("permissions:"))}
      ],
      action: "block_merge",
      origin: ".audittraining/security-errors/ubicity.adoc",
      canonical:
        "Hypatia.Rules.WorkflowAudit.check_permissions/1 and Hypatia.Rules.WorkflowHardening.wh002_excessive_permissions/1"
    },
    %{
      id: "AP015",
      rule: "workflow_unpinned_action",
      logtalk_functor: "workflow_rule",
      logtalk_name: "unpinned_action",
      effect: :preventive,
      description: "Reject workflows with unpinned actions",
      conditions: [
        ~S{pattern("uses:.*@v[0-9]")},
        ~S{not(pattern("#.*v[0-9]"))}
      ],
      action: "block_merge",
      origin: ".audittraining/security-errors/ubicity.adoc",
      canonical:
        "Hypatia.Rules.WorkflowHardening.wh004_unpinned_uses/1 and Hypatia.Rules.WorkflowAudit.check_unpinned_actions/1 (the Logtalk second condition is superseded: a trailing \"# vN\" comment beside a tag ref is not a pin; the Elixir checks require the 40-hex SHA form)"
    }
  ]

  @source_extensions ~w(.rs .ex .exs .erl .gleam .jl .ml .mli .hs .lhs .zig .idr .agda
                        .affine .res .resi .js .mjs .cjs .sh .py .go .java .kt .swift
                        .c .h .cpp .hpp .adb .ads .lean .v)

  @property_input_pattern ~r/\[\s*\w+Input\s*\]/
  @property_request_pattern ~r/\[\s*req\.(body|query|params)/
  @property_allowlist_pattern ~r/allowedProps|whitelist|ALLOWED/

  @doc "The fifteen transcribed rule records, in ledger order (AP001–AP015)."
  def rules, do: @rules

  @doc "One transcribed rule record by AP id or snake-case rule name, or `nil`."
  def rule(id) when is_binary(id) do
    Enum.find(@rules, fn record -> record.id == id or record.rule == id end)
  end

  @doc """
  The canonical Elixir equivalent of a transcribed rule (AP id or
  snake-case rule name), or `nil` for an unknown id.
  """
  def canonical_equivalent(id) when is_binary(id) do
    case rule(id) do
      nil -> nil
      record -> record.canonical
    end
  end

  @doc """
  AP001 — a parser crate whose sources state no input-size limit.

  `crate_type(parser)` is operationalised as a `Cargo.toml` that
  describes the crate with parse/parser/parsing; the negative condition
  keeps the Logtalk pattern verbatim (`MAX.*SIZE|size.*limit`,
  case-insensitive) over every supplied file content.
  """
  def check_parser_input_limits(contents) when is_map(contents) do
    cargo = Map.get(contents, "Cargo.toml", "")
    parser_crate? = Regex.match?(~r/pars(e|er|ing)/i, cargo)

    has_input_limit? =
      contents
      |> Map.values()
      |> Enum.any?(fn content -> Regex.match?(~r/MAX.*SIZE|size.*limit/i, content) end)

    if cargo != "" and parser_crate? and not has_input_limit? do
      [finding(rec("AP001"))]
    else
      []
    end
  end

  @doc """
  AP002 — a WASM crate whose sources state no memory limit.

  `feature("wasm")` is operationalised as a `wasm` entry in the
  `[features]` table, a `wasm-bindgen` dependency, or a `wasm32`
  target mention in `Cargo.toml`; the negative condition keeps the
  Logtalk pattern verbatim (`memory.*limit|max.*alloc`).
  """
  def check_wasm_memory_limits(contents) when is_map(contents) do
    cargo = Map.get(contents, "Cargo.toml", "")

    features =
      case Regex.run(~r/\[features\](.*?)(\n\[|\z)/s, cargo) do
        [_, body | _] -> body
        _ -> ""
      end

    wasm? =
      Regex.match?(~r/\bwasm\b/, features) or
        String.contains?(cargo, "wasm-bindgen") or
        String.contains?(cargo, "wasm32")

    has_memory_limit? =
      contents
      |> Map.values()
      |> Enum.any?(fn content -> Regex.match?(~r/memory.*limit|max.*alloc/i, content) end)

    if cargo != "" and wasm? and not has_memory_limit? do
      [finding(rec("AP002"))]
    else
      []
    end
  end

  @doc """
  AP003 — a Rust repo with no `cargo audit` in any supplied workflow.
  `contents` is keyed by repo-relative path; only entries under
  `.github/workflows/` count as workflow evidence.
  """
  def check_missing_cargo_audit(files, contents)
      when is_list(files) and is_map(contents) do
    workflow_contents =
      for {path, content} <- contents,
          String.starts_with?(path, ".github/workflows/"),
          do: content

    audited? = Enum.any?(workflow_contents, fn content -> String.contains?(content, "cargo audit") end)

    if "Cargo.toml" in files and not audited? do
      [finding(rec("AP003"))]
    else
      []
    end
  end

  @doc """
  AP004 — a Cargo workspace with a known-vulnerable member.
  `any_member_has_vuln` is caller evidence (for example from
  `Hypatia.Rules.DependabotAlerts`); this check contributes only the
  `[workspace]` half of the conjunction.
  """
  def check_workspace_vuln(cargo_toml, any_member_has_vuln) when is_binary(cargo_toml) do
    if String.contains?(cargo_toml, "[workspace]") and any_member_has_vuln == true do
      [finding(rec("AP004"))]
    else
      []
    end
  end

  @doc """
  AP005 — source code with no test workflow. The Logtalk glob
  (`.github/workflows/*test*.yml`) is transcribed semantically: the
  `.yaml` spelling counts too. The ledger records the literal glob.
  """
  def check_missing_ci_tests(files) when is_list(files) do
    test_workflow? =
      Enum.any?(files, fn path ->
        Regex.match?(~r{\A\.github/workflows/[^/]*test[^/]*\.ya?ml\z}i, path)
      end)

    has_source? = Enum.any?(files, fn path -> Path.extname(path) in @source_extensions end)

    if has_source? and not test_workflow? do
      [finding(rec("AP005"))]
    else
      []
    end
  end

  @doc """
  AP006 — a multi-language repo with no CodeQL workflow. The Logtalk
  file test is transcribed semantically: `codeql.yaml` counts too.
  """
  def check_missing_sast(files, language_count) when is_list(files) do
    codeql? =
      Enum.any?(files, fn path ->
        path in [".github/workflows/codeql.yml", ".github/workflows/codeql.yaml"]
      end)

    if is_integer(language_count) and language_count > 1 and not codeql? do
      [finding(rec("AP006"))]
    else
      []
    end
  end

  @doc """
  AP007 — dynamic property access from request input with no allowlist
  nearby (same file). Both Logtalk access patterns must match and the
  allowlist pattern (`allowedProps|whitelist|ALLOWED`) must not.
  """
  def check_property_injection(content) when is_binary(content) do
    if Regex.match?(@property_input_pattern, content) and
         Regex.match?(@property_request_pattern, content) and
         not Regex.match?(@property_allowlist_pattern, content) do
      [Map.put(finding(rec("AP007")), :cwe, "CWE-915")]
    else
      []
    end
  end

  @doc """
  AP009 — a cryptography-tagged repo whose required approving-review
  count is below 2. `pr_reviews < 2` is operationalised as the branch
  protection requirement, matching the audit prose ("Required reviews:
  2 (not 1)"); pair with `Hypatia.Rules.BranchProtection` BP003, which
  enforces the general ≥1 floor.
  """
  def check_crypto_enhanced_review(repo_tags, required_reviews) do
    if cryptography_tagged?(repo_tags) and is_integer(required_reviews) and
         required_reviews < 2 do
      [finding(rec("AP009"))]
    else
      []
    end
  end

  @doc "AP010 — a cryptography-tagged repo with no `fuzz/` tree."
  def check_crypto_require_fuzzing(repo_tags, files) when is_list(files) do
    fuzz? = Enum.any?(files, fn path -> String.starts_with?(path, "fuzz/") end)

    if cryptography_tagged?(repo_tags) and not fuzz? do
      [finding(rec("AP010"))]
    else
      []
    end
  end

  @doc "AP013 — a tooling-purpose repo scoring below 8 on Scorecard."
  def check_exemplary_standards(repo_purpose, scorecard_score) do
    if purpose_string(repo_purpose) == "tooling" and is_number(scorecard_score) and
         scorecard_score < 8 do
      [finding(rec("AP013"))]
    else
      []
    end
  end

  @doc """
  Run every implemented check whose evidence the caller supplied.

  Recognised evidence keys: `:files` (repo-relative paths), `:contents`
  (`%{path => content}`), `:repo_tags`, `:language_count`,
  `:required_reviews`, `:any_member_has_vuln`, `:repo_purpose` and
  `:scorecard_score`. Missing evidence yields no finding for the
  dependent check — never an error, and never a fabricated pass claim:
  findings are the only output. AP007 runs over supplied
  `.js`/`.mjs`/`.cjs` contents. Rules whose canonical equivalent is a
  live checker elsewhere (AP008, AP011, AP012, AP014, AP015) are not
  re-run here.
  """
  def check_all(evidence) when is_map(evidence) do
    files = Map.get(evidence, :files, [])
    contents = Map.get(evidence, :contents, %{})
    repo_tags = Map.get(evidence, :repo_tags, [])

    property_findings =
      Enum.flat_map(contents, fn {path, content} ->
        if javascript_file?(path) and is_binary(content) do
          check_property_injection(content)
        else
          []
        end
      end)

    check_parser_input_limits(contents) ++
      check_wasm_memory_limits(contents) ++
      check_missing_cargo_audit(files, contents) ++
      check_workspace_vuln(
        Map.get(contents, "Cargo.toml", ""),
        Map.get(evidence, :any_member_has_vuln, false)
      ) ++
      check_missing_ci_tests(files) ++
      check_missing_sast(files, Map.get(evidence, :language_count, 0)) ++
      property_findings ++
      check_crypto_enhanced_review(repo_tags, Map.get(evidence, :required_reviews)) ++
      check_crypto_require_fuzzing(repo_tags, files) ++
      check_exemplary_standards(
        Map.get(evidence, :repo_purpose),
        Map.get(evidence, :scorecard_score)
      )
  end

  defp rec(id), do: Enum.find(@rules, fn record -> record.id == id end)

  defp finding(record) do
    %{
      rule: record.id,
      type: record.rule,
      effect: record.effect,
      severity: action_severity(record.action),
      action: record.action,
      description: record.description,
      source: record.origin
    }
  end

  defp action_severity("block_merge_with_escalation"), do: :critical

  defp action_severity(action)
       when action in ["block_commit", "block_merge", "block_release"],
       do: :high

  defp action_severity("warn_and_log"), do: :low
  defp action_severity(_action), do: :medium

  defp cryptography_tagged?(tags) when is_list(tags) do
    Enum.any?(tags, fn tag -> tag in [:cryptography, "cryptography"] end)
  end

  defp cryptography_tagged?(_tags), do: false

  defp javascript_file?(path) when is_binary(path) do
    Path.extname(path) in [".js", ".mjs", ".cjs"]
  end

  defp javascript_file?(_path), do: false

  defp purpose_string(value) when is_binary(value), do: value
  defp purpose_string(value) when is_atom(value), do: Atom.to_string(value)
  defp purpose_string(_value), do: ""
end

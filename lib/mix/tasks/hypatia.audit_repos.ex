# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Mix.Tasks.Hypatia.AuditRepos do
  @moduledoc """
  Audit hyperpolymath repos for description / tag / README quality.

  Ported from `batch-ops/audit-repos.jl` (issue #176).

  ## Usage

      mix hypatia.audit_repos                # Full audit
      mix hypatia.audit_repos --descriptions # Description issues only
      mix hypatia.audit_repos --tags         # Tag issues only
      mix hypatia.audit_repos --readme       # README issues only
      mix hypatia.audit_repos --fix          # Generate fix-suggestion commands

  Walks every directory under `$HYPATIA_REPOS_DIR` (default `~/repos`) that
  contains a `.git` directory, queries the GitHub API via `gh` for
  description + topics, inspects the local tree for languages and README
  content, and reports issues by severity. Full JSON results are saved to
  `~/.git-private-farm/audit-results.json`.

  ## Exit codes

  Non-zero only on hard failure (bad args, unreadable repos dir). Finding
  issues does not fail the task — this is a reporting tool.
  """

  use Mix.Task

  @shortdoc "Audit repos for description/tag/README quality"

  @github_org "hyperpolymath"
  @repos_dir_env "HYPATIA_REPOS_DIR"
  @default_repos_dir "~/repos"
  @default_output "~/.git-private-farm/audit-results.json"

  # Generic/boilerplate description patterns (Regex.compile_all would be nicer
  # but these are compile-time literals — fine as-is).
  @generic_patterns [
    ~r/^$/,
    ~r/^null$/i,
    ~r/^No description/i,
    ~r/^A?\s*(simple|basic)\s*/i,
    ~r/^This (is|repo|repository)/i,
    ~r/^TODO/i,
    ~r/^WIP/i,
    ~r/^Template/i,
    ~r/^Placeholder/i,
    ~r/^Test/i,
    ~r/^Example/i
  ]

  @standards_only_patterns [
    ~r/^RSR\s*(compliant|standard|implementation)$/i,
    ~r/^(OpenSSF|SLSA)\s*compliant$/i,
    # Rust / Deno / OCaml / etc — ReScript removed 2026-04 (retired language).
    ~r/^(Rust|Deno|Elixir|Haskell|OCaml|Zig|Idris2)\s*(project|repo|repository)?$/i,
    ~r/^Built with/i,
    ~r/^Uses?/i,
    ~r/^Implements?\s+(the\s+)?[\w-]+\s*standard$/i
  ]

  @template_markers [
    ~r/^= Project Name$/m,
    ~r/^# Project Name$/m,
    ~r/^TODO:?\s*/im,
    ~r/^\[Insert/im,
    ~r/^Replace this/im,
    ~r/^\{\{.*\}\}/m
  ]

  @language_indicators %{
    "rust" => ["Cargo.toml"],
    # rescript removed 2026-04 (retired from allowed languages).
    "deno" => ["deno.json", "deno.jsonc"],
    "gleam" => ["gleam.toml"],
    "ocaml" => ["dune-project"],
    "haskell" => [".cabal"],
    "elixir" => ["mix.exs"],
    "nickel" => ["*.ncl"],
    "julia" => ["Project.toml"]
  }

  @banned_compliance_tags ~w(rhodium-standard rsr rsr-compliant openssf-scorecard)
  @language_topic_tags ~w(rust deno gleam ocaml haskell elixir zig idris2 ephapax gossamer)

  @domain_tags %{
    "ssg" => ~w(static-site-generator web),
    "mcp" => ~w(model-context-protocol ai-tooling),
    "bot" => ~w(automation bot),
    "cli" => ~w(command-line cli),
    "api" => ~w(api backend),
    "db" => ~w(database),
    "auth" => ~w(authentication security),
    "crypto" => ~w(cryptography security),
    "config" => ~w(configuration),
    "i18n" => ~w(internationalization localization),
    "playground" => ~w(learning experimental),
    "spec" => ~w(specification documentation),
    "standard" => ~w(specification standards),
    "template" => ~w(template scaffolding),
    "wasm" => ~w(webassembly),
    "web" => ~w(web),
    "mobile" => ~w(mobile),
    "desktop" => ~w(desktop),
    "cloud" => ~w(cloud infrastructure),
    "k8s" => ~w(kubernetes containers),
    "container" => ~w(containers docker),
    "network" => ~w(networking),
    "security" => ~w(security),
    "verify" => ~w(verification formal-methods),
    "proof" => ~w(formal-methods verification),
    "llm" => ~w(ai llm),
    "ml" => ~w(machine-learning ai),
    "git" => ~w(git version-control),
    "zotero" => ~w(zotero research bibliography)
  }

  @min_tags 4
  @max_tags 8
  @min_readme_lines 10

  @impl Mix.Task
  def run(args) do
    {opts, _rest, _invalid} =
      OptionParser.parse(args,
        switches: [descriptions: :boolean, tags: :boolean, readme: :boolean, fix: :boolean]
      )

    filter = filter_from_opts(opts)
    fix_mode? = Keyword.get(opts, :fix, false)

    issues = audit_all_repos(filter)

    if fix_mode? do
      print_fix_suggestions(issues)
    else
      generate_report(issues)
    end
  end

  defp filter_from_opts(opts) do
    cond do
      Keyword.get(opts, :descriptions) -> :descriptions
      Keyword.get(opts, :tags) -> :tags
      Keyword.get(opts, :readme) -> :readme
      true -> nil
    end
  end

  # ---------------------------------------------------------------------

  defp audit_all_repos(filter) do
    repos_dir = repos_dir()
    repos = list_local_repos(repos_dir)
    Mix.shell().info("Auditing #{length(repos)} repositories...")

    issues =
      repos
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {repo, i} ->
        IO.write("\r[#{i}/#{length(repos)}] Auditing: #{repo}                    ")
        result = audit_repo(repo)
        # Rate-limit gh API calls — Julia used sleep(0.1).
        Process.sleep(100)
        filter_issues(result, filter)
      end)

    IO.write("\n")
    issues
  end

  defp list_local_repos(repos_dir) do
    case File.ls(repos_dir) do
      {:ok, entries} ->
        entries
        |> Enum.filter(fn name ->
          File.dir?(Path.join([repos_dir, name, ".git"]))
        end)
        |> Enum.sort()

      {:error, reason} ->
        Mix.raise("Cannot list #{repos_dir}: #{inspect(reason)}")
    end
  end

  defp audit_repo(repo_name) do
    repo_path = Path.join(repos_dir(), repo_name)
    info = get_repo_info(repo_name)
    topics = get_repo_topics(repo_name)
    detected_langs = detect_languages_local(repo_path)

    description_issues = description_issues(repo_name, info)
    tag_issues = check_tags(repo_name, topics, detected_langs)

    readme_issues =
      if File.dir?(repo_path), do: check_readme(repo_path, repo_name), else: []

    description_issues ++ tag_issues ++ readme_issues
  end

  # --- GitHub API via gh -----------------------------------------------

  defp get_repo_info(repo_name) do
    api_path = "repos/" <> @github_org <> "/" <> repo_name

    case System.cmd("gh", ["api", api_path], stderr_to_stdout: true) do
      {out, 0} ->
        case Jason.decode(out) do
          {:ok, map} -> map
          _ -> nil
        end

      _ ->
        nil
    end
  end

  defp get_repo_topics(repo_name) do
    api_path = "repos/" <> @github_org <> "/" <> repo_name <> "/topics"

    case System.cmd("gh", ["api", api_path], stderr_to_stdout: true) do
      {out, 0} ->
        case Jason.decode(out) do
          {:ok, %{"names" => names}} when is_list(names) -> names
          _ -> []
        end

      _ ->
        []
    end
  end

  # --- Description checks ----------------------------------------------

  defp description_issues(_repo_name, nil), do: []

  defp description_issues(repo_name, info) do
    description = Map.get(info, "description")

    if no_description?(description) do
      [
        issue(repo_name, :no_description, :critical,
          "Repository has no description",
          "Add a description that explains the project's purpose"
        )
      ]
    else
      desc = String.trim(description)

      # Three independent checks; any combination can fire.
      [
        if(generic_description?(desc),
          do:
            issue(repo_name, :generic_description, :warning,
              "Description is generic: \"#{desc}\"",
              "Replace with specific project purpose and functionality"
            )
        ),
        if(standards_only?(desc),
          do:
            issue(repo_name, :standards_only_description, :warning,
              "Description only mentions standards/technology: \"#{desc}\"",
              "Focus on WHAT the project does, not just how it's built"
            )
        ),
        description_mismatch_issue(repo_name, desc)
      ]
      |> Enum.reject(&is_nil/1)
    end
  end

  defp no_description?(nil), do: true
  defp no_description?(""), do: true
  defp no_description?(d) when is_binary(d), do: String.trim(d) == ""
  defp no_description?(_), do: true

  defp generic_description?(desc) do
    Enum.any?(@generic_patterns, &Regex.match?(&1, desc))
  end

  defp standards_only?(desc) do
    Enum.any?(@standards_only_patterns, &Regex.match?(&1, desc))
  end

  defp description_mismatch_issue(repo_name, desc) do
    desc_lower = String.downcase(desc)

    name_parts =
      repo_name
      |> String.replace(["-", "_"], " ")
      |> String.split()

    significant =
      Enum.reject(name_parts, fn p ->
        String.length(p) <= 2 or p in ~w(the and for mcp ssg)
      end)

    if significant == [] do
      nil
    else
      matches =
        Enum.count(significant, fn p -> String.contains?(desc_lower, String.downcase(p)) end)

      if matches == 0 do
        issue(repo_name, :description_name_mismatch, :notice,
          "Description doesn't reference any key terms from repo name: #{Enum.join(significant, ", ")}",
          "Ensure description reflects the project name's purpose"
        )
      else
        nil
      end
    end
  end

  # --- Tag checks ------------------------------------------------------

  defp check_tags(repo_name, topics, detected_languages) do
    content_tags =
      Enum.reject(topics, fn t ->
        String.downcase(t) in ["rhodium", "rhodium-standard", "rsr", "rsr-compliant"]
      end)

    has_rhodium = Enum.any?(topics, &(String.downcase(&1) == "rhodium"))

    bad_compliance_tags =
      Enum.filter(topics, &(String.downcase(&1) in @banned_compliance_tags))

    issues = []

    issues =
      if bad_compliance_tags != [] do
        [
          issue(repo_name, :insufficient_tags, :notice,
            "Redundant compliance tags: #{Enum.join(bad_compliance_tags, ", ")}",
            "Use only 'rhodium' tag for RSR compliance (remove: #{Enum.join(bad_compliance_tags, ", ")})"
          )
          | issues
        ]
      else
        issues
      end

    cond do
      topics == [] ->
        [
          issue(repo_name, :no_tags, :warning,
            "No topics/tags set for repository",
            "Add 'rhodium' + #{@min_tags}-#{@max_tags} content tags: #{Enum.join(suggest_tags(repo_name, detected_languages), ", ")}"
          )
          | issues
        ]

      true ->
        issues_after_rhodium =
          if not has_rhodium do
            [
              issue(repo_name, :insufficient_tags, :notice,
                "Missing 'rhodium' compliance tag",
                "Add 'rhodium' tag for RSR compliance"
              )
              | issues
            ]
          else
            issues
          end

        issues_after_content =
          if length(content_tags) < @min_tags do
            [
              issue(repo_name, :insufficient_tags, :notice,
                "Only #{length(content_tags)} content tags (recommended: #{@min_tags}-#{@max_tags})",
                "Add content-specific tags: #{Enum.join(suggest_tags(repo_name, detected_languages), ", ")}"
              )
              | issues_after_rhodium
            ]
          else
            issues_after_rhodium
          end

        is_lang_focused = language_focused?(repo_name)

        has_lang_tag =
          Enum.any?(topics, &(String.downcase(&1) in @language_topic_tags))

        if has_lang_tag and not is_lang_focused do
          [
            issue(repo_name, :missing_language_tag, :notice,
              "Has language tags but repo isn't language-focused",
              "Consider removing language tags unless project is FFI/ABI/SSG/playground"
            )
            | issues_after_content
          ]
        else
          issues_after_content
        end
    end
  end

  defp suggest_tags(repo_name, detected_languages) do
    base = ["rhodium"]

    lang_additions =
      if language_focused?(repo_name), do: detected_languages, else: []

    name_parts =
      repo_name
      |> String.replace(["-", "_"], " ")
      |> String.downcase()
      |> String.split()

    domain_additions =
      name_parts
      |> Enum.flat_map(fn part -> Map.get(@domain_tags, part, []) end)

    (base ++ lang_additions ++ domain_additions) |> Enum.uniq()
  end

  defp language_focused?(repo_name) do
    String.contains?(repo_name, "playground") or
      String.contains?(repo_name, "-ffi") or
      String.contains?(repo_name, "-abi") or
      String.ends_with?(repo_name, "-ssg") or
      String.contains?(repo_name, "lang") or
      String.contains?(repo_name, "compiler")
  end

  # --- README checks ---------------------------------------------------

  defp check_readme(repo_path, repo_name) do
    readme_path = find_readme(repo_path)

    if is_nil(readme_path) do
      [
        issue(repo_name, :no_readme, :critical,
          "No README file found",
          "Create README.adoc with project description, installation, and usage"
        )
      ]
    else
      content = File.read!(readme_path)

      non_empty_lines =
        content
        |> String.split("\n")
        |> Enum.reject(fn l -> String.trim(l) == "" end)

      readme_count_issues =
        if length(non_empty_lines) < @min_readme_lines do
          [
            issue(repo_name, :underdeveloped_readme, :warning,
              "README has only #{length(non_empty_lines)} non-empty lines (minimum: #{@min_readme_lines})",
              "Expand README with: overview, installation, usage examples, contributing"
            )
          ]
        else
          []
        end

      template_issues =
        if Enum.any?(@template_markers, &Regex.match?(&1, content)) do
          [
            issue(repo_name, :readme_is_template, :critical,
              "README appears to be an unfilled template",
              "Replace template placeholders with actual project content"
            )
          ]
        else
          []
        end

      readme_count_issues ++ template_issues
    end
  end

  defp find_readme(repo_path) do
    Enum.find_value(
      ~w(README.adoc README.md readme.adoc readme.md),
      fn candidate ->
        p = Path.join(repo_path, candidate)
        if File.regular?(p), do: p, else: nil
      end
    )
  end

  # --- Language detection ----------------------------------------------

  defp detect_languages_local(repo_path) do
    case File.ls(repo_path) do
      {:ok, entries} ->
        Enum.reduce(@language_indicators, [], fn {lang, files}, acc ->
          if language_present?(files, entries, repo_path) do
            [lang | acc]
          else
            acc
          end
        end)
        |> Enum.uniq()

      _ ->
        []
    end
  end

  defp language_present?(files, entries, repo_path) do
    Enum.any?(files, fn file ->
      if String.contains?(file, "*") do
        pattern = String.replace(file, "*", "")
        Enum.any?(entries, &String.ends_with?(&1, pattern))
      else
        File.regular?(Path.join(repo_path, file))
      end
    end)
  end

  # --- Report / output -------------------------------------------------

  defp filter_issues(issues, nil), do: issues

  defp filter_issues(issues, :descriptions) do
    keep = [:no_description, :generic_description, :standards_only_description, :description_name_mismatch]
    Enum.filter(issues, &(&1.issue_type in keep))
  end

  defp filter_issues(issues, :tags) do
    keep = [:no_tags, :insufficient_tags, :missing_language_tag]
    Enum.filter(issues, &(&1.issue_type in keep))
  end

  defp filter_issues(issues, :readme) do
    keep = [:no_readme, :underdeveloped_readme, :readme_is_template]
    Enum.filter(issues, &(&1.issue_type in keep))
  end

  defp generate_report(issues) do
    sep = String.duplicate("=", 70)
    hr = String.duplicate("-", 70)

    IO.puts("\n" <> sep)
    IO.puts("                    REPOSITORY AUDIT REPORT")
    IO.puts(sep)
    IO.puts("Generated: #{DateTime.utc_now() |> DateTime.to_string()}")
    IO.puts("Total issues found: #{length(issues)}")
    IO.puts("")

    critical = Enum.filter(issues, &(&1.severity == :critical))
    warning = Enum.filter(issues, &(&1.severity == :warning))
    notice = Enum.filter(issues, &(&1.severity == :notice))

    IO.puts("By Severity:")
    IO.puts("  🔴 Critical: #{length(critical)}")
    IO.puts("  🟡 Warning:  #{length(warning)}")
    IO.puts("  🔵 Notice:   #{length(notice)}")
    IO.puts("")

    IO.puts("By Issue Type:")

    issues
    |> Enum.reduce(%{}, fn i, acc -> Map.update(acc, i.issue_type, 1, &(&1 + 1)) end)
    |> Enum.sort_by(fn {_k, v} -> -v end)
    |> Enum.each(fn {t, c} -> IO.puts("  #{t}: #{c}") end)

    IO.puts("")

    if critical != [] do
      IO.puts(hr)
      IO.puts("CRITICAL ISSUES (Immediate Action Required)")
      IO.puts(hr)

      Enum.each(critical, fn i ->
        IO.puts("• #{i.repo}")
        IO.puts("  Issue: #{i.details}")
        IO.puts("  Fix: #{i.suggestion}")
        IO.puts("")
      end)
    end

    if warning != [] do
      IO.puts(hr)
      IO.puts("WARNING ISSUES (Top 20)")
      IO.puts(hr)

      warning
      |> Enum.take(20)
      |> Enum.each(fn i -> IO.puts("• #{i.repo}: #{i.details}") end)

      extra = length(warning) - min(20, length(warning))
      if extra > 0, do: IO.puts("  ... and #{extra} more")
      IO.puts("")
    end

    save_results(issues)
  end

  defp print_fix_suggestions(issues) do
    IO.puts("\n=== FIX SUGGESTIONS ===\n")

    issues
    |> Enum.filter(&(&1.severity == :critical))
    |> Enum.each(fn i ->
      IO.puts(
        "gh repo edit #{@github_org}/#{i.repo} --description \"<TODO: describe #{i.repo}>\""
      )
    end)
  end

  defp save_results(issues) do
    output = Path.expand(Keyword.get([], :output, @default_output))
    File.mkdir_p!(Path.dirname(output))

    payload = %{
      "timestamp" => DateTime.utc_now() |> DateTime.to_string(),
      "total_issues" => length(issues),
      "issues" =>
        Enum.map(issues, fn i ->
          %{
            "repo" => i.repo,
            "issue_type" => Atom.to_string(i.issue_type),
            "severity" => Atom.to_string(i.severity),
            "details" => i.details,
            "suggestion" => i.suggestion
          }
        end)
    }

    File.write!(output, Jason.encode!(payload, pretty: true))
    IO.puts("Full results saved to: #{output}")
  end

  # --- Helpers ---------------------------------------------------------

  defp issue(repo, type, severity, details, suggestion) do
    %{repo: repo, issue_type: type, severity: severity, details: details, suggestion: suggestion}
  end

  defp repos_dir do
    Path.expand(System.get_env(@repos_dir_env) || @default_repos_dir)
  end
end

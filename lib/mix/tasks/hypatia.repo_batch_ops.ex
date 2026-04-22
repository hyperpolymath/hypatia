# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Mix.Tasks.Hypatia.RepoBatchOps do
  @moduledoc """
  Checklist-based batch operations across supervised repos.

  Ported from `batch-ops/repo-batch-ops.jl` (issue #176).

  ## Usage

      mix hypatia.repo_batch_ops --list-repos           # Show all repos with metadata
      mix hypatia.repo_batch_ops --select               # Interactive selection
      mix hypatia.repo_batch_ops --checklist            # Show available operations
      mix hypatia.repo_batch_ops --run <operation>      # Run op on selected repos
      mix hypatia.repo_batch_ops --dry-run <operation>  # Preview without changes

  ## State files

  * `$HYPATIA_REPOS_DIR` (default `~/repos`) — where repos live locally.
  * `~/.git-private-farm/selected-repos.json` — last selection, used by
    `--run` / `--dry-run`.
  * `~/.git-private-farm/operations.log` — append-only log of every run.

  ## Available operations

  Structure: `structure-init`, `move-scm`, `move-build`, `create-licenses`
  Cleanup: `remove-jekyll`, `remove-typescript`, `remove-python`, `remove-go`, `remove-makefile`
  Workflows: `add-workflows`, `fix-permissions`, `pin-actions`, `add-spdx`, `fix-codeql`
  Docs: `add-security`, `add-contributing`, `update-readme`
  Sync: `mirror-setup`, `push-all`, `sync-diverged`

  Operations without a concrete implementation in `execute_operation/2`
  print `"Operation not yet implemented"` and are no-ops — matches the
  Julia source's behaviour.
  """

  use Mix.Task

  @shortdoc "Checklist-based batch operations across supervised repos"

  @repos_dir_env "HYPATIA_REPOS_DIR"
  @default_repos_dir "~/repos"
  @selection_file "~/.git-private-farm/selected-repos.json"
  @operations_log "~/.git-private-farm/operations.log"

  @operations %{
    # Structure
    "structure-init" => %{
      name: "Initialize RSR Structure",
      description: "Create .machine_readable/, tasks/, licenses/ directories",
      category: "structure"
    },
    "move-scm" => %{
      name: "Move SCM Files",
      description: "Move *.scm from root to .machine_readable/6scm/",
      category: "structure"
    },
    "move-build" => %{
      name: "Move Build Files",
      description: "Move Justfile/Mustfile to tasks/",
      category: "structure"
    },
    "create-licenses" => %{
      name: "Create License Structure",
      description: "Add licenses/ with MPL-2.0 and Palimpsest in EN/NL",
      category: "structure"
    },
    # Cleanup
    "remove-jekyll" => %{
      name: "Remove Jekyll",
      description: "Delete Jekyll workflows, Gemfile, _config.yml",
      category: "cleanup"
    },
    "remove-typescript" => %{
      name: "Remove TypeScript",
      description: "Flag/remove *.ts files (requires migration)",
      category: "cleanup"
    },
    "remove-python" => %{
      name: "Remove Python",
      description: "Flag/remove *.py files (requires migration)",
      category: "cleanup"
    },
    "remove-go" => %{
      name: "Remove Go",
      description: "Flag/remove *.go files (requires migration)",
      category: "cleanup"
    },
    "remove-makefile" => %{
      name: "Remove Makefiles",
      description: "Delete Makefile, replace with Justfile",
      category: "cleanup"
    },
    # Workflows
    "add-workflows" => %{
      name: "Add Standard Workflows",
      description: "Add/update all RSR-compliant workflows",
      category: "workflows"
    },
    "fix-permissions" => %{
      name: "Fix Workflow Permissions",
      description: "Add 'permissions: read-all' to all workflows",
      category: "workflows"
    },
    "pin-actions" => %{
      name: "Pin GitHub Actions",
      description: "Replace version tags with SHA pins",
      category: "workflows"
    },
    "add-spdx" => %{
      name: "Add SPDX Headers",
      description: "Add SPDX-License-Identifier to all files",
      category: "workflows"
    },
    "fix-codeql" => %{
      name: "Fix CodeQL Languages",
      description: "Set CodeQL matrix to actual repo languages",
      category: "workflows"
    },
    # Docs
    "add-security" => %{
      name: "Add SECURITY.md",
      description: "Add security policy document",
      category: "docs"
    },
    "add-contributing" => %{
      name: "Add CONTRIBUTING.md",
      description: "Add contribution guidelines",
      category: "docs"
    },
    "update-readme" => %{
      name: "Update README badges",
      description: "Add/update OpenSSF, license badges",
      category: "docs"
    },
    # Sync
    "mirror-setup" => %{
      name: "Setup Mirroring",
      description: "Configure GitLab/Bitbucket mirror vars",
      category: "sync"
    },
    "push-all" => %{
      name: "Push All Changes",
      description: "Git push to origin for all selected repos",
      category: "sync"
    },
    "sync-diverged" => %{
      name: "Sync Diverged Repos",
      description: "Resolve diverged branches with upstream",
      category: "sync"
    }
  }

  @language_indicators %{
    "rust" => ["Cargo.toml"],
    # rescript removed 2026-04 (retired from allowed languages).
    "deno" => ["deno.json", "deno.jsonc"],
    "gleam" => ["gleam.toml"],
    "ocaml" => ["dune-project"],
    "haskell" => [".cabal"],
    "elixir" => ["mix.exs"],
    "nickel" => ["*.ncl"],
    "julia" => ["Project.toml"],
    "typescript" => ["tsconfig.json"],
    "python" => ["pyproject.toml", "setup.py", "requirements.txt"],
    "go" => ["go.mod"],
    "ruby" => ["Gemfile"]
  }

  @required_scm ~w(META.scm STATE.scm ECOSYSTEM.scm PLAYBOOK.scm AGENTIC.scm NEUROSYM.scm)

  @impl Mix.Task
  def run(args) do
    case args do
      [] ->
        print_usage()

      ["--list-repos"] ->
        list_repos()

      ["--select"] ->
        interactive_select()

      ["--checklist"] ->
        list_operations()

      ["--run", operation] ->
        run_operation(operation, false)

      ["--dry-run", operation] ->
        run_operation(operation, true)

      _ ->
        IO.puts("Unknown command: #{Enum.join(args, " ")}")
        print_usage()
    end
  end

  defp print_usage do
    IO.puts("""
    Usage:
      mix hypatia.repo_batch_ops --list-repos           # Show all repos
      mix hypatia.repo_batch_ops --select               # Interactive selection
      mix hypatia.repo_batch_ops --checklist            # Show operations
      mix hypatia.repo_batch_ops --run <op>             # Run operation
      mix hypatia.repo_batch_ops --dry-run <op>         # Preview operation
    """)
  end

  # --- Repo scanning ---------------------------------------------------

  defp list_repos do
    repos_dir = repos_dir()
    IO.puts("Scanning repos in #{repos_dir}...")

    repos =
      repos_dir
      |> File.ls!()
      |> Enum.map(&Path.join(repos_dir, &1))
      |> Enum.map(&scan_repo/1)
      |> Enum.reject(&is_nil/1)

    IO.puts("\n=== Repository Summary ===\n")
    IO.puts("Total repos: #{length(repos)}")

    types =
      Enum.reduce(repos, %{}, fn r, acc -> Map.update(acc, r.type, 1, &(&1 + 1)) end)

    IO.puts("\nBy Type:")

    types
    |> Enum.sort_by(fn {_t, c} -> -c end)
    |> Enum.each(fn {t, c} -> IO.puts("  #{t}: #{c}") end)

    banned_counts = %{
      typescript: Enum.count(repos, & &1.has_typescript),
      python: Enum.count(repos, & &1.has_python),
      go: Enum.count(repos, & &1.has_go),
      ruby: Enum.count(repos, & &1.has_ruby),
      makefile: Enum.count(repos, & &1.has_makefile),
      jekyll: Enum.count(repos, & &1.has_jekyll)
    }

    IO.puts("\nBanned Artifacts:")
    IO.puts("  TypeScript: #{banned_counts.typescript}")
    IO.puts("  Python:     #{banned_counts.python}")
    IO.puts("  Go:         #{banned_counts.go}")
    IO.puts("  Ruby:       #{banned_counts.ruby}")
    IO.puts("  Makefile:   #{banned_counts.makefile}")
    IO.puts("  Jekyll:     #{banned_counts.jekyll}")

    missing = Enum.count(repos, &(&1.missing_scm != []))
    IO.puts("\nMissing SCM files: #{missing} repos")

    repos
  end

  defp scan_repo(repo_path) do
    if File.dir?(Path.join(repo_path, ".git")) do
      name = Path.basename(repo_path)
      languages = detect_languages(repo_path)
      type = detect_type(repo_path, name)

      %{
        name: name,
        path: repo_path,
        languages: languages,
        ecosystem: detect_ecosystem(repo_path),
        type: type,
        has_jekyll: has_jekyll?(repo_path),
        has_typescript: has_banned_ext?(repo_path, [".ts", ".tsx"]),
        has_python: has_banned_ext?(repo_path, [".py"]),
        has_go: File.regular?(Path.join(repo_path, "go.mod")),
        has_ruby: File.regular?(Path.join(repo_path, "Gemfile")),
        has_makefile:
          File.regular?(Path.join(repo_path, "Makefile")) or
            File.regular?(Path.join(repo_path, "makefile")),
        missing_scm: missing_scm(repo_path),
        missing_workflows: [],
        last_commit: last_commit(repo_path),
        branch: current_branch(repo_path)
      }
    else
      nil
    end
  end

  defp detect_languages(repo_path) do
    case File.ls(repo_path) do
      {:ok, entries} ->
        @language_indicators
        |> Enum.reduce([], fn {lang, files}, acc ->
          if Enum.any?(files, fn f ->
               if String.contains?(f, "*") do
                 pattern = String.replace(f, "*", "")
                 Enum.any?(entries, &String.ends_with?(&1, pattern))
               else
                 File.regular?(Path.join(repo_path, f))
               end
             end) do
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

  defp detect_type(repo_path, name) do
    cond do
      String.ends_with?(name, "-ssg") ->
        "ssg"

      String.ends_with?(name, "-mcp") ->
        "mcp"

      String.contains?(name, "spec") or String.contains?(name, "standard") ->
        "spec"

      File.regular?(Path.join(repo_path, "Cargo.toml")) ->
        case File.read(Path.join(repo_path, "Cargo.toml")) do
          {:ok, contents} ->
            if String.contains?(contents, "[[bin]]"), do: "application", else: "library"

          _ ->
            "library"
        end

      File.dir?(Path.join(repo_path, "src")) ->
        "library"

      true ->
        "unknown"
    end
  end

  defp has_banned_ext?(repo_path, exts) do
    # Recursive scan — Julia original used walkdir; we use Path.wildcard/1
    # per extension to stay pure-Elixir (no shell out).
    Enum.any?(exts, fn ext ->
      pattern = Path.join(repo_path, "**/*" <> ext)
      Path.wildcard(pattern) != []
    end)
  end

  defp has_jekyll?(repo_path) do
    [
      "_config.yml",
      ".github/workflows/jekyll.yml",
      ".github/workflows/jekyll-gh-pages.yml"
    ]
    |> Enum.any?(fn f -> File.regular?(Path.join(repo_path, f)) end)
  end

  defp missing_scm(repo_path) do
    scm_dir = Path.join([repo_path, ".machine_readable", "6scm"])

    if File.dir?(scm_dir) do
      Enum.reject(@required_scm, &File.regular?(Path.join(scm_dir, &1)))
    else
      @required_scm
    end
  end

  defp last_commit(repo_path) do
    case System.cmd("git", ["-C", repo_path, "log", "-1", "--format=%ci"],
           stderr_to_stdout: true
         ) do
      {out, 0} -> String.trim(out)
      _ -> ""
    end
  end

  defp current_branch(repo_path) do
    case System.cmd("git", ["-C", repo_path, "branch", "--show-current"],
           stderr_to_stdout: true
         ) do
      {out, 0} -> String.trim(out)
      _ -> "unknown"
    end
  end

  defp detect_ecosystem(repo_path) do
    eco_file = Path.join([repo_path, ".machine_readable", "6scm", "ECOSYSTEM.scm"])

    if File.regular?(eco_file) do
      case Regex.run(~r/type\s+"([^"]+)"/, File.read!(eco_file)) do
        [_, match] -> match
        _ -> "unknown"
      end
    else
      "unknown"
    end
  end

  # --- Interactive selection -------------------------------------------

  defp interactive_select do
    repos = list_repos()

    IO.puts("\n=== Selection Filters ===")
    IO.puts("1. All repos")
    IO.puts("2. By language")
    IO.puts("3. By type (ssg, mcp, library, etc.)")
    IO.puts("4. Has banned languages")
    IO.puts("5. Has Jekyll")
    IO.puts("6. Missing SCM files")
    IO.puts("7. Specific repos (comma-separated)")

    choice = prompt("\nSelect filter [1-7]: ")

    selected =
      case choice do
        "1" ->
          Enum.map(repos, & &1.name)

        "2" ->
          lang = prompt("Enter language (rust, deno, etc.): ")
          repos |> filter_repos(language: lang) |> Enum.map(& &1.name)

        "3" ->
          t = prompt("Enter type (ssg, mcp, library, application, spec): ")
          repos |> filter_repos(type: t) |> Enum.map(& &1.name)

        "4" ->
          repos |> filter_repos(has_banned: true) |> Enum.map(& &1.name)

        "5" ->
          repos |> filter_repos(has_jekyll: true) |> Enum.map(& &1.name)

        "6" ->
          repos |> filter_repos(missing_scm: true) |> Enum.map(& &1.name)

        "7" ->
          names = prompt("Enter repo names (comma-separated): ")
          names |> String.split(",") |> Enum.map(&String.trim/1) |> Enum.reject(&(&1 == ""))

        _ ->
          []
      end

    IO.puts("\nSelected #{length(selected)} repos:")
    selected |> Enum.take(10) |> Enum.each(&IO.puts("  - #{&1}"))
    if length(selected) > 10, do: IO.puts("  ... and #{length(selected) - 10} more")

    if prompt("\nSave selection? [y/N]: ") |> String.downcase() == "y" do
      save_selection(selected)
    end

    selected
  end

  defp filter_repos(repos, opts) do
    Enum.reduce(opts, repos, fn
      {:language, ""}, acc -> acc
      {:language, lang}, acc -> Enum.filter(acc, &(lang in &1.languages))
      {:type, ""}, acc -> acc
      {:type, t}, acc -> Enum.filter(acc, &(&1.type == t))
      {:ecosystem, ""}, acc -> acc
      {:ecosystem, e}, acc -> Enum.filter(acc, &(&1.ecosystem == e))
      {:has_banned, true}, acc ->
        Enum.filter(acc, fn r ->
          r.has_typescript or r.has_python or r.has_go or r.has_ruby or r.has_makefile
        end)
      {:has_jekyll, true}, acc -> Enum.filter(acc, & &1.has_jekyll)
      {:missing_scm, true}, acc -> Enum.filter(acc, &(&1.missing_scm != []))
      _, acc -> acc
    end)
  end

  defp prompt(msg) do
    IO.write(msg)
    IO.read(:stdio, :line) |> to_string() |> String.trim()
  end

  defp save_selection(repos) do
    path = Path.expand(@selection_file)
    File.mkdir_p!(Path.dirname(path))

    payload = %{
      "repos" => repos,
      "timestamp" => DateTime.utc_now() |> DateTime.to_string()
    }

    File.write!(path, Jason.encode!(payload, pretty: true))
    IO.puts("Saved #{length(repos)} repos to selection")
  end

  defp load_selection do
    path = Path.expand(@selection_file)

    if File.regular?(path) do
      case Jason.decode(File.read!(path)) do
        {:ok, %{"repos" => repos}} when is_list(repos) -> repos
        _ -> []
      end
    else
      []
    end
  end

  # --- Operation listing -----------------------------------------------

  defp list_operations do
    IO.puts("\n=== Available Operations ===\n")

    @operations
    |> Enum.group_by(fn {_id, op} -> op.category end)
    |> (fn groups ->
          ~w(structure cleanup workflows docs sync)
          |> Enum.each(fn cat ->
            case Map.get(groups, cat) do
              nil ->
                :ok

              ops ->
                IO.puts("[#{cat}]")

                ops
                |> Enum.sort_by(fn {id, _} -> id end)
                |> Enum.each(fn {id, op} ->
                  IO.puts("  #{id}")
                  IO.puts("    #{op.name}: #{op.description}")
                end)

                IO.puts("")
            end
          end)
        end).()
  end

  # --- Run operation ---------------------------------------------------

  defp run_operation(operation, dry_run?) do
    unless Map.has_key?(@operations, operation) do
      IO.puts("Unknown operation: #{operation}")
      IO.puts("Available operations:")
      list_operations()
      exit({:shutdown, 1})
    end

    repos = load_selection()

    if repos == [] do
      IO.puts("No repos selected. Run --select first.")
      exit({:shutdown, 0})
    end

    op = @operations[operation]
    sep = String.duplicate("=", 60)
    IO.puts(sep)
    IO.puts("Operation: #{op.name}")
    IO.puts("Description: #{op.description}")
    IO.puts("Repos: #{length(repos)}")
    IO.puts("Mode: #{if dry_run?, do: "DRY RUN", else: "LIVE"}")
    IO.puts(sep)

    Enum.each(repos, fn repo ->
      repo_path = Path.join(repos_dir(), repo)

      if File.dir?(repo_path) do
        IO.puts("\n>>> Processing: #{repo}")

        if dry_run? do
          IO.puts("  Would execute: #{operation}")
        else
          result =
            try do
              execute_operation(operation, repo_path)
              "success"
            rescue
              e -> "failed: #{Exception.message(e)}"
            end

          log_operation(repo, operation, result)
        end
      else
        IO.puts("SKIP: #{repo} (not found)")
      end
    end)
  end

  defp execute_operation("structure-init", repo_path) do
    for dir <- [
          [".machine_readable", "6scm"],
          [".machine_readable", "anchoring"],
          [".machine_readable", "reproducibility"],
          [".machine_readable", "config"],
          ["tasks"],
          ["licenses"],
          ["docs", "machines"]
        ] do
      File.mkdir_p!(Path.join([repo_path | dir]))
    end

    IO.puts("  Created RSR directory structure")
  end

  defp execute_operation("move-scm", repo_path) do
    dest = Path.join([repo_path, ".machine_readable", "6scm"])
    File.mkdir_p!(dest)

    repo_path
    |> File.ls!()
    |> Enum.filter(&String.ends_with?(&1, ".scm"))
    |> Enum.each(fn scm ->
      src = Path.join(repo_path, scm)
      dst = Path.join(dest, scm)

      if File.regular?(src) do
        File.rename!(src, dst)
        IO.puts("  Moved #{scm} to .machine_readable/6scm/")
      end
    end)
  end

  defp execute_operation("move-build", repo_path) do
    tasks_dir = Path.join(repo_path, "tasks")
    File.mkdir_p!(tasks_dir)

    for build_file <- ~w(Justfile Mustfile justfile mustfile) do
      src = Path.join(repo_path, build_file)

      if File.regular?(src) do
        dst = Path.join(tasks_dir, String.capitalize(build_file))
        File.rename!(src, dst)
        IO.puts("  Moved #{build_file} to tasks/")
      end
    end
  end

  defp execute_operation("remove-jekyll", repo_path) do
    for f <- ~w(_config.yml Gemfile Gemfile.lock .ruby-version) do
      path = Path.join(repo_path, f)

      if File.regular?(path) do
        File.rm!(path)
        IO.puts("  Removed #{f}")
      end
    end

    for wf <- ~w(jekyll.yml jekyll-gh-pages.yml) do
      path = Path.join([repo_path, ".github", "workflows", wf])

      if File.regular?(path) do
        File.rm!(path)
        IO.puts("  Removed .github/workflows/#{wf}")
      end
    end

    for d <- ~w(_site _layouts _includes _posts _data) do
      path = Path.join(repo_path, d)

      if File.dir?(path) do
        File.rm_rf!(path)
        IO.puts("  Removed #{d}/")
      end
    end
  end

  defp execute_operation("remove-makefile", repo_path) do
    for mf <- ~w(Makefile makefile GNUmakefile) do
      path = Path.join(repo_path, mf)

      if File.regular?(path) do
        File.rm!(path)
        IO.puts("  Removed #{mf}")
      end
    end

    repo_path
    |> File.ls!()
    |> Enum.filter(&String.ends_with?(&1, ".mk"))
    |> Enum.each(fn f ->
      File.rm!(Path.join(repo_path, f))
      IO.puts("  Removed #{f}")
    end)
  end

  defp execute_operation("push-all", repo_path) do
    case System.cmd("git", ["-C", repo_path, "push", "origin"], stderr_to_stdout: true) do
      {_, 0} -> IO.puts("  Pushed to origin")
      {err, _} -> raise "push failed: #{err}"
    end
  end

  defp execute_operation(op, _repo_path) do
    IO.puts("  Operation not yet implemented: #{op}")
  end

  defp log_operation(repo, operation, result) do
    path = Path.expand(@operations_log)
    File.mkdir_p!(Path.dirname(path))

    now = DateTime.utc_now() |> DateTime.to_string()
    line = "#{now}\t#{repo}\t#{operation}\t#{result}\n"
    File.write!(path, line, [:append])
  end

  defp repos_dir do
    Path.expand(System.get_env(@repos_dir_env) || @default_repos_dir)
  end
end

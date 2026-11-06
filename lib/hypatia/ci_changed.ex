# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.CiChanged do
  @moduledoc """
  Parser-based local CI selection and observed execution receipts. Source/input
  changes conservatively select every workflow. Skipped jobs, missing receipts,
  changed checkout configuration and write-capable jobs cannot certify execution.
  """
  alias Hypatia.Kyaml

  @doc "Select changed job semantics and reject dangling or cyclic needs."
  def changed_jobs(before, after_text) do
    old = workflow!(before)
    new = workflow!(after_text)
    validate_graph!(Map.fetch!(new, "jobs"))
    globals_changed = Map.delete(old, "jobs") !== Map.delete(new, "jobs")
    for {id, job} <- new["jobs"], globals_changed or get_in(old, ["jobs", id]) !== job, do: id
  end

  defp workflow!(nil), do: %{"jobs" => %{}}

  defp workflow!(text) do
    case Kyaml.parse(text) do
      {:ok, [doc]} ->
        unless Kyaml.within_budget?(doc), do: raise(ArgumentError, "workflow exceeds node budget")

        case value(doc) do
          %{"jobs" => jobs} = workflow when is_map(jobs) -> workflow
          _ -> raise ArgumentError, "workflow must contain a jobs mapping"
        end

      {:error, error} ->
        raise ArgumentError, Exception.message(error)

      _ ->
        raise ArgumentError, "workflow must contain exactly one document"
    end
  end

  defp value(node) do
    case Kyaml.deref(node) do
      %{kind: :map, entries: entries} ->
        Map.new(entries, fn %{key: key, value: val} -> {value(key), value(val)} end)

      %{kind: :seq, items: items} ->
        Enum.map(items, &value/1)

      %{value: scalar} ->
        scalar
    end
  end

  defp validate_graph!(jobs) do
    graph =
      Map.new(jobs, fn {id, job} ->
        unless is_map(job), do: raise(ArgumentError, "job #{id} must be a mapping")
        needs = List.wrap(Map.get(job, "needs", []))

        unless Enum.all?(needs, &(is_binary(&1) and Map.has_key?(jobs, &1))),
          do: raise(ArgumentError, "job #{id} needs a missing or invalid job")

        {id, MapSet.new(needs)}
      end)

    acyclic!(graph)
  end

  defp acyclic!(graph) when map_size(graph) == 0, do: :ok

  defp acyclic!(graph) do
    ready = for {id, deps} <- graph, MapSet.size(deps) == 0, do: id
    if ready == [], do: raise(ArgumentError, "workflow contains cyclic needs")

    graph
    |> Map.drop(ready)
    |> Map.new(fn {id, deps} -> {id, MapSet.difference(deps, MapSet.new(ready))} end)
    |> acyclic!()
  end

  @doc "Include committed, working and untracked inputs, with a supported event per workflow."
  def plan(repo, base, requested_event \\ nil) do
    git!(repo, ["rev-parse", "--verify", "#{base}^{commit}"])
    changed = git!(repo, ["diff", "--name-only", "-z", base, "--"])
    untracked = git!(repo, ["ls-files", "--others", "--exclude-standard", "-z"])
    files = String.split(changed <> untracked, <<0>>, trim: true) |> Enum.uniq() |> Enum.sort()
    inputs_changed = Enum.any?(files, &(not workflow_file?(&1)))

    workflows =
      if inputs_changed,
        do:
          Path.wildcard(Path.join(repo, ".github/workflows/*"))
          |> Enum.map(&Path.relative_to(&1, repo)),
        else: files

    jobs =
      for file <- workflows,
          workflow_file?(file),
          File.regular?(Path.join(repo, file)),
          current = File.read!(Path.join(repo, file)),
          old <- [previous(repo, base, file)],
          selected = changed_jobs(old, current),
          new = workflow!(current),
          id <- if(inputs_changed, do: Map.keys(new["jobs"]), else: selected),
          do: job_plan(file, id, workflow!(old), new, requested_event)

    %{base: base, files: files, jobs: Enum.sort_by(jobs, &{&1.workflow, &1.job})}
  end

  defp workflow_file?(file),
    do: Path.dirname(file) == ".github/workflows" and Path.extname(file) in [".yml", ".yaml"]

  defp event!(workflow, requested) do
    events =
      case Map.get(workflow, "on") do
        text when is_binary(text) -> [text]
        events when is_list(events) -> events
        events when is_map(events) -> Map.keys(events)
        _ -> []
      end

    if requested && requested not in events,
      do: raise(ArgumentError, "requested event is not supported by workflow")

    event =
      requested ||
        Enum.find(
          ["pull_request", "push", "workflow_dispatch", "schedule", "workflow_call"],
          &(&1 in events)
        ) || List.first(Enum.sort(events))

    event || raise ArgumentError, "workflow has no supported event"
  end

  defp checkout_steps(job),
    do:
      Enum.filter(Map.get(job || %{}, "steps", []), fn step ->
        is_map(step) and is_binary(step["uses"]) and
          String.starts_with?(String.downcase(step["uses"]), "actions/checkout@")
      end)

  defp job_plan(file, id, old, new, event) do
    blocker =
      [id | prerequisites(id, new["jobs"])]
      |> Enum.find_value(fn candidate ->
        case direct_blocker(candidate, old, new) do
          nil -> nil
          reason when candidate == id -> reason
          reason -> "prerequisite #{candidate}: #{reason}"
        end
      end)

    %{workflow: file, job: id, event: event!(new, event), local_blocker: blocker}
  end

  defp prerequisites(id, jobs),
    do: walk_needs(List.wrap(Map.get(jobs[id], "needs", [])), jobs, MapSet.new())

  defp walk_needs([], _jobs, seen), do: seen |> MapSet.to_list() |> Enum.sort()

  defp walk_needs([id | rest], jobs, seen) do
    if MapSet.member?(seen, id),
      do: walk_needs(rest, jobs, seen),
      else:
        walk_needs(
          List.wrap(Map.get(jobs[id], "needs", [])) ++ rest,
          jobs,
          MapSet.put(seen, id)
        )
  end

  defp direct_blocker(id, old, new) do
    job = new["jobs"][id]
    old_job = get_in(old, ["jobs", id])

    checkout_context_changed =
      Map.delete(old, "jobs") !== Map.delete(new, "jobs") or
        Map.drop(old_job || %{}, ["steps"]) !== Map.drop(job, ["steps"])

    checkout_changed =
      checkout_steps(job) != [] and
        (checkout_context_changed or checkout_steps(old_job) !== checkout_steps(job))

    permissions = Map.get(job, "permissions", Map.get(new, "permissions", %{}))

    writes =
      permissions == "write-all" or (is_map(permissions) and "write" in Map.values(permissions))

    cond do
      job["uses"] -> "reusable workflow needs a hosted receipt"
      writes -> "write-capable job needs a hosted receipt"
      checkout_changed -> "changed checkout inputs need a hosted receipt; act skips checkout"
      true -> nil
    end
  end

  defp previous(repo, base, path) do
    case System.cmd("git", ["show", "#{base}:#{path}"], cd: repo, stderr_to_stdout: true) do
      {text, 0} -> text
      _ -> nil
    end
  end

  defp git!(repo, argv) do
    case System.cmd("git", argv, cd: repo, stderr_to_stdout: true) do
      {out, 0} -> out
      {out, status} -> raise ArgumentError, "git failed (#{status}): #{out}"
    end
  end

  @doc "Offline rules on changed files, including ancestor-directory findings."
  def scan(repo, files) do
    changed = Enum.map(files, &Path.expand(&1, repo))

    Hypatia.CLI.collect_findings(repo, [
      :code_safety,
      :workflow_audit,
      :workflow_hardening,
      :research_extensions
    ])
    |> Enum.map(fn finding ->
      file = Map.get(finding, :file, ".")

      Map.put(finding, :file, file |> Path.expand(repo) |> Path.relative_to(repo))
    end)
    |> Enum.filter(fn finding ->
      path = Path.expand(finding.file, repo)
      Enum.any?(changed, &(&1 == path or String.starts_with?(&1, path <> "/")))
    end)
  end

  @doc "Apply the canonical governance helper before grading, preserving its schema and expiry checks."
  def apply_baseline(repo, findings, helper \\ nil) do
    baseline = Path.join(repo, ".hypatia-baseline.json")

    if File.regular?(baseline) do
      helper =
        helper || System.get_env("HYPATIA_BASELINE_HELPER") ||
          Path.join(repo, ".standards/scripts/apply-baseline.sh")

      unless File.regular?(helper),
        do:
          raise(
            ArgumentError,
            "set HYPATIA_BASELINE_HELPER to the governance-pinned standards/scripts/apply-baseline.sh"
          )

      directory =
        Path.join(
          System.tmp_dir!(),
          "hypatia-local-baseline-#{System.pid()}-#{System.os_time(:nanosecond)}"
        )

      File.mkdir_p!(directory)

      try do
        input = Path.join(directory, "findings.json")
        File.write!(input, Jason.encode!(findings))

        case System.cmd("bash", [Path.expand(helper), input, baseline, "advisory"],
               env: [{"GITHUB_STEP_SUMMARY", ""}]
             ) do
          {out, 0} ->
            decoded =
              case Jason.decode!(out) do
                %{"findings_kept" => findings} when is_list(findings) -> findings
                _ -> raise ArgumentError, "baseline helper did not return findings"
              end

            names =
              Map.new(
                [:severity, :advisory, :rule_module, :type, :file, :line, :reason, :action],
                &{Atom.to_string(&1), &1}
              )

            Enum.map(decoded, fn finding ->
              Map.new(finding, fn {key, val} -> {Map.get(names, key, key), val} end)
            end)

          {_out, status} ->
            raise ArgumentError, "canonical baseline helper failed (#{status})"
        end
      after
        File.rm_rf!(directory)
      end
    else
      findings
    end
  end

  @doc "Run eligible jobs with JSON receipts; skipped, dry-run and absent results are not completion."
  def execute_jobs(repo, jobs, runner \\ &System.cmd/3) do
    Enum.map(jobs, fn %{workflow: file, job: id, event: event, local_blocker: blocker} ->
      if blocker do
        %{workflow: file, job: id, exit: nil, outcome: :requires_hosted_receipt, output: blocker}
      else
        {output, status} =
          runner.(
            "act",
            [
              event,
              "-W",
              file,
              "-j",
              id,
              "--json",
              "--verbose",
              "--container-options",
              "--cap-drop=ALL"
            ],
            cd: repo,
            stderr_to_stdout: true
          )

        %{
          workflow: file,
          job: id,
          exit: status,
          outcome: receipt(output, id, status),
          output: output
        }
      end
    end)
  end

  @doc "Read act jobResult fields for the exact job and every observed matrix member."
  def receipt(output, id, exit) do
    results =
      output
      |> String.split("\n", trim: true)
      |> Enum.flat_map(fn line ->
        case Jason.decode(line) do
          {:ok, %{"jobID" => ^id, "jobResult" => result, "dryrun" => false} = row}
          when result in ["success", "failure", "skipped"] ->
            [{Map.get(row, "matrix", %{}), result}]

          _ ->
            []
        end
      end)

    cond do
      exit != 0 -> :failed
      results == [] -> :unobserved
      Enum.any?(results, &(elem(&1, 1) == "failure")) -> :failed
      Enum.any?(results, &(elem(&1, 1) == "skipped")) -> :skipped
      true -> :completed
    end
  end
end

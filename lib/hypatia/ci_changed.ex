# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.CiChanged do
  @moduledoc """
  Local execution of new/changed workflow jobs, using the canonical KYAML
  reader. Global workflow changes conservatively select every remaining job.
  No regex or line-based reconstruction of workflow structure is used.
  """
  alias Hypatia.Kyaml

  def changed_jobs(before, after_text) do
    old = workflow!(before)
    new = workflow!(after_text)
    old_jobs = Map.get(old, "jobs", %{})
    new_jobs = Map.get(new, "jobs", %{})
    global_change = Map.delete(old, "jobs") !== Map.delete(new, "jobs")

    for {id, job} <- new_jobs,
        global_change or Map.get(old_jobs, id) !== job,
        do: id
  end

  defp workflow!(nil), do: %{}

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

  def plan(repo, base) do
    git!(repo, ["rev-parse", "--verify", "#{base}^{commit}"])
    changed = git!(repo, ["diff", "--name-only", "-z", base, "--"])
    untracked = git!(repo, ["ls-files", "--others", "--exclude-standard", "-z"])
    files = String.split(changed <> untracked, <<0>>, trim: true) |> Enum.uniq() |> Enum.sort()

    jobs =
      for file <- files,
          Path.dirname(file) == ".github/workflows",
          Path.extname(file) in [".yml", ".yaml"],
          File.regular?(Path.join(repo, file)),
          id <- changed_jobs(previous(repo, base, file), File.read!(Path.join(repo, file))),
          do: %{workflow: file, job: id}

    %{base: base, files: files, jobs: Enum.sort_by(jobs, &{&1.workflow, &1.job})}
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

  @doc "Offline rules, scoped to the changed files, including existing suppressions."
  def scan(repo, files) do
    changed = MapSet.new(files, &Path.expand(&1, repo))

    Hypatia.CLI.collect_findings(repo, [
      :code_safety,
      :workflow_audit,
      :workflow_hardening,
      :research_extensions
    ])
    |> Enum.filter(fn finding ->
      file = Map.get(finding, :file, ".")

      MapSet.member?(changed, Path.expand(file, repo)) or
        MapSet.member?(changed, Path.expand(Path.join(".github/workflows", file), repo))
    end)
  end

  def execute_jobs(repo, jobs, runner \\ &System.cmd/3) do
    Enum.map(jobs, fn %{workflow: file, job: id} ->
      # An argv list, never shell interpolation. act runs the actual workflow,
      # including setup actions and `needs`, rather than copied run commands.
      {output, status} =
        runner.(
          "act",
          [
            "pull_request",
            "-W",
            file,
            "-j",
            id,
            "--bind",
            "--container-options",
            "--cap-drop=ALL"
          ],
          cd: repo,
          stderr_to_stdout: true
        )

      %{workflow: file, job: id, exit: status, output: output}
    end)
  end
end

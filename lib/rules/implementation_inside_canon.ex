# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ImplementationInsideCanon do
  @moduledoc """
  Executable evaluator for the standards-authored HYP-S009 rule.

  The rule identity, severity, routing, scanner paths, and manifest lists remain
  in `hypatia-rules/implementation-inside-canon.a2ml` in the scanned standards
  checkout. This module is deliberately a narrow evaluator for that known rule;
  it does not interpret arbitrary `@logic` text.
  """

  alias Hypatia.Rules.RuleLoader
  alias Hypatia.Rules.RuleLoader.RuleDef

  @rule_path "hypatia-rules/implementation-inside-canon.a2ml"
  @rule_id "HYP-S009"

  @doc "Run HYP-S009 when its canonical rule definition exists in `repo_path`."
  @spec scan(String.t()) :: {:ok, %{findings: [map()]}} | {:error, term()}
  def scan(repo_path) do
    rule_path = Path.join(repo_path, @rule_path)

    if File.regular?(rule_path) do
      with {:ok, rule_text} <- File.read(rule_path),
           {:ok, %RuleDef{id: @rule_id} = rule} <- RuleLoader.parse(rule_text),
           {:ok, registry_text} <- File.read(Path.join(repo_path, registry_path(rule))),
           {:ok, tracked_files} <- tracked_files(repo_path) do
        evaluate(rule, registry_text, tracked_files,
          read_file: &File.read(Path.join(repo_path, &1))
        )
      else
        {:ok, %RuleDef{id: other}} -> {:error, {:unexpected_rule_id, other}}
        error -> error
      end
    else
      {:ok, %{findings: []}}
    end
  end

  @doc "Evaluate an already-loaded HYP-S009 definition against tracked paths."
  @spec evaluate(RuleDef.t(), String.t(), [String.t()], keyword()) ::
          {:ok, %{findings: [map()]}} | {:error, term()}
  def evaluate(rule, registry_text, tracked_files, opts \\ [])

  def evaluate(%RuleDef{id: @rule_id} = rule, registry_text, tracked_files, opts) do
    read_file = Keyword.get(opts, :read_file, fn _ -> {:error, :no_reader} end)

    with {:ok, homes} <- local_homes(registry_text),
         {:ok, parameters} <- parameters(rule) do
      findings =
        homes
        |> Enum.flat_map(fn %{id: spec_id, home: home} ->
          tracked_files
          |> Enum.filter(&descendant?(&1, home))
          |> Enum.reject(&exempt?(&1, parameters.path_exemptions))
          |> Enum.filter(&manifest?(&1, parameters, read_file))
          |> Enum.map(&finding(rule, spec_id, home, &1))
        end)
        |> Enum.uniq_by(&{&1.spec_id, &1.file})
        |> Enum.sort_by(&{&1.spec_id, &1.file})

      {:ok, %{findings: findings}}
    end
  end

  def evaluate(%RuleDef{id: other}, _registry, _files, _opts),
    do: {:error, {:unsupported_rule, other}}

  defp tracked_files(repo_path) do
    case System.cmd("git", ["-C", repo_path, "ls-files", "-z"], stderr_to_stdout: true) do
      {output, 0} -> {:ok, String.split(output, <<0>>, trim: true)}
      {output, status} -> {:error, {:git_ls_files, status, String.trim(output)}}
    end
  end

  defp registry_path(%RuleDef{parameters_raw: raw}) do
    scalar(raw, "registry") || ".machine_readable/REGISTRY.a2ml"
  end

  defp parameters(%RuleDef{parameters_raw: raw}) when is_binary(raw) do
    basenames = list(raw, "implementation_basenames")
    conditional = list(raw, "conditional_basenames")
    exemptions = list(raw, "path_exemptions")

    if basenames == [] do
      {:error, :missing_implementation_basenames}
    else
      {:ok,
       %{
         implementation_basenames: MapSet.new(basenames),
         conditional_basenames: MapSet.new(conditional),
         path_exemptions: exemptions
       }}
    end
  end

  defp parameters(_), do: {:error, :missing_parameters}

  defp local_homes(text) do
    specs =
      text
      |> String.split(~r/^\s*\[\[spec\]\]\s*$/m)
      |> Enum.drop(1)
      |> Enum.map(fn block ->
        %{id: scalar(block, "id"), home: scalar(block, "home"), kind: scalar(block, "kind")}
      end)
      |> Enum.reject(&(&1.kind == "external"))

    case Enum.find(specs, &(not valid_home?(&1.home))) do
      nil -> {:ok, specs}
      bad -> {:error, {:invalid_local_home, bad.id, bad.home}}
    end
  end

  defp valid_home?(home) when is_binary(home) and home != "" do
    Path.type(home) == :relative and
      not Enum.member?(Path.split(home), "..") and
      home != "."
  end

  defp valid_home?(_), do: false

  defp scalar(text, key) when is_binary(text) do
    case Regex.run(~r/^\s*#{Regex.escape(key)}\s*(?:=|:)\s*"?([^"#\n]+)"?\s*$/m, text) do
      [_, value] -> String.trim(value)
      _ -> nil
    end
  end

  defp scalar(_, _), do: nil

  defp list(text, key) do
    case Regex.run(
           ~r/^\s*#{Regex.escape(key)}\s*:\s*\n((?:\s+-\s+[^\n]+\n?)*)/m,
           text
         ) do
      [_, body] ->
        ~r/^\s*-\s+"?([^"\n]+)"?\s*$/m
        |> Regex.scan(body)
        |> Enum.map(fn [_, value] -> String.trim(value) end)

      _ ->
        []
    end
  end

  defp descendant?(path, home) do
    normalized_home = String.trim_trailing(home, "/")
    String.starts_with?(path, normalized_home <> "/")
  end

  defp exempt?(path, exemptions) do
    Enum.any?(exemptions, fn pattern ->
      segment = pattern |> String.trim("*") |> String.trim("/")
      String.contains?("/" <> path <> "/", "/" <> segment <> "/")
    end)
  end

  defp manifest?(path, parameters, read_file) do
    basename = Path.basename(path)

    cond do
      MapSet.member?(parameters.implementation_basenames, basename) ->
        true

      MapSet.member?(parameters.conditional_basenames, basename) ->
        case read_file.(path) do
          {:ok, content} -> non_stub?(content)
          _ -> false
        end

      true ->
        false
    end
  end

  defp non_stub?(content) do
    content
    |> String.split("\n")
    |> Enum.any?(fn line ->
      trimmed = String.trim(line)
      trimmed != "" and not String.starts_with?(trimmed, ";")
    end)
  end

  defp finding(rule, spec_id, home, path) do
    %{
      rule_module: "implementation_inside_canon",
      type: rule.id,
      severity: to_string(rule.severity || :medium),
      file: path,
      reason:
        "Implementation manifest #{path} is inside LOCAL canonical spec home " <>
          "'#{home}' (#{spec_id}).",
      action: to_string(rule.router_default_strategy || :review),
      recipe_id: rule.action_recipe,
      signal: rule.action_signal,
      spec_id: spec_id,
      home: home
    }
  end
end

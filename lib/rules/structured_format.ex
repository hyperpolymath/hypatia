# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Rules.StructuredFormat do
  @moduledoc """
  SF001: advisory detection of an Elixir regex that reconstructs workflow
  mapping structure. Parse Elixir syntax first, so prose and commented-out
  examples cannot trigger it. Canonical YAML lives in Hypatia.Kyaml.
  This heuristic is evidence of parser debt, not proof of a vulnerability.
  """
  def scan_content(content) do
    case Code.string_to_quoted(content) do
      {:ok, ast} ->
        {_ast, lines} =
          Macro.prewalk(ast, [], fn
            {:sigil_r, meta, [{:<<>>, _, [source]}, _]} = node, lines when is_binary(source) ->
              if Enum.any?(
                   ["jobs:", "steps:", "runs-on:", "permissions:"],
                   &String.contains?(source, &1)
                 ),
                 do: {node, [Keyword.get(meta, :line, 1) | lines]},
                 else: {node, lines}

            node, lines ->
              {node, lines}
          end)

        lines
        |> Enum.uniq()
        |> Enum.sort()
        |> Enum.map(fn line ->
          %{
            rule_module: "structured_format",
            type: "SF001",
            severity: "medium",
            advisory: true,
            line: line,
            action: "review",
            reason:
              "Regex reads workflow mapping structure; use Hypatia.Kyaml positioned nodes (971 / 1009)"
          }
        end)

      _ ->
        []
    end
  end
end

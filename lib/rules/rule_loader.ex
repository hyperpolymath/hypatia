# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.RuleLoader do
  @moduledoc """
  Loads standards-authored rule definitions (`.a2ml` files) into structured
  `RuleDef`s so the rule *catalogue and routing* are sourced from
  `hyperpolymath/standards` rather than hand-ported into Elixir.

  This is HYP-S increment 1: the "single source of truth" wiring the audit asked
  for. The seven `standards/hypatia-rules/*.a2ml` files (HYP-S001..S007) declare
  each rule's identity, severity, scanner globs, router strategy + strategy caps,
  emitted signal, and recipe. Historically those were specified in standards but
  *ghost* in hypatia (no loader existed). This module parses them.

  ## Scope of this increment

  This loader extracts the **declarative** parts of a rule definition — the parts
  that are safe to source from files and route on:

    * identity/metadata (`id`, `name`, `description`, `severity`, `category`,
      `auto_fixable`, `source`)
    * scanner globs (`@scanner` → `find:` / `glob:`)
    * routing (`@router` → `default_strategy`, `recipe`, and the
      **`strategy_caps`** — the safety-critical part, e.g. the Manual-Only
      licence cap that demotes any licence/SPDX-overlapping finding to `:review`)
    * action (`@action` → `emit_signal`, `recipe`)
    * the raw `@logic` block text, preserved verbatim for a later increment that
      executes it (this loader does NOT interpret `@logic` — it does not invent
      detection behaviour it cannot yet verify).

  The rule files use the A2ML *markup* block dialect (`@block(attrs): … @end` with
  YAML-flavoured `key: value` and `- ` list items), distinct from the record
  dialect used by descriptile/criteria files. This parser is deliberately narrow
  to that block shape rather than a general YAML engine.
  """

  defmodule RuleDef do
    @moduledoc "One parsed standards rule definition."
    @enforce_keys [:id]
    defstruct id: nil,
              name: nil,
              description: nil,
              severity: nil,
              category: nil,
              auto_fixable: nil,
              source: nil,
              scanner_globs: [],
              router_default_strategy: nil,
              router_recipe: nil,
              # Each cap: %{when: pattern, cap: strategy_atom, reason: text}
              strategy_caps: [],
              action_signal: nil,
              action_recipe: nil,
              # Verbatim @logic block body — NOT interpreted in this increment.
              logic_raw: nil

    @type t :: %__MODULE__{}
  end

  @strategies ~w(auto_execute review report_only)a

  @doc """
  Load and parse every `*.a2ml` rule definition in `dir`.

  Returns `{:ok, [RuleDef.t()]}` sorted by `id`, or `{:error, reason}` if the
  directory is unreadable. Individual files that fail to parse are returned in
  the error list rather than silently dropped (fail loudly).
  """
  @spec load_dir(String.t()) :: {:ok, [RuleDef.t()]} | {:error, term()}
  def load_dir(dir) do
    case File.ls(dir) do
      {:ok, entries} ->
        {rules, errors} =
          entries
          |> Enum.filter(&String.ends_with?(&1, ".a2ml"))
          |> Enum.sort()
          |> Enum.reduce({[], []}, fn file, {ok, bad} ->
            path = Path.join(dir, file)

            case File.read(path) do
              {:ok, text} ->
                case parse(text) do
                  {:ok, rule} -> {[rule | ok], bad}
                  {:error, why} -> {ok, [{file, why} | bad]}
                end

              {:error, why} ->
                {ok, [{file, why} | bad]}
            end
          end)

        if errors == [] do
          {:ok, Enum.sort_by(rules, & &1.id)}
        else
          {:error, {:parse_failures, Enum.reverse(errors)}}
        end

      {:error, reason} ->
        {:error, {:cannot_read_dir, dir, reason}}
    end
  end

  @doc """
  Parse one rule-definition file's text into a `RuleDef`.

  Returns `{:error, :no_rule_block}` when the text carries no `@rule` block or
  `{:error, {:missing_id, ...}}` when the mandatory `id` is absent — the loader
  refuses to fabricate an identity.
  """
  @spec parse(String.t()) :: {:ok, RuleDef.t()} | {:error, term()}
  def parse(text) when is_binary(text) do
    blocks = tokenize_blocks(text)

    with %{body: rule_body} <- find_block(blocks, "rule"),
         id when is_binary(id) <- scalar(rule_body, "id") do
      router = find_block(blocks, "router")
      action = find_block(blocks, "action")
      scanner = find_block(blocks, "scanner")
      logic = find_block(blocks, "logic")

      {:ok,
       %RuleDef{
         id: id,
         name: scalar(rule_body, "name"),
         description: scalar(rule_body, "description"),
         severity: scalar(rule_body, "severity") |> to_atom_or_nil(),
         category: scalar(rule_body, "category"),
         auto_fixable: scalar(rule_body, "auto_fixable") |> to_bool_or_nil(),
         source: scalar(rule_body, "source"),
         scanner_globs: scanner |> body_of() |> extract_globs(),
         router_default_strategy:
           router |> body_of() |> scalar("default_strategy") |> to_strategy_or_nil(),
         router_recipe: router |> body_of() |> scalar("recipe"),
         strategy_caps: router |> body_of() |> extract_strategy_caps(),
         action_signal: action |> body_of() |> scalar("emit_signal"),
         action_recipe: action |> body_of() |> scalar("recipe"),
         logic_raw: logic |> body_of()
       }}
    else
      # find_block/2 returned nil (no @rule block) or scalar/2 returned nil (no
      # `id`); either way we refuse to fabricate a rule identity.
      _ -> {:error, :no_rule_or_id}
    end
  end

  @doc """
  The subset of `strategy_caps` across all rules that pin a finding to `:review`
  because it overlaps licence/SPDX content. Surfacing this explicitly makes the
  Manual-Only licence guardrail auditable: downstream routing MUST honour these.
  """
  @spec licence_caps([RuleDef.t()]) :: [map()]
  def licence_caps(rules) when is_list(rules) do
    rules
    |> Enum.flat_map(& &1.strategy_caps)
    |> Enum.filter(fn cap ->
      cap[:cap] == :review and licence_related?(cap[:when] || "")
    end)
  end

  # --- internal parsing helpers ---------------------------------------------

  # A block is %{name: "rule", attrs: "version=\"1.0\"", body: "…lines…"}.
  # Blocks open on a line like `@name(attrs):` or `@name:` and close on `@end`.
  defp tokenize_blocks(text) do
    lines = String.split(text, "\n")

    {blocks, _open} =
      Enum.reduce(lines, {[], nil}, fn raw, {acc, open} ->
        line = raw

        cond do
          # close current block
          Regex.match?(~r/^\s*@end\s*$/, line) and open != nil ->
            {[finalize(open) | acc], nil}

          # open a new block: @name(...)?:?  (but not @end)
          match = Regex.run(~r/^\s*@([a-zA-Z][\w-]*)\s*(?:\((.*)\))?\s*:?\s*$/, line) ->
            [_, name, attrs] = normalize_match(match)

            if name == "end" do
              {acc, open}
            else
              # starting a new block implicitly closes a dangling one
              acc = if open, do: [finalize(open) | acc], else: acc
              {acc, %{name: name, attrs: attrs, lines: []}}
            end

          # a body line inside an open block
          open != nil ->
            {acc, %{open | lines: [line | open.lines]}}

          # preamble / stray line outside any block — ignore
          true ->
            {acc, open}
        end
      end)

    Enum.reverse(blocks)
  end

  defp normalize_match([_, name]), do: [nil, name, ""]
  defp normalize_match([_, name, attrs]), do: [nil, name, attrs || ""]

  defp finalize(%{name: name, attrs: attrs, lines: lines}) do
    %{name: name, attrs: attrs, body: lines |> Enum.reverse() |> Enum.join("\n")}
  end

  defp find_block(blocks, name), do: Enum.find(blocks, &(&1.name == name))

  defp body_of(nil), do: ""
  defp body_of(%{body: body}), do: body

  # Extract a flat scalar `key: value` from a block body. Strips a trailing
  # inline comment, surrounding quotes, and whitespace. Ignores list items.
  defp scalar(body, key) when is_binary(body) do
    regex = ~r/^\s*#{Regex.escape(key)}\s*:\s*(.+?)\s*$/m

    case Regex.run(regex, body) do
      [_, val] -> val |> strip_inline_comment() |> unquote_val()
      _ -> nil
    end
  end

  defp scalar(_, _), do: nil

  defp extract_globs(body) when is_binary(body) do
    ~r/glob\s*:\s*"?([^"\n]+?)"?\s*$/m
    |> Regex.scan(body)
    |> Enum.map(fn [_, g] -> String.trim(g) end)
  end

  defp extract_globs(_), do: []

  # Parse the `strategy_caps:` list of `- when:/cap:/reason:` entries in a
  # @router body. This is the safety-critical part: the licence cap lives here.
  defp extract_strategy_caps(body) when is_binary(body) do
    lines = String.split(body, "\n")

    {caps, cur} =
      Enum.reduce(lines, {[], nil}, fn line, {acc, cur} ->
        # Compute matches up front so the bindings are always in scope in the
        # branch bodies (assigning inside a `cond` condition does not reliably
        # bind in the body).
        when_m = Regex.run(~r/^\s*-\s*when\s*:\s*(.+?)\s*$/, line)
        cap_m = Regex.run(~r/^\s*cap\s*:\s*(.+?)\s*$/, line)
        reason_m = Regex.run(~r/^\s*reason\s*:\s*(.+?)\s*$/, line)

        cond do
          # new cap entry begins with `- when:`
          when_m ->
            acc = if cur, do: [cur | acc], else: acc
            {acc, %{when: unquote_val(strip_inline_comment(Enum.at(when_m, 1)))}}

          cur && cap_m ->
            {acc, Map.put(cur, :cap, to_strategy_or_nil(unquote_val(Enum.at(cap_m, 1))))}

          cur && reason_m ->
            {acc, Map.put(cur, :reason, unquote_val(strip_inline_comment(Enum.at(reason_m, 1))))}

          true ->
            {acc, cur}
        end
      end)

    if(cur, do: [cur | caps], else: caps) |> Enum.reverse()
  end

  defp extract_strategy_caps(_), do: []

  defp licence_related?(text) do
    String.match?(text, ~r/SPDX|licen[cs]e|PMPL|MPL-2|AGPL|Palimpsest/i)
  end

  defp strip_inline_comment(val) do
    # Drop a trailing ` # comment`, but never split inside a quoted string.
    if String.contains?(val, "\"") do
      val
    else
      val |> String.split(~r/\s+#/, parts: 2) |> hd() |> String.trim()
    end
  end

  defp unquote_val(nil), do: nil

  defp unquote_val(val) do
    val
    |> String.trim()
    |> String.replace(~r/^["']|["']$/, "")
    |> String.trim()
  end

  defp to_atom_or_nil(nil), do: nil
  defp to_atom_or_nil(s), do: String.to_atom(s)

  defp to_bool_or_nil("true"), do: true
  defp to_bool_or_nil("false"), do: false
  defp to_bool_or_nil(_), do: nil

  defp to_strategy_or_nil(nil), do: nil

  defp to_strategy_or_nil(s) do
    a = String.to_atom(s)
    if a in @strategies, do: a, else: nil
  end
end

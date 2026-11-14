# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.Workflow do
  @moduledoc """
  One parsed model of a GitHub Actions workflow, for every rule that looks
  inside workflows (#971).

  A workflow is parsed once by `Hypatia.Kyaml`, the port of kyq's KYAML reader
  (YAML 1.2 Core Schema, block and flow), and read into three records:

      %Hypatia.Workflow{path, doc, triggers, env, permissions, jobs}
      %Hypatia.Workflow.Job{id, line, node, runs_on, uses, if, env, permissions, steps}
      %Hypatia.Workflow.Step{line, node, uses, run, run_style, with, env, if}

  Scalars a rule compares (`uses`, `run`, `if`) are their YAML *values*: a
  folded or literal block scalar is folded and chomped, a quoted one
  unquoted, an alias read through. Mappings a rule walks (`env`, `with`,
  `permissions`) stay `Hypatia.Kyaml` nodes, so every value keeps its own
  source line, and `strings/1` maps each byte of a value back to the line it
  came from. A rule reports at a source line, never at a position in a
  rewritten text.

  `run_style` is the scalar's style as the reader records it: `:plain`,
  `:quoted` or `:block`. (The reader does not distinguish `|` from `>`; the
  value already reflects the difference.)

  What the reader refuses is not guessed at. A `<<` merge key (which GitHub
  Actions refuses too), a document past kyq's node budget, or any other KYAML
  error is `{:error, %Hypatia.Kyaml.Error{}}`, at the error's location, and a
  rule decides how to report a workflow it cannot read.
  """

  alias Hypatia.Kyaml

  defstruct [:path, :doc, triggers: [], env: nil, permissions: nil, jobs: []]

  defmodule Job do
    @moduledoc "A job of a workflow: its own fields, with its steps in order."
    defstruct [
      :id,
      :line,
      :node,
      :runs_on,
      :uses,
      :if,
      :env,
      :permissions,
      steps: []
    ]
  end

  defmodule Step do
    @moduledoc "A step of a job: its own fields, values read as YAML gives them."
    defstruct [:line, :node, :uses, :run, :run_style, :with, :env, :if]
  end

  @type t :: %__MODULE__{}

  @doc """
  Reads a workflow file and returns the result of `parse/2`, with its path
  stored in the model. Returns `{:ok, nil}` for an empty stream.

  Raises `File.Error` if the file cannot be read.
  """
  def read(path), do: path |> File.read!() |> parse(path)

  @doc """
  Parses a workflow's bytes into the model. The first document is the
  workflow; `{:ok, nil}` for an empty stream. Returns `{:ok, workflow}` on
  success, storing `path` as supplied without reading it.

  Returns `{:error, %Hypatia.Kyaml.Error{}}` for a KYAML parse error
  anywhere in the stream or when the first document exceeds the expanded
  node budget.
  """
  def parse(bytes, path \\ nil) when is_binary(bytes) do
    case Kyaml.parse(bytes) do
      {:ok, []} -> {:ok, nil}
      {:ok, [doc | _]} -> from_doc(doc, path)
      {:error, error} -> {:error, error}
    end
  end

  # Past kyq's node budget the workflow is refused, as kyq's bridge refuses
  # it, rather than read in part.
  defp from_doc(doc, path) do
    if Kyaml.within_budget?(doc) do
      {:ok,
       %__MODULE__{
         path: path,
         doc: doc,
         triggers: events(doc),
         env: Kyaml.get(doc, "env"),
         permissions: Kyaml.get(doc, "permissions"),
         jobs: for(entry <- entries(Kyaml.get(doc, "jobs")), do: job(entry))
       }}
    else
      {line, column} = doc.loc

      {:error,
       %Kyaml.Error{
         code: "NODE_BUDGET_EXCEEDED",
         message:
           "the document expands to more than #{Kyaml.node_budget()} nodes through its aliases",
         line: line,
         column: column
       }}
    end
  end

  # A job or step read through an alias (`b: *a`, `- *step`) keeps the alias
  # around everything read from it (`child/2`, `entries/1`, `items/1`), so a
  # rule reports what it finds there at the alias, not at the anchor.
  defp job(%{key: key, value: value}) do
    %Job{
      id: key.value,
      line: elem(key.loc, 0),
      node: value,
      runs_on: child(value, "runs-on"),
      uses: text(child(value, "uses")),
      if: text(child(value, "if")),
      env: child(value, "env"),
      permissions: child(value, "permissions"),
      steps: for(item <- items(child(value, "steps")), do: step(item))
    }
  end

  defp step(item) do
    run = child(item, "run")

    %Step{
      line: elem(item.loc, 0),
      node: item,
      uses: text(child(item, "uses")),
      run: text(run),
      run_style: run && Kyaml.deref(run)[:style],
      with: child(item, "with"),
      env: child(item, "env"),
      if: text(child(item, "if"))
    }
  end

  # `Kyaml.get/2`, with the value kept under the alias it was read through.
  defp child(%{kind: :alias} = alias, key), do: under(alias, child(alias.target, key))
  defp child(node, key), do: Kyaml.get(node, key)

  # `node`, read through `alias`: reported at the alias, as `strings/1` reports it.
  defp under(_alias, nil), do: nil
  defp under(alias, node), do: %{alias | target: node}

  @doc """
  The events of the workflow's `on:`: `on: push`, a list, or the keys of a
  mapping (`on: {push: {paths: […]}}` names one event).
  """
  def events(doc) do
    case Kyaml.get(doc, "on") && Kyaml.deref(Kyaml.get(doc, "on")) do
      %{kind: :map, entries: entries} -> Enum.map(entries, &to_string(&1.key.value))
      %{kind: :seq, items: items} -> Enum.flat_map(items, &(&1 |> text() |> List.wrap()))
      %{} = scalar -> ~r/[A-Za-z_]+/ |> Regex.scan(text(scalar) || "") |> List.flatten()
      nil -> []
    end
  end

  @doc """
  A step input's scalar text (`with: {egress-policy: block}`), as returned by
  `text/1`; `nil` if the input is missing or is a collection. The name matches
  the resolved string key exactly.
  """
  def input(%Step{with: with}, name), do: with |> Kyaml.get(name) |> text()

  @doc """
  The entries of a mapping node; [] for anything else. Through an alias,
  each value stays under the alias, so it is reported at the alias.
  """
  def entries(%{kind: :alias} = alias),
    do: for(entry <- entries(alias.target), do: %{entry | value: under(alias, entry.value)})

  def entries(%{kind: :map, entries: entries}), do: entries
  def entries(_), do: []

  @doc """
  The items of a sequence node; [] for anything else. Through an alias,
  each item stays under the alias, so it is reported at the alias.
  """
  def items(%{kind: :alias} = alias), do: for(item <- items(alias.target), do: under(alias, item))
  def items(%{kind: :seq, items: items}), do: items
  def items(_), do: []

  @doc """
  A scalar's text, whatever it resolved to (`egress-policy: block`, `ref: 1`):
  the value of a string, the source text of any other scalar; nil for a
  collection or a missing node.
  """
  def text(node) do
    case node && Kyaml.deref(node) do
      %{kind: :string, value: value} -> value
      %{kind: kind, raw: raw} when kind not in [:map, :seq] -> raw
      _ -> nil
    end
  end

  @doc """
  The string values under a node in document order, each with a function
  from a byte offset in the value to its source line. A block scalar maps
  each of its lines; through an alias, every offset maps to the alias's line.
  Returns `{value, line_of}` pairs. Offsets are zero-based bytes and source
  lines are one-based. Mapping keys and non-string scalars are omitted.
  Returns `[]` for `nil` or a node exceeding kyq's expanded node budget;
  no partial results are returned.
  """
  def strings(node), do: strings(node, nil)

  @doc """
  Like `strings/1`, with an optional source line override. When `alias_line`
  is non-nil, every returned offset mapper reports that line, including
  values reached through nested aliases.
  """
  def strings(nil, _), do: []

  def strings(node, alias_line) do
    if Kyaml.within_budget?(node), do: walk(node, alias_line), else: []
  end

  defp walk(%{kind: :alias, target: target, loc: {line, _}}, alias_line),
    do: walk(target, alias_line || line)

  defp walk(%{kind: :map, entries: entries}, alias_line),
    do: Enum.flat_map(entries, &walk(&1.value, alias_line))

  defp walk(%{kind: :seq, items: items}, alias_line),
    do: Enum.flat_map(items, &walk(&1, alias_line))

  defp walk(%{kind: :string, value: value} = scalar, alias_line) do
    line_of =
      cond do
        alias_line ->
          fn _ -> alias_line end

        scalar.segments == [] ->
          fn _ -> elem(scalar.loc, 0) end

        true ->
          fn at ->
            scalar.segments
            |> Enum.take_while(fn {offset, _} -> offset <= at end)
            |> List.last({0, elem(scalar.loc, 0) + 1})
            |> elem(1)
          end
      end

    [{value, line_of}]
  end

  defp walk(_scalar, _alias_line), do: []

  @doc """
  A `run:` script with each shell comment blanked to spaces, byte for byte,
  so an offset in the result is the same offset in `script` and line breaks
  stay where they were. With `quotes: true` the text inside quotes is blanked
  too; the quote marks stay.

  The shell's rules, not YAML's: a quote opens anywhere outside quotes and
  carries across lines; a `#` opens a comment only where a word starts
  (line start, or after a blank or `; & | ( ) < >`); a backslash escapes the
  next byte outside single quotes.
  """
  def shell_mask(script, opts \\ []) when is_binary(script),
    do: mask(script, nil, true, false, Keyword.get(opts, :quotes, false), [])

  @word_breaks [?\s, ?\t, ?;, ?&, ?|, ?(, ?), ?<, ?>]

  defp mask(<<>>, _q, _ws?, _cmt?, _qs?, acc), do: acc |> Enum.reverse() |> IO.iodata_to_binary()
  defp mask(<<?\n, r::binary>>, q, _, _, qs?, acc), do: mask(r, q, true, false, qs?, [?\n | acc])
  defp mask(<<_, r::binary>>, q, ws?, true, qs?, acc), do: mask(r, q, ws?, true, qs?, [?\s | acc])

  defp mask(<<?#, r::binary>>, nil, true, _, qs?, acc),
    do: mask(r, nil, true, true, qs?, [?\s | acc])

  defp mask(<<c, r::binary>>, nil, _, _, qs?, acc) when c in [?', ?"],
    do: mask(r, c, false, false, qs?, [c | acc])

  defp mask(<<c, r::binary>>, c, _, _, qs?, acc), do: mask(r, nil, false, false, qs?, [c | acc])

  defp mask(<<?\\, c, r::binary>>, q, _, _, qs?, acc) when q != ?' do
    kept = if q != nil and qs?, do: [blank(c), ?\s], else: [c, ?\\]
    mask(r, q, false, false, qs?, kept ++ acc)
  end

  defp mask(<<c, r::binary>>, nil, _, _, qs?, acc),
    do: mask(r, nil, c in @word_breaks, false, qs?, [c | acc])

  defp mask(<<c, r::binary>>, q, _, _, qs?, acc),
    do: mask(r, q, false, false, qs?, [if(qs?, do: blank(c), else: c) | acc])

  defp blank(?\n), do: ?\n
  defp blank(_), do: ?\s
end

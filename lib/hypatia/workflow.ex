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
  Reads a workflow file. `{:ok, nil}` for an empty stream.
  """
  def read(path), do: path |> File.read!() |> parse(path)

  @doc """
  Parses a workflow's bytes into the model. The first document is the
  workflow; `{:ok, nil}` for an empty stream.
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

  defp job(%{key: key, value: value}) do
    node = Kyaml.deref(value)

    %Job{
      id: key.value,
      line: elem(key.loc, 0),
      node: value,
      runs_on: Kyaml.get(node, "runs-on"),
      uses: text(Kyaml.get(node, "uses")),
      if: text(Kyaml.get(node, "if")),
      env: Kyaml.get(node, "env"),
      permissions: Kyaml.get(node, "permissions"),
      steps: for(item <- items(Kyaml.get(node, "steps")), do: step(item))
    }
  end

  defp step(item) do
    node = Kyaml.deref(item)
    run = Kyaml.get(node, "run")

    %Step{
      line: elem(item.loc, 0),
      node: node,
      uses: text(Kyaml.get(node, "uses")),
      run: text(run),
      run_style: run && Kyaml.deref(run)[:style],
      with: Kyaml.get(node, "with"),
      env: Kyaml.get(node, "env"),
      if: text(Kyaml.get(node, "if"))
    }
  end

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

  @doc "A step input's value (`with: {egress-policy: block}`), or nil."
  def input(%Step{with: with}, name), do: with |> Kyaml.get(name) |> text()

  @doc "The entries of a mapping node (through an alias); [] for anything else."
  def entries(node) do
    case node && Kyaml.deref(node) do
      %{kind: :map, entries: entries} -> entries
      _ -> []
    end
  end

  @doc "The items of a sequence node (through an alias); [] for anything else."
  def items(node) do
    case node && Kyaml.deref(node) do
      %{kind: :seq, items: items} -> items
      _ -> []
    end
  end

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
  The walk is bounded like the reader: past kyq's node budget a node yields
  nothing more.
  """
  def strings(node), do: strings(node, nil)

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
end

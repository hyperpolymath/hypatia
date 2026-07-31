# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.RsrCriteria do
  @moduledoc """
  Loads the **RSR v2.0 criteria SSOT** (`rsr-criteria-v2.a2ml` from
  `hyperpolymath/standards`) into structured data hypatia can act on.

  This is the record-dialect sibling of `Hypatia.Rules.RuleLoader` (which reads
  the markup-dialect HYP-S rule files): both realise the audit's "standards is
  the single source of truth; hypatia is compiled from it" contract. RSR v2.0
  ships its 11 weighted categories and 74 criteria as ONE machine-readable file
  in the A2ML record dialect; this module parses it via
  `Hypatia.A2ml.RecordDialect` and exposes the criteria as a queryable set.

  It does NOT itself score repositories — that is a later increment. It provides
  the *catalogue*: every criterion with its id, tier, gating capability, and
  detection rule, plus the tier thresholds. A criterion's `detect` field is the
  hypatia rule id that checks it (or the literal `"manual"`), which is the join
  point to the live scanner.
  """

  alias Hypatia.A2ml.RecordDialect

  defmodule Criterion do
    @moduledoc false
    @enforce_keys [:id]
    defstruct [
      :id,
      :name,
      :desc,
      :tier,
      :gate,
      :detect,
      :template_ref,
      :category_id,
      :category_key
    ]

    @type t :: %__MODULE__{}
  end

  @tiers ~w(bronze silver gold rhodium)

  @doc """
  Load and shape the criteria SSOT at `path`.

  Returns `{:ok, %{version:, status:, tiers:, categories:, criteria:}}` where
  `criteria` is a flat list of `Criterion` structs across all categories.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, term()}
  def load(path) when is_binary(path) do
    with {:ok, text} <- File.read(path),
         {:ok, tree} <- RecordDialect.parse(text) do
      shape(tree)
    end
  end

  @doc "Parse already-read record-dialect `text` into the shaped catalogue."
  @spec load_text(String.t()) :: {:ok, map()} | {:error, term()}
  def load_text(text) when is_binary(text) do
    with {:ok, tree} <- RecordDialect.parse(text), do: shape(tree)
  end

  @doc "Criteria whose gate is `\"universal\"` (apply to every repo)."
  @spec universal(map()) :: [Criterion.t()]
  def universal(%{criteria: cs}), do: Enum.filter(cs, &(&1.gate == "universal"))

  @doc "Criteria required at or below `tier` (bronze ⊆ silver ⊆ gold ⊆ rhodium)."
  @spec required_for_tier(map(), String.t()) :: [Criterion.t()]
  def required_for_tier(%{criteria: cs}, tier) when tier in @tiers do
    rank = tier_rank(tier)
    Enum.filter(cs, &(tier_rank(&1.tier) <= rank))
  end

  @doc "Criteria whose detection is automated (a real rule id, not \"manual\")."
  @spec automatable(map()) :: [Criterion.t()]
  def automatable(%{criteria: cs}), do: Enum.reject(cs, &(&1.detect in [nil, "manual"]))

  # --- shaping --------------------------------------------------------------

  defp shape(tree) do
    meta = Map.get(tree, "meta", %{})
    tiers = Map.get(tree, "tiers", %{})
    categories = Map.get(tree, "category", [])

    cond do
      not is_list(categories) or categories == [] ->
        {:error, :no_categories}

      true ->
        criteria =
          Enum.flat_map(categories, fn cat ->
            cat_id = Map.get(cat, "id")
            cat_key = Map.get(cat, "key")

            cat
            |> Map.get("criteria", [])
            |> Enum.map(fn c ->
              %Criterion{
                id: Map.get(c, "id"),
                name: Map.get(c, "name"),
                desc: Map.get(c, "desc"),
                tier: Map.get(c, "tier"),
                gate: Map.get(c, "gate"),
                detect: Map.get(c, "detect"),
                template_ref: Map.get(c, "template_ref"),
                category_id: cat_id,
                category_key: cat_key
              }
            end)
          end)

        {:ok,
         %{
           version: Map.get(meta, "version"),
           status: Map.get(meta, "status"),
           tiers: tiers,
           categories: categories,
           criteria: criteria
         }}
    end
  end

  defp tier_rank("bronze"), do: 1
  defp tier_rank("silver"), do: 2
  defp tier_rank("gold"), do: 3
  defp tier_rank("rhodium"), do: 4
  defp tier_rank(_), do: 99
end

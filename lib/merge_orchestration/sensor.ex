# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.Sensor do
  @moduledoc """
  The sensory cortex — the *sense* half of sense → deliberate → actuate.

  It turns a raw **PR observation** into the decision `ctx` the `Strategist`
  and `Dispatcher` consume. An observation is the token-free record a scanning
  bot deposits in the shared store (verisim-data) per open PR:

      %{"repo" => "hyperpolymath/panll", "number" => 412,
        "branch" => "dependabot/cargo/serde-1.0.200",
        "title" => "Bump serde from 1.0.199 to 1.0.200",
        "head_sha" => "abc123", "files" => ["Cargo.lock"], "labels" => ["dependencies"]}

  The brain holds no GitHub token: the *fetch* (gh / API) is done by the
  token-bearing scanning bot (or a CI job, or the `.git-private-farm` hand) and
  written to the store as an observation; the brain only ever **reads** it. So
  the three inputs a context needs all come off the shared flat-file store:

    * the **observation** itself (above) — what changed;
    * the repo's **pool policy** (artifact-5 `RepoPoolPolicy`, the per-repo
      `bot_directive` merge label) — how aggressively this repo may be merged;
    * the bots' **signed attestations** (artifact-3) for the PR — the evidence.

  Classification is pure (`classify/1`): a PR's branch/title/files/labels decide
  its `change_class` (route) and `change_level` (object vs the meta reflexivity
  guard) and whether it is a license touch (the owner-only symbolic veto). No
  confidence lives here — confidence is the council's job, downstream.
  """

  alias Hypatia.MergeOrchestration.Dispatcher

  # ── Sense pipeline ──────────────────────────────────────────────────────────

  @doc """
  Sense across many observations into decision contexts.

  `resolve_pool` is `(repo -> pool_policy | nil)`; `resolve_attestations` is
  `(repo, number -> [attestation])`. Both default to store-backed readers via
  `store_resolvers/2`. Returns a list of `ctx` maps ready for `Dispatcher`.
  """
  def sense(observations, resolve_pool, resolve_attestations) when is_list(observations) do
    Enum.map(observations, fn obs ->
      to_context(obs, resolve_pool.(obs["repo"]), resolve_attestations.(obs["repo"], obs["number"]))
    end)
  end

  @doc "Sense → deliberate in one shot: returns the `Dispatcher.decide_all/1` result."
  def sense_and_decide(observations, resolve_pool, resolve_attestations) do
    observations |> sense(resolve_pool, resolve_attestations) |> Dispatcher.decide_all()
  end

  @doc """
  Build a single decision `ctx` from an observation + its pool policy + attestations.
  Pure. `pool_policy` may be `nil` (unlabelled repo ⇒ conservative `:p1` default).
  """
  def to_context(obs, pool_policy, attestations) do
    {class, level, license_touch} = classify(obs)
    atts = Enum.map(attestations, &normalize_attestation/1)

    %{
      pr: %{repo: obs["repo"], number: obs["number"]},
      change_class: class,
      change_level: level,
      license_touch: license_touch,
      pool: pool_atom(pool_policy),
      branch: Map.get(obs, "branch", "main"),
      paths: Map.get(obs, "files", []),
      attestations: atts,
      contributing_bots: atts |> Enum.map(& &1.bot) |> Enum.uniq()
    }
  end

  # ── Classification (pure) ────────────────────────────────────────────────────

  @doc "Classify an observation into `{change_class, change_level, license_touch}`."
  def classify(obs) do
    files = Map.get(obs, "files", []) || []
    branch = Map.get(obs, "branch", "") || ""
    title = Map.get(obs, "title", "") || ""
    labels = Map.get(obs, "labels", []) || []
    repo = Map.get(obs, "repo", "") || ""

    license_touch? = Enum.any?(files, &license_path?/1)
    level = if Enum.any?(files, &meta_path?(&1, repo)), do: :meta, else: :object
    {class_of(files, branch, title, labels, license_touch?), level, license_touch?}
  end

  # Priority order, most-guarded first: license → bump → proof → security →
  # docs → chore → refactor → (everything else routes via rhodibot as :feature).
  defp class_of(files, branch, title, labels, license_touch?) do
    cond do
      license_touch? -> :license_touch
      bump?(branch, title) -> :bump
      Enum.any?(files, &proof_path?/1) -> :proof
      security?(files, branch, labels) -> :security
      files != [] and Enum.all?(files, &doc_path?/1) -> :docs
      chore?(branch, title) -> :chore
      refactor?(branch, title) -> :refactor
      true -> :feature
    end
  end

  # license / SPDX (filename-level; in-file SPDX edits are the scanner's content veto)
  defp license_path?(p) do
    base = Path.basename(p)
    ext = ext(p)

    ext in [".license", ".spdx"] or
      base in ["LICENSE", "COPYING", "COPYING.LESSER", "NOTICE", "UNLICENSE"] or
      String.starts_with?(base, "LICENSE.") or String.starts_with?(base, "LICENSE-") or
      String.contains?(p, "/licenses/") or String.starts_with?(p, "licenses/")
  end

  # meta = the oracle surfaces the actuator must never self-approve
  defp meta_path?(p, repo) do
    String.contains?(p, ".github/workflows/") or
      (String.contains?(p, ".github/") and ext(p) in [".yml", ".yaml"]) or
      String.contains?(p, "/bot_directives/") or String.starts_with?(p, "bot_directives/") or
      String.contains?(p, "lib/rules/") or
      String.contains?(p, "merge-orchestration/schemas") or
      String.contains?(p, "merge-orchestration/ci-templates") or
      (repo == "hyperpolymath/standards" and
         (String.contains?(p, "contractiles/") or String.contains?(p, "rulesets/")))
  end

  defp proof_path?(p),
    do: ext(p) in [".agda", ".idr", ".idr2", ".lean", ".v", ".thy"] or String.contains?(p, "proofs/")

  defp doc_path?(p), do: ext(p) in [".adoc", ".md", ".txt", ".rst"] or String.contains?(p, "docs/")

  defp security?(files, branch, labels) do
    Enum.any?(files, fn p ->
      String.contains?(p, "SECURITY") or String.contains?(p, "security/")
    end) or String.starts_with?(branch, "security/") or "security" in labels
  end

  defp bump?(branch, title) do
    String.starts_with?(branch, "dependabot/") or String.starts_with?(branch, "renovate/") or
      String.starts_with?(title, "Bump ") or String.starts_with?(title, "chore(deps") or
      String.starts_with?(title, "build(deps")
  end

  defp chore?(branch, title),
    do: String.starts_with?(branch, "chore/") or String.starts_with?(downcase(title), "chore")

  defp refactor?(branch, title),
    do: String.starts_with?(branch, "refactor/") or String.starts_with?(downcase(title), "refactor")

  defp ext(p), do: p |> Path.extname() |> String.downcase()
  defp downcase(nil), do: ""
  defp downcase(s), do: String.downcase(s)

  # ── Shape normalisation ──────────────────────────────────────────────────────

  @doc "Map an on-disk (string-keyed) or in-memory (atom-keyed) attestation onto the council shape."
  def normalize_attestation(a) do
    base = %{
      bot: pick(a, "bot"),
      verdict: verdict_atom(pick(a, "verdict")),
      confidence: pick(a, "confidence") || 0.0
    }

    case pick(a, "rationale") do
      nil -> base
      r -> Map.put(base, :rationale, r)
    end
  end

  @doc "Map a `RepoPoolPolicy` (`\"P2\"`/`\"mass_squash\"`) onto the Strategist pool atom; nil ⇒ `:p1`."
  def pool_atom(nil), do: :p1
  def pool_atom(%{} = policy), do: policy |> pick("pool") |> pool_atom()
  def pool_atom("P0"), do: :p0
  def pool_atom("P1"), do: :p1
  def pool_atom("P2"), do: :p2
  def pool_atom("P3"), do: :p3
  def pool_atom("mass_squash"), do: :mass_squash
  def pool_atom(a) when a in [:p0, :p1, :p2, :p3, :mass_squash], do: a
  def pool_atom(_), do: :p1

  defp verdict_atom(v) when v in [:approve, :hold, :veto], do: v
  defp verdict_atom("approve"), do: :approve
  defp verdict_atom("hold"), do: :hold
  defp verdict_atom("veto"), do: :veto
  defp verdict_atom(_), do: :hold

  # accept both "key" and :key
  defp pick(map, key) when is_map(map), do: Map.get(map, key) || Map.get(map, safe_atom(key))
  defp safe_atom(k) when is_binary(k), do: String.to_atom(k)
  defp safe_atom(k), do: k

  # ── Store-backed resolvers (the production default source) ────────────────────

  @doc """
  Build `{resolve_pool, resolve_attestations}` over a verisim-data store dir.

  Layout (token-free, what the scanning bots deposit):
    * `<dir>/pools/<repo-flattened>.json`  — the repo's `RepoPoolPolicy`
    * `<dir>/attestations/*.json`          — signed attestations (filtered by subject)

  `opts[:decode]` defaults to `&Jason.decode!/1` (a real hypatia dep). Repo names
  are flattened `owner/name -> owner__name` for the pool filename.
  """
  def store_resolvers(dir, opts \\ []) do
    decode = Keyword.get(opts, :decode, &json_decode!/1)
    {file_pool_resolver(Path.join(dir, "pools"), decode),
     dir_attestation_resolver(Path.join(dir, "attestations"), decode)}
  end

  @doc "A `(repo -> pool_policy | nil)` that reads `<dir>/<owner__name>.json`."
  def file_pool_resolver(dir, decode \\ &__MODULE__.json_decode!/1) do
    fn repo ->
      path = Path.join(dir, flatten_repo(repo) <> ".json")
      case File.read(path) do
        {:ok, body} -> decode.(body)
        _ -> nil
      end
    end
  end

  @doc "A `(repo, number -> [attestation])` that reads every `*.json` in `dir` and filters by subject."
  def dir_attestation_resolver(dir, decode \\ &__MODULE__.json_decode!/1) do
    fn repo, number ->
      case File.ls(dir) do
        {:ok, names} ->
          names
          |> Enum.filter(&String.ends_with?(&1, ".json"))
          |> Enum.map(&decode.(File.read!(Path.join(dir, &1))))
          |> Enum.filter(&attests_to?(&1, repo, number))

        _ ->
          []
      end
    end
  end

  @doc false
  # Indirection so the default stays late-bound: prod resolves Jason; the dep-free
  # test harness never reaches here because it injects its own decode.
  def json_decode!(body), do: apply(Jason, :decode!, [body])

  defp attests_to?(att, repo, number) do
    subj = pick(att, "subject") || %{}
    pick(subj, "repo") == repo and pick(subj, "number") == number
  end

  defp flatten_repo(repo), do: String.replace(repo, "/", "__")
end

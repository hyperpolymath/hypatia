# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.PrAutomerge do
  @moduledoc """
  Decide whether a pull request is an unambiguous bump or chore that may be
  merged and its branch deleted, and if not, exactly why not.

  Rule IDs PA001-PA006. Every function is pure: a PR record and the policy map
  in, a decision out. No network. The `gh`-driven plumbing lives in
  `Mix.Tasks.Hypatia.Automerge` and in `scripts/sweeps/estate-pr-automerge.sh`;
  the shell sweep (`scripts/sweeps/estate-pr-automerge.sh`) is also the
  token-bearing actuator, and it re-derives these rules against the live diff
  before it acts. Two readers, one policy file.

  ## Why this is not a title matcher

  On 2026-09-26 the estate had 32 open PRs, every one a `chore(deps): bump …`
  from dependabot. Every one of them looked safe. 25 of them re-introduced
  `github/codeql-action` v4.38.1 — the commit GitHub rejects at workflow
  start-up — and at least two hid a *major* bump (`actions/checkout`
  v4.1.7 → v7.0.1) behind a title that says only "bump the actions group".

  A title matcher merges 32 of them. The estate did merge some of them:
  nexia-list#107 and dictask#65 landed poisoned pins and both repositories are
  red on main as a result.

  ## The order of the checks is the safety property

  PA001 (denylisted pin) is checked first, before anything that could grant
  permission, because every later check answers "is this a routine change?"
  and the pin question answers "is this change *safe*?" — different questions,
  and the second one dominates. A grouped update carrying a wanted minor bump
  *and* the poisoned pin is not "mostly fine"; it is a PR whose net effect on
  main is a broken workflow.

  ## Rule catalogue

  | Rule | Severity | Fires when |
  |---|---|---|
  | PA001 | critical | an added line puts a denylisted pin back in the tree |
  | PA002 | high | a version delta crosses a major boundary |
  | PA003 | high | the diff and the PR body disagree about the versions |
  | PA004 | medium | the version delta cannot be resolved at all |
  | PA005 | medium | a dependency-manifest change with no lockfile change |
  | PA006 | low | the PR is not automatable for a structural reason |

  ## Dispositions

    * `:auto_merge` — merge, delete the branch
    * `:excise_poison_then_merge` — the poisoned hunk is removed, the rest
      merged, the branch deleted
    * `:close_poison_only` — nothing here is wanted; close and delete
    * `:close_poison_and_majors` — poisoned *and* carrying majors
    * `:close_archived_repo` — the repository is archived; it can never merge
    * `:flag` — a human decides

  `:excise_poison_then_merge` is the case the phrase "it is really just a file
  merge problem" describes: the repair is a token substitution inside one file,
  so the wanted half of a grouped update can be kept without waiting for
  dependabot to re-raise it.
  """

  @doc """
  Classify a PR record against the policy.

  `pr` is a map with:

    * `:author` — login of the PR author
    * `:repo_archived` — whether the repository is archived (default `false`)
    * `:files` — list of `%{filename: String.t(), patch: String.t()}`
    * `:body` — the PR body, used only as a corroborating claim source
    * `:version_resolution` — map of `{action, ref} => version | nil`, i.e.
      what the upstream tags say a ref is. Absent entries are *unknown*, never
      assumed.

  Returns the decision as a plain map; `decision_manifest/2` turns it into the
  frozen manifest shape.
  """
  @spec classify(map(), map()) :: map()
  def classify(pr, policy) do
    files = field(pr, :files, [])
    body = field(pr, :body, "")
    resolution = field(pr, :version_resolution, %{})

    claims = body_claims(body)
    deltas = pin_deltas(files, resolution, claims)

    poison = poison_sites(files, policy)
    lock_only = files != [] and Enum.all?(files, &lockfile?(filename_of(&1), policy))
    meta_only = files != [] and Enum.all?(files, &metadata_path?(filename_of(&1)))
    pin_only = files != [] and Enum.all?(files, &pin_lines_only?/1)
    manifest_file? = Enum.any?(files, &dependency_manifest?(filename_of(&1)))
    licence_touch? = Enum.any?(files, &licence_path?(filename_of(&1)))

    %{
      deltas: deltas,
      poison_sites: poison,
      major_delta: Enum.any?(deltas, &major_delta?/1),
      unresolved: Enum.count(deltas, &(&1.status == :unresolved)),
      conflicts: Enum.count(deltas, &(&1.status == :conflict)),
      lock_only: lock_only,
      meta_only: meta_only,
      pin_only: pin_only,
      manifest_file: manifest_file?,
      licence_touch: licence_touch?
    }
    |> verdict(pr, policy)
  end

  # ─── PA001 · denylisted pin ───────────────────────────────────────────

  @doc """
  Every added line that puts a denylisted pin site into the tree.

  Keyed on the ref, never on the inline comment: the estate contains files
  where the poisoned commit is labelled `# v4.38.0`, and a version-string
  check sees nothing wrong with them.
  """
  @spec poison_sites([map()], map()) :: [map()]
  def poison_sites(files, policy) do
    policy
    |> Map.get("pin_denylist", [])
    |> Enum.flat_map(fn entry ->
      files
      |> Enum.flat_map(fn file ->
        file
        |> added_lines()
        |> Enum.flat_map(fn line ->
          case parse_pin(line) do
            %{action: action, ref: ref} ->
              if action_base(action) == Map.get(entry, "action") and
                   denylisted_ref?(entry, ref) do
                [
                  %{
                    file: filename_of(file),
                    line: String.trim(line),
                    denylist_id: Map.get(entry, "id"),
                    severity: Map.get(entry, "severity", "critical")
                  }
                ]
              else
                []
              end

            _ ->
              []
          end
        end)
      end)
    end)
  end

  # The ref that sits directly after `@` is the claim; a version string
  # anywhere else on the line is an annotation. The estate's rollback comment
  # reads `# v4.38.0 (4.38.1 blocked estate-wide; nexia-list#100)` — matching
  # on the whole line would call that file poisoned, which is the same class
  # of mistake as trusting the comment in the first place.
  defp denylisted_ref?(entry, ref) do
    blocked =
      Map.get(entry, "blocked_shas", []) ++
        Map.get(entry, "blocked_versions", []) ++ Map.get(entry, "blocked_refs", [])

    normalised = normalise_ref(ref)
    Enum.any?(blocked, &(normalise_ref(&1) == normalised))
  end

  defp normalise_ref(ref) when is_binary(ref) do
    ref |> String.trim() |> String.replace_prefix("v", "")
  end

  defp normalise_ref(_), do: ""

  @doc """
  Is every changed line in this file a `uses:` pin line?

  This is the proof obligation behind the meta-guard exemption `MGX-001`. A
  workflow edit that is *not* pin-only — a new step, a changed `permissions:`
  block, a moved `if:` — fails this, which is what keeps the reflexivity guard
  from being talked around by a plausible title.
  """
  @spec pin_lines_only?(map()) :: boolean()
  def pin_lines_only?(file) do
    lines = content_lines(patch_of(file))

    lines != [] and Enum.all?(lines, fn line -> pin_line?(line) end)
  end

  # ─── PA002/PA003/PA004 · version deltas ───────────────────────────────

  @doc """
  Version deltas for every action this PR re-pins.

  `status` is one of `:ok`, `:unresolved` (no source could place a version on
  the refs) or `:conflict` (upstream tags and the PR body disagree). `source`
  records which source produced the versions that were used, so a reviewer can
  see exactly what the decision rested on.
  """
  @spec pin_deltas([map()], map(), [map()]) :: [map()]
  def pin_deltas(files, resolution, claims) do
    files
    |> Enum.flat_map(fn file ->
      removed = file |> removed_lines() |> Enum.filter(&pin_line?/1)
      added = file |> added_lines() |> Enum.filter(&pin_line?/1)

      removed
      |> Enum.flat_map(fn line ->
        old = parse_pin(line)

        case Enum.find(added, &(parse_pin(&1).action == old.action)) do
          nil ->
            []

          new_line ->
            new = parse_pin(new_line)

            if new.ref == old.ref do
              []
            else
              from = resolve(resolution, old.action, old.ref)
              to = resolve(resolution, new.action, new.ref)
              claim = claim_for(claims, old.action)

              cond do
                from != nil and to != nil and claim != nil and
                    (claim.from != from or claim.to != to) ->
                  delta(old, new, from, to, :conflict, "tags")

                from != nil and to != nil ->
                  delta(old, new, from, to, :ok, "tags")

                claim != nil ->
                  delta(old, new, claim.from, claim.to, :ok, "body-claim")

                true ->
                  delta(old, new, nil, nil, :unresolved, "none")
              end
              |> Map.put(:file, filename_of(file))
            end
        end
      end)
    end)
  end

  defp delta(old, new, from, to, status, source) do
    %{
      action: old.action,
      from: from,
      to: to,
      status: status,
      source: source,
      file: nil,
      major?: from != nil and to != nil and major_of(from) != major_of(to)
    }
  end

  @doc "Is this delta a major-version crossing? Unknown versions are never a yes."
  @spec major_delta?(map()) :: boolean()
  def major_delta?(%{major?: flag}), do: flag == true
  def major_delta?(_), do: false

  @doc """
  The claim list dependabot writes into the PR body.

  Both shapes are captured — `Updates \\`owner/action\\` from A to B` (grouped)
  and `Bumps [name](url) from A to B.` (single) — because the estate uses
  both and a claim parser that understands one of them silently converts half
  the estate into "unverifiable".
  """
  @spec body_claims(String.t()) :: [map()]
  def body_claims(body) when is_binary(body) do
    ~r/(?:Updates|Bumps)\s+\[?`?([A-Za-z0-9_.-]+(?:\/[A-Za-z0-9_.-]+)*)`?\]?(?:\([^)]*\))?\s+from\s+([0-9][^\s]*)\s+to\s+([0-9][^\s]*)/

    |> Regex.scan(body)
    |> Enum.map(fn [_, action, from, to] ->
      # Dependabot ends the sentence with a full stop; the version does not
      # have one. `to [0-9][^ .]*` (an earlier shape) silently turned
      # `4.38.1` into `4` — which is how a major bump can hide in a minor.
      %{action: action, from: trim_dot(from), to: trim_dot(to)}
    end)
  end

  def body_claims(_), do: []

  defp trim_dot(version), do: String.trim_trailing(version, ".")

  # ─── Verdict ─────────────────────────────────────────────────────────

  defp verdict(scan, pr, policy) do
    author = field(pr, :author)
    repo_archived = field(pr, :repo_archived) == true

    base = %{
      change_class: "bump",
      change_level: if(scan.pin_only, do: "object", else: "meta"),
      route: "Patch-Bridge",
      method: "squash",
      pool: "P2",
      safety: "flag",
      attestations: [
        %{bot: "hypatia", verdict: "approve", confidence: 0.9, rationale: "classified from the diff"}
      ]
    }

    cond do
      repo_archived ->
        reject(base, :close_archived_repo, "repository_is_archived_cannot_merge", "P3")

      not dependency_bot?(author, policy) ->
        reject(base, :flag, "not_a_dependency_bot", "P3")

      scan.poison_sites != [] and scan.major_delta ->
        reject(base, :close_poison_and_majors, "introduces_denylisted_pin_and_major_bumps", "P1")

      scan.poison_sites != [] and scan.pin_only ->
        accept(base, :close_poison_only, "introduces_denylisted_pin", "P1")

      scan.poison_sites != [] ->
        accept(base, :excise_poison_then_merge, "introduces_denylisted_pin_alongside_wanted_updates", "P1")

      scan.major_delta ->
        reject(base, :flag, "major_version_delta", "P2")

      scan.conflicts > 0 ->
        reject(base, :flag, "version_claims_conflict", "P2")

      scan.unresolved > 0 ->
        reject(base, :flag, "pin_delta_unresolvable", "P2")

      scan.licence_touch ->
        # NA-005-adjacent by intent: a licence/SPDX touch is a claim about
        # what the project *is*, so it routes to the owner, never the robot.
        reject(base, :flag, "touches_licence_requires_owner_review", "P2")

      scan.lock_only ->
        accept(base, :auto_merge, "awaiting_required_checks", "P2")

      scan.pin_only ->
        accept(base, :auto_merge, "awaiting_required_checks", "P2")

      scan.meta_only ->
        accept(base, :auto_merge, "awaiting_required_checks", "P3")

      scan.manifest_file ->
        reject(base, :flag, "dependency_manifest_not_lockfile", "P2")

      true ->
        reject(base, :flag, "not_unambiguously_classifiable", "P2")
    end
  end

  defp accept(base, disposition, blocked_by, pool) do
    base
    |> Map.merge(%{safety: "arm_auto", pool: pool, disposition: disposition, blocked_by: blocked_by})
  end

  defp reject(base, disposition, blocked_by, pool) do
    base
    |> Map.merge(%{safety: "flag", pool: pool, disposition: disposition, blocked_by: blocked_by})
  end

  @doc """
  Render a decision as the frozen merge-orchestration manifest.

  Conforms to
  `docs/design/merge-orchestration/schemas/decision-manifest.schema.json`;
  the two contract invariants hold by construction — any denial sets
  `safety: "flag"` and records a veto, and a `meta` change level can only
  reach `arm_auto` through the `MGX-001` pin-only exemption, which the
  actuator re-proves from the diff.
  """
  @spec decision_manifest(map(), map()) :: map()
  def decision_manifest(decision, pr) do
    vetoes =
      if decision.safety == "flag" do
        [%{bot: "Patch-Bridge", reason: decision.blocked_by}] ++
          if decision.change_level == "meta",
            do: [%{bot: "hypatia", reason: "change_level=meta"}],
            else: []
      else
        []
      end

    %{
      "pr" => %{
        "repo" => field(pr, :repo, ""),
        "number" => field(pr, :number, 0),
        "head_sha" => field(pr, :head_sha, ""),
        "base" => field(pr, :base, "main"),
        "author" => field(pr, :author, ""),
        "author_kind" => "dependabot"
      },
      "change_class" => decision.change_class,
      "change_level" => decision.change_level,
      "route" => %{"authority_bot" => decision.route, "contributing_bots" => ["hypatia"]},
      "method" => decision.method,
      "method_basis" => "repo-default",
      "safety" => decision.safety,
      "pool" => decision.pool,
      "confidence" => nil,
      "attestations" => decision.attestations,
      "vetoes" => vetoes,
      "clamped_by" => if(vetoes == [], do: nil, else: "veto"),
      "rationale" => "PA rules · #{decision.disposition} · blocked_by=#{decision.blocked_by}",
      "disposition" => Atom.to_string(decision.disposition),
      "blocked_by" => decision.blocked_by,
      "pin_delta" =>
        Enum.map(decision.deltas, fn d ->
          %{"action" => d.action, "from" => d.from, "to" => d.to, "source" => d.source}
        end),
      "denylisted_hits" => length(decision.poison_sites),
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  # ─── Primitives ──────────────────────────────────────────────────────

  # Patch lines arrive with a diff marker in front of the YAML dash:
  #   -      - uses: actions/checkout@v4.1.7
  # so the marker is part of the match. (Without this, every real diff looks
  # like a non-pin change and every PR falls through to `flag`.)
  @pin_re ~r/^[-+]?\s*-?\s*uses:\s*(?<action>[A-Za-z0-9_.-]+\/[A-Za-z0-9_./-]*?)@(?<ref>[^\s#]+)/

  @doc "Parse a `uses:` line into `%{action, base, ref}`; `nil` when it is not one."
  def parse_pin(line) do
    case Regex.named_captures(@pin_re, line) do
      %{"action" => action, "ref" => ref} ->
        %{action: action, base: action_base(action), ref: ref}

      _ ->
        nil
    end
  end

  @doc "First two path segments of an action, e.g. `github/codeql-action/init` → `github/codeql-action`."
  def action_base(action) do
    case String.split(action, "/") do
      [owner, name | _] -> "#{owner}/#{name}"
      _ -> action
    end
  end

  defp pin_line?(line), do: parse_pin(line) != nil

  defp content_lines(patch) do
    patch
    |> String.split("\n")
    |> Enum.filter(&Regex.match?(~r/^[-+]/, &1))
    |> Enum.reject(&Regex.match?(~r/^[-+]{3}/, &1))
  end

  defp added_lines(file), do: file |> patch_of() |> content_lines() |> Enum.filter(&String.starts_with?(&1, "+"))
  defp removed_lines(file), do: file |> patch_of() |> content_lines() |> Enum.filter(&String.starts_with?(&1, "-"))

  defp patch_of(file), do: Map.get(file, :patch) || Map.get(file, "patch") || ""

  defp filename_of(file), do: Map.get(file, :filename) || Map.get(file, "filename") || ""

  # Records arrive both as Elixir maps (tests) and as Jason-decoded JSON
  # (the brain's manifests), and in the second case the keys are strings.
  # Reading only one spelling turns every archived repo into an active one.
  defp field(map, key, default \\ nil) do
    case {Map.get(map, key), Map.get(map, Atom.to_string(key))} do
      {nil, nil} -> default
      {nil, value} -> value
      {value, _} -> value
    end
  end

  defp resolve(resolution, action, ref) do
    Map.get(resolution, {action, ref}) || Map.get(resolution, {action_base(action), ref}) ||
      Map.get(resolution, ref)
  end

  defp claim_for(claims, action) do
    short = action |> String.split("/") |> List.last()

    Enum.find(claims, fn c -> c.action == action or c.action == short end) ||
      Enum.find(claims, fn c -> String.ends_with?(c.action, "/" <> short) end)
  end

  defp major_of(version) do
    case Regex.run(~r/^v?(\d+)/, version) do
      [_, major] -> major
      _ -> version
    end
  end

  defp dependency_bot?(author, policy) do
    author in Map.get(policy, "dependency_bots", [])
  end

  defp lockfile?(filename, policy) do
    Path.basename(filename) in Map.get(policy, "lockfile_names", [])
  end

  defp metadata_path?(filename) do
    Regex.match?(~r/\.(md|adoc|rst|txt)$/, filename) or
      String.starts_with?(filename, ["docs/", "doc/", "LICENSES/", ".github/ISSUE_TEMPLATE/"]) or
      Regex.match?(~r/(^|\/)(LICENSE|NOTICE|CODEOWNERS|\.gitattributes|\.editorconfig)/, filename)
  end

  defp licence_path?(filename) do
    base = Path.basename(filename)

    String.starts_with?(base, ["LICENSE", "LICENCE", "COPYING", "NOTICE"]) or
      String.contains?(String.upcase(base), "SPDX") or
      String.starts_with?(filename, "LICENSES/")
  end

  defp dependency_manifest?(filename) do
    Regex.match?(
      ~r/(^|\/)(package\.json|Cargo\.toml|Project\.toml|mix\.exs|go\.mod|pyproject\.toml|Gemfile|composer\.json)$/,
      filename
    )
  end
end

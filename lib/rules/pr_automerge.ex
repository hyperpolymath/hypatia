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

  alias Hypatia.Rules.PinSites

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

    files
    |> scan_of(pin_deltas(files, resolution, claims), poison_sites(files, policy), policy)
    |> verdict(pr, policy)
  end

  defp scan_of(files, deltas, poison, policy) do
    %{
      deltas: deltas,
      poison_sites: poison,
      major_delta: Enum.any?(deltas, &major_delta?/1),
      unresolved: Enum.count(deltas, &(&1.status == :unresolved)),
      conflicts: Enum.count(deltas, &(&1.status == :conflict)),
      lock_only: files != [] and Enum.all?(files, &lockfile?(filename_of(&1), policy)),
      meta_only: files != [] and Enum.all?(files, &metadata_path?(filename_of(&1))),
      pin_only: files != [] and Enum.all?(files, &pin_lines_only?/1),
      manifest_file: Enum.any?(files, &dependency_manifest?(filename_of(&1))),
      licence_touch: Enum.any?(files, &licence_path?(filename_of(&1)))
    }
  end

  # The PR as it would stand once its poisoned pins are excised: a file whose
  # every added line carries only denylisted sites drops out, and so does a
  # delta onto a denylisted ref. Everything else is still there to classify.
  defp remainder_scan(scan, files, policy) do
    kept = Enum.reject(files, &poison_only_file?(&1, policy))

    deltas =
      Enum.reject(
        scan.deltas,
        &denylisted_pin?(%{base: action_base(&1.action), ref: &1.to_ref}, policy)
      )

    scan_of(kept, deltas, [], policy)
  end

  defp poison_only_file?(file, policy) do
    added = added_entries(file)

    pin_lines_only?(file) and added != [] and
      Enum.all?(added, fn {line, _certain?, continued} ->
        pins = parse_pins(line) ++ continued
        pins != [] and Enum.all?(pins, &denylisted_pin?(&1, policy))
      end)
  end

  defp denylisted_pin?(%{base: base, ref: ref}, policy) do
    policy
    |> Map.get("pin_denylist", [])
    |> Enum.any?(fn entry ->
      String.downcase(base) == String.downcase(Map.get(entry, "action", "")) and
        denylisted_ref?(entry, ref)
    end)
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
        |> added_entries()
        |> Enum.flat_map(fn {line, certain?, continued} ->
          # Every site on the line: a flow sequence can put several steps on
          # one changed line, and a pin judged by its first site alone lets a
          # denylisted second one through to `arm_auto`. A line that is the
          # value of a `uses:` key on the line above it (the key unchanged
          # context, the reference re-pinned) holds that site too. The action
          # is compared case-folded — GitHub resolves `GitHub/CodeQL-Action`
          # and `github/codeql-action` to one repository.
          line
          |> parse_pins()
          |> Kernel.++(continued)
          |> Enum.filter(fn %{base: base, ref: ref} ->
            String.downcase(base) == String.downcase(Map.get(entry, "action", "")) and
              denylisted_ref?(entry, ref)
          end)
          |> Enum.map(fn _pin ->
            %{
              file: filename_of(file),
              line: String.trim(line),
              denylist_id: Map.get(entry, "id"),
              severity: Map.get(entry, "severity", "critical"),
              # false when the hunk shows the line among script text: it may
              # be a fixture inside a `run: |` whose header is outside the hunk.
              certain: certain?
            }
          end)
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
    trimmed = ref |> String.trim() |> String.replace_prefix("v", "")
    # A commit ID in upper-case hex is the same object the table carries in
    # lower case; git resolves either spelling.
    if Regex.match?(~r/^[0-9a-fA-F]{40}$/, trimmed), do: String.downcase(trimmed), else: trimmed
  end

  defp normalise_ref(_), do: ""

  @doc """
  Is every changed line in this file a `uses:` pin line?

  This is the proof obligation behind the meta-guard exemption `MGX-001`. A
  workflow edit that is *not* pin-only — a new step, a changed `permissions:`
  block, a moved `if:` — fails this, which is what keeps the reflexivity guard
  from being talked around by a plausible title. So does a diff of pin lines
  that adds or swaps an action: the removed and added sites must name the
  same actions, as many times each.
  """
  @spec pin_lines_only?(map()) :: boolean()
  def pin_lines_only?(file) do
    lines = changed_lines(file)

    # A changed line inside a block scalar (`run: |`) is script text, never a
    # pin line, so a script edit is not a pin-only change.
    # A line the hunk shows among script text is not known to be a pin line.
    # And every pin the file adds replaces a removed pin of the same action:
    # a new step (`+ { uses: a/b@main },`) or a swapped action is a change to
    # what the workflow runs, not a re-pin of what it already ran.
    lines != [] and
      Enum.all?(lines, fn {line, scalar?, certain?, continued} ->
        certain? and not scalar? and (pin_line?(line) or continued_value_line?(line, continued))
      end) and
      action_multiset(removed_pins(file)) == action_multiset(added_pins(file))
  end

  # A changed line that is nothing but the reference a `uses:` key above it
  # holds (`-          a/b@x` -> `+          a/b@y`): a re-pin as much as a
  # one-line `uses:` is.
  defp continued_value_line?(line, [_ | _]),
    do: Regex.match?(~r/^\s*\S+\s*(?:#.*)?$/, String.replace(line, ~r/^[-+]/, ""))

  defp continued_value_line?(_line, []), do: false

  defp action_multiset(pins) do
    pins |> Enum.map(&String.downcase(&1.action)) |> Enum.sort()
  end

  # ─── PA002/PA003/PA004 · version deltas ───────────────────────────────

  @doc """
  Version deltas for every action this PR re-pins.

  Each removed pin is paired with the first added pin for the same action
  in the same file that no earlier removal has claimed. Unpaired pins and
  unchanged refs are omitted. Returns a
  flat list of maps with `:action`, `:file`, `:from`, `:to`, `:status`,
  `:source` and `:major?`.

  `resolution` supplies versions keyed by `{action, ref}`, then
  `{action_base, ref}`, then `ref`, in that order of preference. If either
  ref is unresolved, a matching claim from `body_claims/1` supplies both
  versions; without a claim, both are `nil`.

  `status` is one of `:ok`, `:unresolved` (no source supplied both versions)
  or `:conflict` (resolved versions and the PR body disagree). `source`
  records which source produced the versions that were used, so a reviewer can
  see exactly what the decision rested on.
  """
  @spec pin_deltas([map()], map(), [map()]) :: [map()]
  def pin_deltas(files, resolution, claims) do
    # Basenames two different actions of the PR share (`acme/setup`,
    # `other/setup`): a claim naming only that basename belongs to neither.
    shared =
      files
      |> Enum.flat_map(&removed_pins/1)
      |> Enum.map(&String.downcase(&1.action))
      |> Enum.uniq()
      |> Enum.frequencies_by(&(&1 |> String.split("/") |> List.last()))
      |> Enum.filter(fn {_, n} -> n > 1 end)
      |> MapSet.new(&elem(&1, 0))

    files
    |> Enum.flat_map(fn file ->
      # Every structural site of every changed line: a flow sequence carries
      # several steps on one line (`steps: [ { uses: a@x }, { uses: b@y } ]`),
      # and each of them is a delta of its own — including a major bump on a
      # line that does not start with `uses:`.
      removed = removed_pins(file)
      added = added_pins(file)

      # Each added site pairs with ONE removed site of the same action, in
      # order: a line that keeps one `actions/checkout` at v4 and moves a
      # second from v4 to v7 carries the major bump, which pairing every
      # removal with the first addition would hide.
      removed
      |> Enum.flat_map_reduce(added, fn old, pool ->
        case Enum.find_index(pool, &(String.downcase(&1.action) == String.downcase(old.action))) do
          nil ->
            {[], pool}

          i ->
            {Enum.at(pool, i) |> pin_delta(old, file, resolution, claims, shared),
             List.delete_at(pool, i)}
        end
      end)
      |> elem(0)
    end)
  end

  defp pin_delta(new, old, file, resolution, claims, shared) do
    if new.ref == old.ref do
      []
    else
      from = resolve(resolution, old.action, old.ref)
      to = resolve(resolution, new.action, new.ref)
      claim = claim_for(claims, old.action, shared)

      cond do
        from != nil and to != nil and claim != nil and
            (claim.from != from or claim.to != to) ->
          delta(old, new, from, to, :conflict, "tags")

        from != nil and to != nil ->
          delta(old, new, from, to, :ok, "tags")

        # One endpoint resolved: the claim must agree with it, or the
        # evidence there is contradicts it (a claimed `2.0.0 -> 2.1.0` from
        # a ref that resolves to `1.0.0` may hide a major crossing).
        claim != nil and ((from != nil and claim.from != from) or (to != nil and claim.to != to)) ->
          delta(old, new, from || claim.from, to || claim.to, :conflict, "tags")

        claim != nil ->
          delta(old, new, claim.from, claim.to, :ok, "body-claim")

        true ->
          delta(old, new, nil, nil, :unresolved, "none")
      end
      |> Map.put(:file, filename_of(file))
      # flat_map_reduce needs a list: a bare map would be flattened into its
      # {key, value} pairs.
      |> List.wrap()
    end
  end

  defp delta(old, new, from, to, status, source) do
    %{
      action: old.action,
      from: from,
      to: to,
      to_ref: new.ref,
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

    # The decision carries the scan it rests on (deltas, licence_touch, …),
    # so decision_manifest/2 and any reviewer can see what it was decided from.
    base =
      Map.merge(scan, %{
        change_class: "bump",
        change_level: if(scan.pin_only, do: "object", else: "meta"),
        route: "Patch-Bridge",
        method: "squash",
        pool: "P2",
        safety: "flag",
        attestations: [
          %{
            bot: "hypatia",
            verdict: "approve",
            confidence: 0.9,
            rationale: "classified from the diff"
          }
        ]
      })

    cond do
      repo_archived ->
        reject(base, :close_archived_repo, "repository_is_archived_cannot_merge", "P3")

      not dependency_bot?(author, policy) ->
        reject(base, :flag, "not_a_dependency_bot", "P3")

      Enum.any?(scan.poison_sites, &(not &1.certain)) ->
        # The hunk shows a denylisted pin among script text: it may be a
        # fixture inside a `run: |` whose header is outside the hunk. Closing
        # the PR or excising the line would both act on a guess.
        reject(base, :flag, "denylisted_pin_context_unknown", "P1")

      scan.poison_sites != [] and scan.major_delta ->
        reject(base, :close_poison_and_majors, "introduces_denylisted_pin_and_major_bumps", "P1")

      scan.poison_sites != [] and scan.pin_only ->
        # Closing is not merging: a poisoned pin must never arm automerge.
        reject(base, :close_poison_only, "introduces_denylisted_pin", "P1")

      scan.poison_sites != [] ->
        # Finding the poison must not upgrade a PR that would be flagged
        # without it: what remains after the excision is classified on its
        # own, and only a remainder that would arm by itself is merged.
        rest = verdict(remainder_scan(scan, field(pr, :files, []), policy), pr, policy)

        if rest.safety == "arm_auto" do
          accept(
            base,
            :excise_poison_then_merge,
            "introduces_denylisted_pin_alongside_wanted_updates",
            "P1"
          )
        else
          base
          |> Map.put(:remainder_blocked_by, rest.blocked_by)
          |> reject(:flag, "introduces_denylisted_pin_alongside_unmergeable_changes", "P1")
        end

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
    |> Map.merge(%{
      safety: "arm_auto",
      pool: pool,
      disposition: disposition,
      blocked_by: blocked_by
    })
  end

  defp reject(base, disposition, blocked_by, pool) do
    base
    |> Map.merge(%{safety: "flag", pool: pool, disposition: disposition, blocked_by: blocked_by})
  end

  @doc """
  Render a decision as the frozen merge-orchestration manifest.

  Takes the decision from `classify/2` and PR metadata with atom or string
  keys. Includes pin deltas, the denylisted-site count and the current UTC
  timestamp in ISO 8601 format. The author kind is always `"dependabot"`.

  A decision with `safety: "flag"` gets a string-keyed Patch-Bridge veto,
  plus a hypatia veto when its change level is `"meta"`, and
  `"clamped_by" => "veto"`. Other safety values produce no vetoes and a
  `nil` clamp. Safety and change level are copied from the decision;
  this function does not validate the result against
  `docs/design/merge-orchestration/schemas/decision-manifest.schema.json`.

  Missing required keys in the decision or its delta maps raise `KeyError`.
  """
  @spec decision_manifest(map(), map()) :: map()
  def decision_manifest(decision, pr) do
    vetoes =
      if decision.safety == "flag" do
        [%{"bot" => "Patch-Bridge", "reason" => decision.blocked_by}] ++
          if decision.change_level == "meta",
            do: [%{"bot" => "hypatia", "reason" => "change_level=meta"}],
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
  # The key and the value may each be quoted (`"uses": "owner/repo@sha"`), the
  # step may be a flow mapping (`- { uses: owner/repo@sha, with: ... }`), and
  # the ref ends at a flow delimiter as well as at a quote, space or comment.
  @pin_re ~r/^[-+]?\s*-?\s*(?:\{\s*)?["']?uses["']?\s*:\s*["']?(?<action>[A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@(?<ref>[^\s#"',}\]]+)/

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

  @doc """
  Every structural `uses:` site on a diff line, in order, as
  `%{action, base, ref}`.

  A flow sequence may put several steps on one line
  (`+  { uses: a/b@x }, { uses: c/d@y },`); `parse_pin/1` reads only the
  first. Which sites count is `Hypatia.Rules.PinSites`'s rule, the same one
  PI001 reads a file with.
  """
  @spec parse_pins(String.t()) :: [map()]
  def parse_pins(line) do
    line
    |> String.replace(~r/^[-+]/, "")
    |> PinSites.sites()
    |> Enum.map(fn %{action: action, ref: ref} ->
      %{action: action, base: action_base(action), ref: ref}
    end)
  end

  defp pin_line?(line), do: line |> String.replace(~r/^[-+]/, "") |> PinSites.pin_line?()

  # Every changed line of a patch as `{line, scalar?, certain?, continued}`.
  #
  # `continued`: the pins the line holds as the value of a `uses:` key whose
  # reference YAML continues below it (`- uses:`, then the reference on the
  # line under it, past any blank or comment lines) — read, like
  # `PinSites.continuations/1` reads a file, from the line's own side of the
  # hunk, so a re-pinned value under an unchanged key is still a pin site.
  #
  # `scalar?`: the line sits inside a scalar an earlier line of its own side
  # opened, as far as the hunk shows — a block scalar (`run: |`,
  # `script: >-`) or a quoted one continued over lines (`- run: "echo` ...
  # `done"`). Each side's context and changed lines are read in order by
  # `PinSites.line_states/1`, the reader PI001 reads a file with. A pin
  # written inside a script is text. So is a block `uses:` key the side's
  # indentation shows under no step or job — an input of that name (`with:`
  # then `uses: ...`, `PinSites.data_uses_lines/1`): data, not a pin line.
  #
  # `certain?`: the hunk does not show the line among script text. A line
  # after an opening quote the hunk never shows closed is not certain: the
  # scalar may run on past the hunk. A hunk
  # that starts inside a long `run: |` has no header to see, so the line's
  # YAML ancestry is unknown; the context around it says which. Every other
  # line of its side at its indentation or shallower — its siblings and
  # ancestors as far as the hunk shows — must be YAML (a key, a list item, a
  # flow bracket, a comment); one that is not (`cat > f <<'EOF'`, `EOF`,
  # `make test`) leaves it uncertain. A hunk with no such line gives no
  # evidence and leaves it certain.
  defp changed_lines(file) do
    file
    |> patch_of()
    |> String.split("\n")
    |> Enum.chunk_while(
      [],
      fn line, hunk ->
        if String.starts_with?(line, "@@"),
          do: {:cont, Enum.reverse(hunk), []},
          else: {:cont, [line | hunk]}
      end,
      fn hunk -> {:cont, Enum.reverse(hunk), []} end
    )
    |> Enum.flat_map(&hunk_lines/1)
  end

  defp hunk_lines(hunk) do
    hunk = Enum.reject(hunk, &Regex.match?(~r/^[-+]{3}/, &1))
    old = side_view(hunk, "-")
    new = side_view(hunk, "+")
    old_cont = Map.merge(side_continuations(old), side_aliases(old), fn _, a, b -> a ++ b end)
    new_cont = Map.merge(side_continuations(new), side_aliases(new), fn _, a, b -> a ++ b end)

    hunk
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, i} ->
      cond do
        String.starts_with?(line, "+") ->
          [line_status(line, i, new, Map.get(new_cont, i, []))]

        String.starts_with?(line, "-") ->
          [line_status(line, i, old, Map.get(old_cont, i, []))]

        true ->
          []
      end
    end)
  end

  # The continued `uses:` values of one side of a hunk, as
  # `%{hunk_index => [pin]}` on the value's line. A `:text` line, a key that
  # holds data among them, continues no pin.
  defp side_continuations(view) do
    texts = for {_, text, state} <- view, do: if(state == :text, do: "", else: text)
    index = view |> Enum.map(&elem(&1, 0)) |> List.to_tuple()

    for {_key, value, site} <- PinSites.continuations(texts), into: %{} do
      {elem(index, value - 1),
       [%{action: site.action, base: action_base(site.action), ref: site.ref}]}
    end
  end

  # The anchored values one side of a hunk shows an alias running, as
  # `%{hunk_index => [pin]}` on the anchor's line: `- uses: *bad` in context
  # with `BAD: &bad <action>@<ref>` changed (`PinSites.alias_sites/2`), or a
  # step given whole as an alias whose anchored mapping's `uses:` changed
  # (`PinSites.node_alias_sites/1`). The re-pinned anchor is the site, once
  # however many readers reach it; one that is already a site on its own
  # line is counted there, not twice.
  defp side_aliases(view) do
    texts = for {_, text, state} <- view, do: if(state == :text, do: "", else: text)
    side = Enum.join(texts, "\n")
    index = view |> Enum.map(&elem(&1, 0)) |> List.to_tuple()
    masked = PinSites.mask_lines(side)

    (PinSites.alias_sites(masked, PinSites.flow_prefixes(side)) ++ PinSites.node_alias_sites(side))
    |> Enum.uniq_by(&{&1.anchor_line, &1.site.ref_at})
    |> Enum.reject(fn %{anchor_line: n, site: site} ->
      texts |> Enum.at(n - 1) |> PinSites.sites() |> Enum.any?(&(&1.ref_at == site.ref_at))
    end)
    |> Enum.group_by(
      &elem(index, &1.anchor_line - 1),
      &%{action: &1.site.action, base: action_base(&1.site.action), ref: &1.site.ref}
    )
  end

  # One side of a hunk (context lines plus the changed lines of `mark`), in
  # order, as `{hunk_index, text, state}` — `state` as
  # `PinSites.line_states/1` reads the side: `:yaml`, `:text`, `:value` (a
  # `uses:` block scalar's one reference line) or `:open`; a
  # block `uses:` key the side shows under no step or job
  # (`PinSites.data_uses_lines/1`) is `:text` too.
  defp side_view(hunk, mark) do
    entries =
      hunk
      |> Enum.with_index()
      |> Enum.filter(fn {line, _} ->
        String.starts_with?(line, mark) or String.starts_with?(line, " ") or line == ""
      end)
      |> Enum.map(fn {line, i} -> {i, String.slice(line, 1..-1//1)} end)

    side = Enum.map_join(entries, "\n", &elem(&1, 1))
    states = PinSites.line_states(side)
    # Unless a step or job given whole as an alias runs the mapping it sits
    # in (`- *step`, the mapping anchored in a matrix entry).
    run = for %{key_line: n} <- PinSites.node_alias_sites(side), into: MapSet.new(), do: n
    data = MapSet.difference(PinSites.data_uses_lines(side), run)

    entries
    |> Enum.zip(states)
    |> Enum.with_index(1)
    |> Enum.map(fn {{{i, text}, {_, state}}, n} ->
      {i, text, if(n in data, do: :text, else: state)}
    end)
  end

  defp line_status(line, i, view, continued) do
    {before, [{_, text, state} | rest]} = Enum.split_while(view, fn {j, _, _} -> j != i end)
    indent = indent_of(text)

    certain? =
      state != :open and
        (before ++ rest)
        |> Enum.reject(fn {_, t, s} -> s in [:text, :value] or String.trim(t) == "" end)
        |> Enum.filter(fn {_, t, _} -> indent_of(t) <= indent end)
        |> Enum.all?(fn {_, t, _} -> yaml_like?(t) end)

    {line, state == :text, certain?, continued}
  end

  defp indent_of(text), do: String.length(text) - String.length(String.trim_leading(text, " "))

  # A line YAML structure could be: a key, a list item, a flow bracket, or
  # nothing but a comment. Shell, prose and heredoc markers are not.
  defp yaml_like?(text) do
    t = text |> PinSites.mask() |> String.trim()

    t == "" or Regex.match?(~r/^(?:-\s+)?["']?[\w.\/-][\w.\/ -]*["']?\s*:(?:\s|$)/, t) or
      Regex.match?(~r/^-(?:\s|$)/, t) or Regex.match?(~r/^[{}\[\],]/, t)
  end

  defp added_pins(file),
    do:
      Enum.flat_map(added_entries(file), fn {line, _, continued} ->
        parse_pins(line) ++ continued
      end)

  defp removed_pins(file),
    do:
      for(
        {line, false, _, continued} <- changed_lines(file),
        String.starts_with?(line, "-"),
        pin <- parse_pins(line) ++ continued,
        do: pin
      )

  # Added lines outside block and continued quoted scalars, each with its
  # certainty.
  defp added_entries(file),
    do:
      for(
        {line, false, certain?, continued} <- changed_lines(file),
        String.starts_with?(line, "+"),
        do: {line, certain?, continued}
      )

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

  # The body claim for `action`: one naming the action itself, or its root
  # repository (`github/codeql-action` for `github/codeql-action/init`), in
  # either case. A claim naming only a basename (`[setup](...)`) is this
  # action's only when no other action of the PR shares that basename
  # (`shared`). A claim for another repository that ends the same way
  # (`acme/setup` for `other/setup`) is never borrowed: the delta stays
  # unresolved rather than verified by someone else's version numbers.
  defp claim_for(claims, action, shared) do
    full = String.downcase(action)
    base = String.downcase(action_base(action))
    short = full |> String.split("/") |> List.last()

    Enum.find(claims, &(String.downcase(&1.action) == full)) ||
      Enum.find(claims, &(String.downcase(&1.action) == base)) ||
      if(short not in shared, do: Enum.find(claims, &(String.downcase(&1.action) == short)))
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

# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.PinIntegrity do
  @moduledoc """
  Pin-level integrity rules for workflow and lockfile content.

  Rule IDs PI001-PI005. Every function here is **pure**: it takes file
  content and the policy map, and returns findings or a rewritten string.
  No network, no repo handle, no clock. That is deliberate — this is the
  module that a token-bearing actuator re-runs to *independently re-prove*
  a decision the brain made, so it must be drivable from either side.

  ## Why these rules exist

  The estate lost a day to `github/codeql-action` **v4.38.1**
  (`1c5b675653bb5c22dbe9b12b556ec555138e09fd`). GitHub rejects that commit
  at workflow start-up — `startup_failure`, zero jobs, no logs — so CodeQL,
  Hypatia Security Scan and Scorecard died on every repo it reached.

  The estate rolled it back. Then it came back, twice, because the
  rollback **relabelled the pin without changing it**: files across the
  estate now carry

      uses: github/codeql-action/init@1c5b6756...  # v4.38.0

  which is the 4.38.1 commit wearing a 4.38.0 label. A check that greps
  for the version string sees a correct pin. A check that greps for the
  commit sees 108 files across 84 repositories.

  So: **pin identity is the SHA, never the comment.** PI002 exists
  specifically to catch the label/pin divergence, and it is the rule that
  would have stopped the second round.

  ## Rule catalogue

  | Rule | Severity | Fires when |
  |---|---|---|
  | PI001 | critical | a `uses:` ref resolves to a denylisted SHA or version |
  | PI002 | high | the inline comment names a version the ref does not resolve to |
  | PI003 | high | a moving ref (`stable`, `latest`, ...) carries a SHA pin |
  | PI004 | medium | a lockfile entry disagrees with the workflow it locks |
  | PI005 | info | a pin whose upstream ref has moved (stale-pin, advisory) |

  PI001 and PI002 are the pair that matters: PI001 alone is defeated by a
  mislabelled pin, and PI002 alone is defeated by a confidently wrong
  comment on a *good* pin. Their conjunction is the invariant.

  ## Dispatch

  Findings route to `Patch-Bridge` as `pin_rollback_denylisted`
  (auto-executable, because the repair is a literal token substitution)
  or to the owner as `flag` when the repair is not a substitution.
  """

  alias Hypatia.Rules.PinSites

  @doc """
  Parse every `uses:` pin site in a workflow file.

  A site is what `Hypatia.Rules.PinSites` reads as one — block form
  (`- uses: a/b@ref # v1`), KYAML form (`uses: "a/b@ref", # v1`), and every
  step of a compact flow sequence or mapping (`steps: [{ uses: a/b@x }]`,
  `- { name: x, uses: a/b@x }`) — so the file scan sees the pins the diff
  rules (PA001-PA004) see. A `uses:` inside a quoted `run:` string, a
  comment or script text is not one, nor is one on a line of a `run: |`
  block or of a quoted scalar continued from an earlier line: the file is
  read through `PinSites.mask_lines/1`, which carries that state from line
  to line. A line inside a flow collection opened on an earlier line is
  judged against the brackets still open (`PinSites.flow_prefixes/1`), so a
  step's `uses:` after a sibling (`name: "hello` then `world", uses: a/b@x
  }]`) is a site and one in an input's mapping is not. A `uses:` whose reference YAML continues below it (`- uses:`
  then `    a/b@x`, blank and comment-only lines allowed between) is a site
  on the value's line (`PinSites.continuations/1`), and a `uses: *name` is a
  site on the line of the anchor it names, where its reference stands
  (`PinSites.alias_sites/1`).

  Returns a list of maps with `:line`, `:action` (the bare action path as
  written, e.g. `github/codeql-action/init`), `:action_base` (the first
  two path segments, which is what an allow/deny list keys on), `:ref`,
  `:ref_at` (the ref's byte offset in its line) and `:comment` — the
  line's comment, trimmed, when the site is the line's only one, else `""`:
  `[ { uses: a/b@x }, { uses: c/d@y } ] # v4` labels neither pin.
  """
  @spec pin_sites(String.t()) :: [map()]
  def pin_sites(content) when is_binary(content) do
    masked = PinSites.mask_lines(content)
    prefixes = PinSites.flow_prefixes(content)
    numbered = masked |> Enum.zip(prefixes) |> Enum.with_index(1)

    # A `uses:` whose reference continues below it — past any blank or
    # comment-only lines — is a site on the value's line.
    continued =
      for {_key, number, site} <- PinSites.continuations(masked) do
        next = Enum.at(masked, number - 1)
        site_entry(site, number, next |> PinSites.comment() |> String.trim())
      end

    # A `uses: *name` runs the reference the anchor holds: its site is the
    # anchor's value, where a repair rewrites it (every alias with it).
    aliased =
      for %{anchor_line: number, site: site} <- PinSites.alias_sites(masked, prefixes) do
        site_entry(
          site,
          number,
          masked |> Enum.at(number - 1) |> PinSites.comment() |> String.trim()
        )
      end

    numbered
    |> Enum.flat_map(fn {{line, prefix}, number} -> line_sites(line, number, prefix) end)
    |> Kernel.++(continued)
    |> Kernel.++(aliased)
    |> Enum.uniq_by(&{&1.line, &1.ref_at})
    |> Enum.sort_by(&{&1.line, &1.ref_at})
  end

  def pin_sites(_), do: []

  defp line_sites(line, number, flow_prefix) do
    sites = PinSites.sites(line, flow_prefix)
    comment = if length(sites) == 1, do: line |> PinSites.comment() |> String.trim(), else: ""

    for site <- sites, do: site_entry(site, number, comment)
  end

  defp site_entry(site, number, comment) do
    %{
      line: number,
      action: site.action,
      action_base: action_base(site.action),
      ref: site.ref,
      ref_at: site.ref_at,
      comment: comment
    }
  end

  # GitHub resolves `GitHub/CodeQL-Action` and `github/codeql-action` to one
  # repository, so a table keyed on one spelling must match the other.
  defp same_action?(a, b) when is_binary(a) and is_binary(b),
    do: String.downcase(a) == String.downcase(b)

  defp same_action?(_, _), do: false

  @doc """
  The first two segments of an action path.

  `github/codeql-action/init` → `github/codeql-action`, because the
  denylist names the *release unit*, not the sub-action. Denying
  `init` but allowing `analyze` would leave half a CodeQL install
  poisoned.
  """
  @spec action_base(String.t()) :: String.t()
  def action_base(action) do
    case String.split(action, "/") do
      [owner, name | _rest] -> "#{owner}/#{name}"
      _ -> action
    end
  end

  @doc """
  Does this ref sit on the policy denylist?

  Accepts a SHA (`1c5b6756...`), a tag (`v4.38.1`), or a version
  (`4.38.1`) — the estate uses all three forms and the decision must not
  depend on which spelling a repo happens to carry.
  """
  @spec denylisted?(map(), String.t(), String.t()) :: nil | map()
  def denylisted?(policy, action, ref) do
    base = action_base(action)
    normalised = normalise_ref(ref)

    policy
    |> Map.get("pin_denylist", [])
    |> Enum.find(fn entry ->
      same_action?(Map.get(entry, "action"), base) and
        (normalised in normalise_all(Map.get(entry, "blocked_shas", [])) or
           normalised in normalise_all(Map.get(entry, "blocked_versions", [])) or
           normalised in normalise_all(Map.get(entry, "blocked_refs", [])))
    end)
  end

  # Both sides of the comparison get the same treatment: the policy writes
  # `v4.38.1` while a workflow may write `4.38.1`, and a denylist that only
  # matches one spelling is a denylist with a hole in it.
  defp normalise_all(list), do: Enum.map(list, &normalise_ref/1)

  defp normalise_ref(ref) do
    trimmed = ref |> String.trim() |> String.trim_leading("v")

    # `v4.38.1` and `4.38.1` are the same claim about the same artefact; a
    # commit ID in upper-case hex is the same object as the lower-case one
    # the table carries, so both sides compare in lower case.
    if Regex.match?(~r/^[0-9a-fA-F]{40}$/, trimmed), do: String.downcase(trimmed), else: trimmed
  end

  @doc """
  The known-good replacement for a denylisted pin, or `nil`.
  """
  @spec known_good(map(), String.t()) :: nil | map()
  def known_good(policy, action) do
    base = action_base(action)

    case Enum.find(
           Map.get(policy, "pin_denylist", []),
           &same_action?(Map.get(&1, "action"), base)
         ) do
      %{"known_good_sha" => sha} = entry when is_binary(sha) ->
        %{sha: sha, version: Map.get(entry, "known_good_version"), rule: Map.get(entry, "id")}

      _ ->
        nil
    end
  end

  # ─── PI001 · denylisted pin ────────────────────────────────────────────

  @doc """
  PI001: a `uses:` ref resolves to a denylisted SHA or version.

  Severity `:critical` for every entry carrying `"severity": "critical"`
  in the policy (currently PIN-001); `:high` otherwise.
  """
  @spec pi001_denylisted_pin(String.t(), map(), keyword()) :: [map()]
  def pi001_denylisted_pin(content, policy, opts \\ []) do
    path = Keyword.get(opts, :path, "<content>")
    repo = Keyword.get(opts, :repo, "<repo>")

    content
    |> pin_sites()
    |> Enum.flat_map(fn site ->
      case denylisted?(policy, site.action, site.ref) do
        nil ->
          []

        entry ->
          [
            finding(
              "PI001",
              severity_atom(entry),
              repo,
              path,
              site.line,
              """
              `#{site.action}@#{site.ref}` is on the pin denylist (#{entry["id"]}).

              #{entry["reason"]}

              Replacement: #{format_replacement(entry)}
              """,
              %{
                action: site.action,
                ref: site.ref,
                denylist_id: entry["id"],
                known_good_sha: entry["known_good_sha"],
                repair: "substitution"
              }
            )
          ]
      end
    end)
  end

  # ─── PI002 · mislabelled pin ───────────────────────────────────────────

  @doc """
  PI002: the inline comment names a version the ref does not resolve to.

  This is the rule the estate needed on 2026-09-23 and did not have.
  A file carrying

      uses: github/codeql-action/init@1c5b6756... # v4.38.0

  is *poisoned and labelled correct*. Only a rule that compares the
  comment against the resolved ref can see it.

  `resolution` is a map of ref → version, or ref → `%{version: v, tainted: bool}`.
  When the ref is absent from the map the rule cannot judge and stays
  silent — an unresolved claim is not a finding.
  """
  @spec pi002_mislabelled_pin(String.t(), map(), keyword()) :: [map()]
  def pi002_mislabelled_pin(content, resolution, opts \\ []) do
    path = Keyword.get(opts, :path, "<content>")
    repo = Keyword.get(opts, :repo, "<repo>")

    content
    |> pin_sites()
    |> Enum.flat_map(fn site ->
      with claimed when is_binary(claimed) <- claimed_version(site.comment),
           true <- claimed != "",
           resolved when is_binary(resolved) <- resolve_version(resolution, site.ref),
           true <- not version_prefix?(normalise_ref(claimed), normalise_ref(resolved)) do
        [
          finding(
            "PI002",
            :high,
            repo,
            path,
            site.line,
            """
            Pin comment claims `#{claimed}` but `#{site.ref}` resolves to `#{resolved}`.

            The comment is what a human reviewer reads and what a
            version-string grep matches, so a divergence here makes a
            bad pin look like a good one. This is the pattern that let
            the codeql-action 4.38.1 rollback fail silently across the
            estate.
            """,
            %{
              action: site.action,
              ref: site.ref,
              claimed: claimed,
              resolved: resolved,
              repair: "relabel_or_resubstitute"
            }
          )
        ]
      else
        _ -> []
      end
    end)
  end

  @doc """
  Pull a version out of a pin comment.

  Handles the estate's actual comment shapes:

      # v4.38.0
      # v4.38.0 (4.38.1 blocked estate-wide; nexia-list#100)
      # 4.38.0
      # v3
      # Pinned to v1.2.3 — do not move

  Returns the first matching version without its `v` prefix. A bare major
  such as `3` is ignored unless prefixed with `v`; dotted versions need no
  prefix. Returns `nil` when there is no match or the input is not a string.
  """
  @spec claimed_version(String.t()) :: nil | String.t()
  def claimed_version(comment) when is_binary(comment) do
    # `v3` is a real shape in the estate (dictask carries a poisoned pin
    # annotated `# v3`), so a bare major behind a `v` counts. A bare number
    # without the `v` does not: `# 2 jobs` is prose, not a version claim.
    # `capture: :all_but_first` still DROPS trailing unmatched groups, so a
    # `v`-led match yields one element and a bare `4.38.0` yields two.
    case Regex.run(~r/\b(?:v(\d+(?:\.\d+)*)|(\d+\.\d+(?:\.\d+)?))\b/, comment,
           capture: :all_but_first
         ) do
      [version] -> version
      ["", version] -> version
      _ -> nil
    end
  end

  def claimed_version(_), do: nil

  # A claim that names only the leading segments (`v3` against `3.1.0`) is
  # coarse, not wrong. Only a claim that contradicts a segment it does name
  # is a mislabel.
  defp version_prefix?(claimed, resolved) do
    claimed_parts = String.split(claimed, ".")
    resolved_parts = String.split(resolved, ".")

    length(claimed_parts) <= length(resolved_parts) and
      Enum.take(resolved_parts, length(claimed_parts)) == claimed_parts
  end

  defp resolve_version(resolution, ref) do
    case Map.get(resolution, ref) do
      %{version: v} -> v
      v when is_binary(v) -> v
      _ -> nil
    end
  end

  # ─── PI003 · locked moving ref ────────────────────────────────────────

  @doc """
  PI003: a moving ref carries a SHA pin.

  `dtolnay/rust-toolchain@stable` re-points on every Rust release. A
  lockfile that pins the SHA it happened to have kills every consumer at
  `Set up job` from the next release onward (hypatia CI-1: 4 workflows,
  13 jobs, all four open dependabot PRs).

  The cure is structural — do not lock moving refs — so the finding
  carries `repair: "unpin_moving_ref"`, never `"substitution"`.
  """
  @spec pi003_locked_moving_ref(String.t(), map(), keyword()) :: [map()]
  def pi003_locked_moving_ref(content, policy, opts \\ []) do
    path = Keyword.get(opts, :path, "<content>")
    repo = Keyword.get(opts, :repo, "<repo>")
    moving = Map.get(policy, "moving_refs_never_lockable", [])
    locked = locked_refs(content)

    for {ref, {action, line}} <- locked,
        ref in moving do
      finding(
        "PI003",
        :high,
        repo,
        path,
        line,
        """
        `#{action}@#{ref}` is a moving ref locked to a fixed SHA.

        Moving refs cannot be lockfile-pinned: the pin stops matching the
        ref at the next upstream release and every job using it dies before
        checkout. Remove the lockfile entry rather than re-pinning it.
        """,
        %{action: action, ref: ref, repair: "unpin_moving_ref"}
      )
    end
  end

  @doc """
  Extract `action@sha` pairs from a `gh actions-lock` file (TOML-ish).
  Used by PI003 and PI004.

  Reads the first single-quoted `action@ref` on each line and returns
  `[{ref, {action_base, line_number}}]` in file order, with one-based line
  numbers and action paths reduced to their first two segments. One entry
  per action and ref: two actions locked at the same ref
  (`actions/checkout@v4`, `github/codeql-action@v4`) are two entries, and
  the same pair met again (a workflow's edge and its dependency record)
  keeps the line of its last mention. No matches produce `[]`.
  """
  @spec locked_refs(String.t()) :: [{String.t(), {String.t(), integer()}}]
  def locked_refs(content) do
    content
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {line, number} ->
      case Regex.run(~r/'(?<action>[A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]+)@(?<ref>[^\s']+)'/, line) do
        nil ->
          []

        captures ->
          action = Enum.at(captures, 1)
          ref = Enum.at(captures, 2)
          [{ref, {action_base(action), number}}]
      end
    end)
    |> Enum.reverse()
    |> Enum.uniq_by(fn {ref, {action, _}} -> {String.downcase(action), ref_identity(ref)} end)
    |> Enum.reverse()
  end

  # What a ref names, for comparing two spellings of it: a commit ID is one
  # object in either case (`1C5B…` is `1c5b…`); a symbolic ref (`main`,
  # `v4`) is a name and compares exactly — `v4` and `4` are two refs.
  defp ref_identity(ref) do
    if Regex.match?(~r/^[0-9a-fA-F]{40}$/, ref), do: String.downcase(ref), else: ref
  end

  # ─── PI004 · lock/spec divergence ─────────────────────────────────────

  @doc """
  PI004: a workflow pins a SHA the lockfile disagrees with.

  hypatia on 2026-09-26: `actions.lock` pinned
  `github/codeql-action@b96794f0...` while `codeql.yml` and
  `security-policy.yml` carried `1c5b6756...`. The lock was right and the
  workflows were wrong, so any gate that verifies the lockfile rather
  than the workflows passes a poisoned repo. The direction of the
  divergence does not matter — agreement does.
  """
  @spec pi004_lock_divergence(String.t(), String.t(), keyword()) :: [map()]
  def pi004_lock_divergence(workflow_content, lock_content, opts \\ []) do
    path = Keyword.get(opts, :path, "<content>")
    repo = Keyword.get(opts, :repo, "<repo>")
    locked = locked_refs(lock_content)

    workflow_content
    |> pin_sites()
    |> Enum.flat_map(fn site ->
      locked_refs_for_action =
        locked
        |> Enum.filter(fn {_ref, {action, _line}} -> same_action?(action, site.action_base) end)
        |> Enum.map(fn {ref, _} -> ref end)
        |> Enum.uniq_by(&ref_identity/1)

      cond do
        locked_refs_for_action == [] ->
          []

        # The same commit in another case is agreement, not divergence.
        ref_identity(site.ref) in Enum.map(locked_refs_for_action, &ref_identity/1) ->
          []

        true ->
          [
            finding(
              "PI004",
              :medium,
              repo,
              path,
              site.line,
              """
              `#{site.action}` is pinned to `#{site.ref}` here, but the lockfile
              pins `#{Enum.join(locked_refs_for_action, ", ")}`.

              The lockfile is the thing a verify step reads, so a divergence
              makes a poisoned workflow look compliant.
              """,
              %{
                action: site.action,
                workflow_ref: site.ref,
                lock_refs: locked_refs_for_action,
                repair: "reconcile_lock_and_workflow"
              }
            )
          ]
      end
    end)
  end

  # ─── Mechanical repair ────────────────────────────────────────────────

  @doc """
  Rewrite every denylisted pin site in `content` to its known-good SHA.

  This is the whole "it is really just a file merge problem" case: the
  repair is a token substitution that either applies exactly or does not
  apply at all. Returns
  `{:ok, new_content, excisions}` | `{:error, reason}`.

  Refuses — rather than guesses — when:

    * a denylisted site has no `known_good_sha` in the policy;
    * the substituted content would still parse fewer pin sites than it
      started with (a rewrite that deletes a `uses:` line is not a
      substitution).

  The refusal path is the point. A repair that cannot be proved is a
  finding, not an action.
  """
  @spec substitute_denylisted_pins(String.t(), map(), keyword()) ::
          {:ok, String.t(), [map()]} | {:error, String.t()}
  def substitute_denylisted_pins(content, policy, _opts \\ []) do
    lines = String.split(content, "\n")
    sites = pin_sites(content)
    hits = Enum.filter(sites, &denylisted?(policy, &1.action, &1.ref))

    cond do
      hits == [] ->
        {:ok, content, []}

      Enum.any?(hits, &(known_good(policy, &1.action) == nil)) ->
        {:error, "no known_good_sha for a denylisted action in this file"}

      true ->
        {new_lines, excisions} =
          lines
          |> Enum.with_index(1)
          |> Enum.map_reduce([], fn {original, number}, log ->
            case Enum.filter(hits, &(&1.line == number)) do
              [] -> {original, log}
              line_hits -> rewrite_line(original, line_hits, sites, policy, log)
            end
          end)

        new_content = Enum.join(new_lines, "\n")

        if length(pin_sites(new_content)) == length(sites) do
          {:ok, new_content, excisions}
        else
          {:error, "substitution would remove a pin site — refusing"}
        end
    end
  end

  # Each denylisted ref is spliced at its own offset, right to left so the
  # offsets still to come stay valid: a line that carries the same ref twice,
  # or the ref's text again in a `name:`, changes only at its sites. The
  # comment is relabelled only on a line whose one site was the hit — on a
  # multi-site line it labels no pin in particular.
  defp rewrite_line(original, line_hits, sites, policy, log) do
    rewritten =
      line_hits
      |> Enum.sort_by(& &1.ref_at, :desc)
      |> Enum.reduce(original, fn site, acc ->
        %{sha: sha} = known_good(policy, site.action)
        tail_at = site.ref_at + byte_size(site.ref)

        binary_part(acc, 0, site.ref_at) <>
          sha <> binary_part(acc, tail_at, byte_size(acc) - tail_at)
      end)

    rewritten =
      case {line_hits, Enum.count(sites, &(&1.line == hd(line_hits).line))} do
        {[site], 1} -> relabel_line(rewritten, known_good(policy, site.action).version)
        _ -> rewritten
      end

    excisions =
      for site <- line_hits do
        %{sha: sha, version: version} = known_good(policy, site.action)

        %{
          line: site.line,
          action: site.action,
          from_ref: site.ref,
          to_ref: sha,
          to_version: version,
          comment_before: site.comment,
          comment_after: PinSites.comment(rewritten)
        }
      end

    {rewritten, log ++ excisions}
  end

  # The comment is the text after the `#` PinSites finds outside quoted
  # scalars, so `name: "C# build"` is not cut in two.
  defp relabel_line(line, version) when is_binary(version) do
    head = PinSites.head(line)

    # Mirror estate-pin-integrity.sh: the head keeps no `#`, the comment
    # is handed over WITH it, and relabel/2 returns it with it.
    if byte_size(head) < byte_size(line),
      do: head <> relabel("#" <> PinSites.comment(line), version),
      else: line
  end

  defp relabel_line(line, _version), do: line

  @doc """
  Relabel a pin's inline comment — but only when the comment **leads** with a
  version claim.

  `version` is the replacement without a `v` prefix. The leading claim must
  end at whitespace or the end of the comment. A rewritten comment has one
  leading `#`; existing whitespace and trailing prose are preserved. Bare
  claims with no leading whitespace gain a space after `#`. A `nil` version
  leaves the comment unchanged.

  With `version` set to `"4.38.0"`, these estate shapes are covered:

      "v3"                                           -> "# v4.38.0"
      "# v3"                                        -> "# v4.38.0"
      "# v4.38.0 (4.38.1 blocked estate-wide; …)"   -> unchanged

  and prose that merely mentions a version mid-sentence is returned
  byte-identical, because mangling a sentence to fix a label trades a silent
  wrong pin for a silent wrong sentence.

  Must stay behaviourally identical to `relabel_comment` in
  `scripts/sweeps/estate-pin-integrity.sh`. Two readers, one policy, one edit.
  """
  @spec relabel(String.t(), nil | String.t()) :: String.t()
  def relabel(comment, version) when is_binary(comment) and is_binary(version) do
    # pin_sites/1 hands over the comment with its `#` and spacing already
    # stripped (`v3`); restore the canonical `# ` so the result is a comment.
    comment =
      if comment == "" or String.starts_with?(comment, "#"), do: comment, else: "# " <> comment

    body = String.replace_prefix(comment, "#", "")
    hashed? = body != comment

    case Regex.run(~r/^\s*/, body) do
      [lead] ->
        trimmed = String.slice(body, String.length(lead)..-1//1)

        # pin_sites/1 hands over the comment without its `#`; a bare claim
        # comes back in the canonical `# vX` shape rather than as `#vX`.
        # Promoted only after slicing, so the claim's first byte survives.
        out_lead = if hashed? or lead != "", do: lead, else: " "

        case Regex.run(~r/^(v?\d+(?:\.\d+)*)(?:\s|$)/, trimmed) do
          [_whole, claim] ->
            "#" <> out_lead <> String.replace_prefix(trimmed, claim, "v" <> version)

          _ ->
            comment
        end

      _ ->
        comment
    end
  end

  def relabel(comment, _version), do: comment

  @doc """
  Prove the meta-guard exemption `MGX-001` (`pin_only_workflow_edit`).

  Given the removed and added lines of a diff, answer only one question:
  **is every changed line a `uses:` pin token substitution on a line that
  exists on both sides, with the action name unchanged?**

  Everything else — a new step, a changed `permissions:` block, a moved
  `if:`, an added `on:` trigger — makes this `false`, which forces the
  decision back to `change_level = meta` and `safety = flag`.

  The actuator calls this on the raw diff. It never accepts the brain's
  claim that the edit was pin-only; it re-derives it.
  """
  @spec pin_only_edit?([String.t()], [String.t()], map()) :: boolean()
  def pin_only_edit?(removed, added, policy) do
    removed_sites = Enum.flat_map(removed, &line_sites(&1, 0, ""))
    added_sites = Enum.flat_map(added, &line_sites(&1, 0, ""))

    pinned_removed = removed_sites |> Enum.map(& &1.ref) |> Enum.sort()
    pinned_added = added_sites |> Enum.map(& &1.ref) |> Enum.sort()

    cond do
      removed == [] or added == [] ->
        false

      not Enum.all?(removed ++ added, &PinSites.pin_line?/1) ->
        # At least one changed line is not a `uses:` line.
        false

      Enum.sort(Enum.map(removed_sites, &String.downcase(&1.action))) !=
          Enum.sort(Enum.map(added_sites, &String.downcase(&1.action))) ->
        # An action was swapped for a different action.
        false

      Enum.any?(added_sites, &denylisted?(policy, &1.action, &1.ref)) ->
        # The substitution moves *onto* the denylist.
        false

      pinned_removed == pinned_added ->
        # Nothing actually changed.
        false

      true ->
        true
    end
  end

  # ─── PI005 · stale pin (advisory) ─────────────────────────────────────

  @doc """
  PI005: a pin whose upstream ref has moved on, reported as information.

  Deliberately `:info` and deliberately last. A stale pin is not a
  defect; it becomes one only when it is behind a security fix. The
  estate's problem in September 2026 was never that pins were *old* — it
  was that a fresh pin was *poisoned* and nobody could tell the two apart
  because both arrived as `chore(deps): bump …`.
  """
  @spec pi005_stale_pin(String.t(), map(), keyword()) :: [map()]
  def pi005_stale_pin(content, latest_versions, opts \\ []) do
    path = Keyword.get(opts, :path, "<content>")
    repo = Keyword.get(opts, :repo, "<repo>")

    content
    |> pin_sites()
    |> Enum.flat_map(fn site ->
      case Map.get(latest_versions, site.action_base) do
        latest when is_binary(latest) ->
          if normalise_ref(latest) != normalise_ref(site.ref) and
               claimed_version(site.comment) not in [nil, latest] do
            [
              finding(
                "PI005",
                :info,
                repo,
                path,
                site.line,
                "`#{site.action}` pins #{site.ref}; upstream is at #{latest}.",
                %{action: site.action, ref: site.ref, latest: latest, repair: "advisory"}
              )
            ]
          else
            []
          end

        _ ->
          []
      end
    end)
  end

  # ─── Shared finding shape ─────────────────────────────────────────────

  defp finding(rule_id, severity, repo, path, line, description, detail) do
    %{
      rule_id: rule_id,
      severity: severity,
      repo: repo,
      path: path,
      line: line,
      description: String.trim(description),
      detail: detail
    }
  end

  defp severity_atom(%{"severity" => "critical"}), do: :critical
  defp severity_atom(%{"severity" => "high"}), do: :high
  defp severity_atom(%{"severity" => "medium"}), do: :medium
  defp severity_atom(_), do: :high

  defp format_replacement(%{"known_good_sha" => sha, "known_good_version" => version})
       when is_binary(sha) do
    suffix =
      case version do
        v when is_binary(v) -> " (v" <> v <> ")"
        _ -> ""
      end

    "pin to `" <> sha <> "`" <> suffix
  end

  defp format_replacement(%{"replacement" => replacement}), do: replacement
  defp format_replacement(_), do: "manual review"
end

# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ResearchExtensions do
  @moduledoc """
  Static detection of GitHub Actions workflow defects sourced from
  commercial CI/CD security tools (Snyk, StepSecurity, Endor Labs,
  Datadog CI Visibility) and recent academic literature on CI/CD
  attack surface.

  This module is complementary to `WorkflowHardening` (which draws on
  the open-source `actionlint`/`zizmor` audit cluster) and
  `BaselineHealth` (which detects baseline-drift conditions). The
  rules here represent the *extension* of the open-source set with
  the patterns surfaced by commercial / academic research that aren't
  yet covered by the OSS auditors.

  Rule IDs RE001-RE010.

  ## Provenance map

  Each rule cites its upstream source (tool name and/or paper).

  - **RE001** — A job holds a real credential (any `secrets.*` other
    than the ambient `GITHUB_TOKEN`) but neither installs
    `step-security/harden-runner` nor declares its egress in the repo
    deed (standards ADR-009). It is `:high` when the credential shares
    a step with code the job cannot vouch for (an untrusted-input
    trigger, a PR-head checkout, `curl | sh`, or a dependency
    install/build). StepSecurity's Harden-Runner provides outbound-egress
    monitoring/blocking; the declaration records the boundary for it.
    From StepSecurity Harden-Runner deployment guide.
  - **RE002** — Harden-Runner is installed but configured with
    `egress-policy: audit` (telemetry-only, never blocks). On a
    protected-branch workflow this provides no actual containment.
    From StepSecurity production-deployment guidance.
  - **RE003** — `actions/cache` key interpolates `github.head_ref`
    (or PR title/body). Forked PRs can poison the cache by picking a
    head ref that overlaps a privileged restore key. Snyk's dataflow
    auditor catches this; same defect class as the 2022 GitHub cache-
    poisoning advisory.
  - **RE004** — A container `uses: docker://image:tag` reference (or
    `container: image:tag`) pinned by tag, not by digest. Tag mutation
    re-points the image at attacker-controlled bytes. Same defect as
    Snyk container scanning's tag-pin warning.
  - **RE005** — A test step swallows non-zero exit (`|| true`, or
    `continue-on-error: true` on a step whose name contains
    `test`/`spec`/`check`). Datadog CI Visibility catalogues this as
    the canonical flake-masking pattern: the suite goes green while
    real failures pile up.
  - **RE006** — A composite action (`action.yml` with
    `runs.using: composite`) calls a third-party `uses:` reference
    that is not SHA-pinned. Endor Labs' R-END-05 surfaces this as a
    *nested* pinning gap — the consuming workflow may pin the
    composite, but the composite then loads tags transitively.
  - **RE007** — Workflow-level `env:` block contains a
    `${{ secrets.* }}` reference. Endor Labs' R-END-01 (least-
    privilege secrets scope): a workflow-scoped env exposes the
    secret to every step, including untrusted third-party actions
    that shouldn't see it. Per-step or per-job `env:` is the fix.
  - **RE008** — `github.actor == 'dependabot[bot]'` (or any other
    spoofable bot identity) used as a trust gate. `github.actor` is
    *the user who triggered the run* — on `pull_request_target` from
    a fork, the attacker controls the value. From zizmor's
    `bot-conditions` audit and Koishybayev et al. (USENIX Security
    2022). The correct gate is `github.actor_id == <numeric_id>` or
    `github.event.pull_request.user.login`.
  - **RE009** — `${{ fromJSON(secrets.X) }}` — parsing a secret as
    structured JSON. Each parsed field becomes a separate runner-
    side value, and GitHub's runner-side redaction operates on the
    *literal* secret text, not its parsed fields. The parsed fields
    then leak into logs unredacted. From zizmor's
    `unredacted-secrets`.
  - **RE010** — `workflow_run` trigger that consumes artifacts from
    the upstream workflow without provenance verification. The
    cross-workflow trust crossing is the EuroS&P 2023
    (Kermabon-Bobinnec et al.) attack surface — a successful PR
    workflow can publish a malicious artifact that the privileged
    `workflow_run` then trusts. Mitigation: verify artifact provenance
    or `actions/download-artifact@v4`'s `run-id` constraint.

  ## Architecture

  All rules are pure-local file scans (no GitHub API). Each takes a
  `repo_path` and returns a list of finding maps in the standard
  shape:

      %{rule: "RENNN", file: "...", severity: :critical | :high | :warn | :info,
        reason: "...", action: :report, detail: %{fix: "...", ...}}

  Workflow file discovery uses the same scope as `WorkflowHardening`
  and `BaselineHealth` BH004: only `<repo>/.github/workflows/*.yml`
  / `*.yaml` at the repo root. RE006 additionally inspects any local
  composite `action.yml` files found under the repo (those *can*
  live anywhere — that's how composite actions are addressed via
  `uses: ./path/to/action`).

  ## Implementation note

  Regex on YAML text rather than parsed YAML, for the same reasons
  given in `Hypatia.Rules.WorkflowHardening`: corpus is small,
  patterns are syntactically distinctive, false positives are caught
  downstream by safety-triangle review routing.
  """

  require Logger

  # ─── Workflow-file discovery ────────────────────────────────────────

  @doc false
  def workflow_files(repo_path) do
    root = Path.join([repo_path, ".github", "workflows"])

    cond do
      not File.dir?(root) ->
        []

      true ->
        root
        |> File.ls!()
        |> Enum.filter(&(String.ends_with?(&1, ".yml") or String.ends_with?(&1, ".yaml")))
        |> Enum.map(&Path.join(root, &1))
        |> Enum.filter(&File.regular?/1)
    end
  end

  @doc false
  def composite_action_files(repo_path) do
    # Composite action files live at `action.yml` or `action.yaml`
    # anywhere in the repo. Walk via `find` with depth cap so a
    # monorepo doesn't pay for unbounded traversal.
    case System.cmd(
           "find",
           [
             repo_path,
             "-maxdepth",
             "6",
             "-type",
             "f",
             "(",
             "-name",
             "action.yml",
             "-o",
             "-name",
             "action.yaml",
             ")"
           ],
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        output
        |> String.split("\n", trim: true)
        |> Enum.reject(&String.contains?(&1, "/node_modules/"))
        |> Enum.reject(&String.contains?(&1, "/.git/"))
        |> Enum.filter(&File.regular?/1)

      _ ->
        []
    end
  end

  # ─── RE001: secret-bearing job neither hardened nor declared ─────────

  # Events whose payload carries text or code authored by people without
  # write access to the repository.
  @re001_untrusted_events ~w(pull_request_target issue_comment issues discussion
                             discussion_comment pull_request_review
                             pull_request_review_comment workflow_run)

  # The secret every job already holds as `github.token`. Naming it adds no
  # credential the job did not have, so it is not an exposure on its own.
  @re001_ambient_secrets ["GITHUB_TOKEN"]

  # The head of a pull request or of the run that triggered this one.
  # (`workflow_run` names its head fields `head_sha`, `head_branch`,
  # `head_repository`; a pull request nests them under `head.`.)
  @re001_head_ref ~r/github\.event\.pull_request\.head\.|github\.event\.workflow_run\.head(?:\.|_(?:sha|branch|repository)\b)|github\.head_ref/

  # Where naming that head means fetching its code: a checkout's `ref:` or
  # `repository:`, or a git checkout/fetch/switch in a `run:` script. A head
  # SHA in `env:`, an `if:` or a `concurrency:` group only names it.
  # Block (`ref: …`) or flow (`with: {ref: …}`) form, the key plain or quoted.
  @re001_checkout_key ~r/(?:^|[\s{,])["']?(?:ref|repository)["']?\s*:/
  @re001_checkout_action ~r/^\s*(?:-\s+)?["']?uses["']?\s*:\s*["']?actions\/checkout@/
  # One git command naming the head: no `;`, `&` or `|` between them. `\s`
  # spans the line break of a folded `run: >-`.
  @re001_git_head_fetch ~r/\bgit\s+(?:checkout|fetch|switch)\b[^;&|]*?(?:github\.event\.pull_request\.head\.|github\.event\.workflow_run\.head(?:\.|_(?:sha|branch|repository)\b)|github\.head_ref)/

  # Code fetched at run time: piped installers, package installs, and builds
  # or test runs that execute dependencies' build scripts.
  @re001_fetched_code ~r/(?:curl|wget)\b[^|;&]*\|\s*(?:sudo\s+)?(?:ba|z)?sh\b|\b(?:npm|pnpm|yarn)\s+(?:i|install|ci)\b|\bnpx\s|\bpip3?\s+install\b|\b(?:pipx|uvx)\s|\bcargo\s+(?:install|build|test|run)\b|\bgo\s+(?:install|build|test|run)\b|\bgem\s+install\b|\bbundle\s+install\b|\bmix\s+(?:deps\.get|compile|test)\b|\bdeno\s+(?:run|install)\b|\bbun\s+(?:i|install|run)\b|\bopam\s+install\b|\bcabal\s+(?:install|build|test)\b|\bstack\s+(?:build|test)\b/

  @doc """
  RE001: every locally executed workflow job that reads a real secret either
  installs `step-security/harden-runner@...` or declares its egress in the
  repo deed (standards ADR-009). Each other such job is reported, more
  severely when the credential sits next to code the job cannot vouch for.

  A job is in scope when it references a secret other than `GITHUB_TOKEN`
  (the ambient `github.token` every job holds anyway). It is satisfied by:

    * a harden-runner step in the job, or
    * an entry in the `(egress …)` clause of the repo deed
      (`<repo>_chora.deed`, profile
      `metadatastician/standards/1-formats/deed/spec/abnf/clauses/egress.abnf`)
      whose `(:workflow, :job)` names the job and whose `:secrets` equal the
      secrets it references.

  An undeclared job is reported. Its **exposures** are the evidence:

    * the workflow runs on an untrusted-input event (`pull_request_target`,
      `issue_comment`, `issues`, `workflow_run`, …), narrowed by a job `if:`
      that admits only listed events;
    * the job checks out or fetches the head of a pull request or triggering
      run (a checkout's `ref:`/`repository:`, or a git checkout/fetch/switch
      naming it; merely naming the head SHA in `env:` or `if:` is not one);
    * a step pipes a download into a shell or installs or builds dependencies
      (`npm ci`, `cargo build`, `mix deps.get`, …).

  Severity is `:high` when the credential and such code share one step (the
  code has the credential in its own environment), otherwise `:warn`, with
  or without exposures: ADR-009 asks every secret-bearing job to declare.

  Secrets are read the way GitHub reads them: across each `${{ … }}` in the
  job, even one a folded scalar splits over lines; `secrets.NAME` and
  `secrets['NAME']`, the context matched without regard to case; string
  literals skipped; only the top-level context counted. A read by a key
  computed at run time (`secrets[env.X]`, `toJSON(secrets)`, `secrets.*`)
  cannot be listed, so no declaration covers that job.

  The deed is trusted only as far as it is still true:

    * a listed job whose `:secrets` differ from its references (drift);
    * a listed job that no longer exists, runs elsewhere, or reads no real
      secret (stale), reported against the deed.

  An unreadable deed or clause declares nothing, and the finding says why.
  `:endpoints` are not compared with the workflow: that needs run-time
  observation or a model of what each action reaches.

  Full-line comments and reusable-workflow jobs without a direct `runs-on`
  field are ignored. Keys are matched plain or quoted, as YAML reads them, and
  YAML aliases are expanded before jobs are read (`expand_aliases/1`).
  Provenance: StepSecurity Harden-Runner deployment guide.

  ## Why this is structural, not a regex

  The check is per job, and both halves of it are indentation-sensitive:

    * **`runs-on` must belong to this job directly.** A reusable job
      (`uses:` at job level) delegates its runtime to the source workflow, so a
      sibling job's `runs-on` must not satisfy it. `direct_job_field?/2`
      therefore compares against the job's own field indent rather than
      matching `runs-on:` anywhere in the block.

    * **The hardener must be a real step.** `harden_runner_step?/1` walks
      `steps:` by indent and accepts the reference only as a step entry or a
      direct property of one. Two cases make a line-oriented match wrong in
      *opposite* directions, so both are handled explicitly:

      - `uses: "step-security/harden-runner@..."` — quoted scalars are valid
        YAML and still install the hardener. Missing them raises a **false
        positive** against an already-hardened job.
      - the same text inside a block scalar (`run: |`, `env: >`) is shell or
        data, not a step. `block_scalar?/1` opens a skip region until the
        indent closes, because treating it as a step raises a **false
        negative** that suppresses a genuine finding.

  Severity: `:high` or `:warn`, as above. Action: `:report`.
  """
  def re001_missing_harden_runner(repo_path) do
    {deed_file, declared, deed_note} =
      case Hypatia.Deed.egress_declarations(repo_path) do
        {:ok, file, jobs} -> {file, Map.new(jobs, &{{&1.workflow, &1.job}, &1}), nil}
        :none -> {nil, %{}, nil}
        {:error, file, reason} -> {file, %{}, "the repo deed declares nothing (#{reason})"}
      end

    jobs =
      repo_path
      |> workflow_files()
      |> Enum.flat_map(fn path -> re001_jobs(path, Path.relative_to(path, repo_path)) end)

    job_findings =
      Enum.flat_map(jobs, fn job ->
        case Map.fetch(declared, {job.workflow, job.id}) do
          :error ->
            cond do
              job.hardened? -> []
              # A key computed at run time cannot be listed, so no declaration covers it.
              job.dynamic_line -> [re001_dynamic(job, deed_file, false)]
              true -> [re001_undeclared(job, deed_file, deed_note)]
            end

          # A declaration is an attestation, checked even when harden-runner
          # already satisfies the rule: what it lists, and the enforcement it
          # claims, must still be true.
          {:ok, decl} ->
            # A fixed list cannot describe a key computed at run time, hardened or not.
            dynamic =
              if job.dynamic_line,
                do: [re001_dynamic(job, deed_file, true)],
                else: []

            drift =
              if job.dynamic_line == nil and decl.secrets != job.secrets,
                do: [re001_drift(job, deed_file, decl.secrets)],
                else: []

            enforcement =
              cond do
                decl.enforcement != "none" and not job.hardened? ->
                  [re001_enforcement(job, deed_file, decl.enforcement)]

                # Claiming block while the hardener only audits is a false attestation too.
                decl.enforcement == "harden-runner-block" and job.harden_policy != "block" ->
                  [re001_enforcement(job, deed_file, decl.enforcement)]

                true ->
                  []
              end

            dynamic ++ drift ++ enforcement
        end
      end)

    # Every declaration must still describe a local job that reads a real secret.
    live = MapSet.new(jobs, &{&1.workflow, &1.id})

    stale_findings =
      declared
      |> Map.keys()
      |> Enum.reject(&MapSet.member?(live, &1))
      |> Enum.sort()
      |> Enum.map(&re001_stale(&1, deed_file))

    job_findings ++ stale_findings
  end

  # The locally executed jobs of one workflow that read a real secret, with
  # what each holds and what it runs next to the credential.
  defp re001_jobs(path, rel) do
    # Comments are not runner configuration. Preserve physical line numbers
    # so existing findings are not reported against a newly added line-1 header.
    active_lines =
      path
      |> read_workflow()
      |> String.split("\n")
      |> Enum.with_index(1)
      |> Enum.reject(fn {line, _} -> String.starts_with?(String.trim_leading(line), "#") end)
      |> expand_aliases()

    untrusted_events =
      active_lines
      |> workflow_events()
      |> Enum.filter(&(&1 in @re001_untrusted_events))

    # Secrets in the workflow's top-level `env:` are inherited by every job,
    # so each job reads them too, and a declaration must list them.
    workflow_env = workflow_env_lines(active_lines)
    workflow_refs = secret_refs(workflow_env)

    active_lines
    |> workflow_job_lines()
    # Reusable jobs delegate their runtime to the source workflow. A sibling's
    # runner or hardener cannot establish this job's execution policy.
    |> Enum.filter(&direct_job_field?(&1, "runs-on"))
    |> Enum.flat_map(fn [{header, _} | _] = job_lines ->
      refs =
        for {name, n} <- secret_refs(job_lines) ++ workflow_refs,
            name not in @re001_ambient_secrets,
            do: {name, n}

      case refs do
        [] ->
          []

        [{_, first_line} | _] ->
          # A line that reads a real secret, by name or by a computed key.
          credential_lines = MapSet.new(refs, &elem(&1, 1))
          credential? = fn {_, n} -> MapSet.member?(credential_lines, n) end
          steps = job_steps(job_lines)
          # A credential in the job's or the workflow's `env:` is in every step's
          # environment.
          job_env_credentials = Enum.filter(job_env_lines(job_lines) ++ workflow_env, credential?)

          job_untrusted_events =
            case job_event_gate(job_lines) do
              nil -> untrusted_events
              gated -> Enum.filter(untrusted_events, &(&1 in gated))
            end

          # A step that both holds the credential and runs fetched or head code
          # hands the credential straight to that code.
          shared_line =
            Enum.find_value(steps, fn step ->
              credential =
                Enum.find(step, credential?) ||
                  Enum.find(job_env_credentials, &(not overrides_env?(step, &1)))

              credential && step_code_exposures(step) != [] && elem(credential, 1)
            end)

          [
            %{
              workflow: rel,
              id: job_id(header),
              line: shared_line || first_line,
              secrets: MapSet.new(for {name, _} <- refs, is_binary(name), do: name),
              dynamic_line: Enum.find_value(refs, fn {name, n} -> name == :dynamic && n end),
              hardened?: harden_runner_step?(job_lines),
              harden_policy: harden_runner_policy(steps),
              exposures: re001_exposures(steps, job_untrusted_events),
              shared?: shared_line != nil
            }
          ]
      end
    end)
  end

  # YAML anchors and aliases, which GitHub expands in workflows: `*name`
  # stands for the node anchored `&name`, so `env: *common`, `run: *script`,
  # `<<: *defaults` or `deploy: *build` brings the anchored node into the job
  # that holds the alias. Each alias is expanded in place before any job is
  # read, and every line it brings carries the alias's line number, so a
  # finding points at the alias:
  #
  #   * an alias that is a key's whole value, or a list item, is replaced by
  #     what follows its anchor on the anchor's line (a scalar, a block
  #     scalar's `|`, a flow collection), and the anchor's indented lines
  #     follow at the column of the `*`;
  #   * a `<<:` merge puts the anchored mapping's entries into the mapping
  #     that holds the key, at the key's column, minus every entry whose key
  #     that mapping sets itself: YAML gives the explicit key precedence;
  #   * an alias inside a flow collection is followed by a copy of the
  #     anchored lines, deeper than the line.
  #
  # Anchors and aliases are recognised only where a node starts, outside
  # quoted scalars and block-scalar text, so `echo "x *common"` and a `run: |`
  # line `ls *common` stay text. The anchor is the nearest one of that name
  # above the alias.
  @anchor ~r/(?:^\s*(?:-\s+)*|:\s+|[\[{,]\s*)&([A-Za-z0-9_-]+)(?=\s|$)/
  @alias ~r/(?:^\s*(?:-\s+)*|:\s+|[\[{,]\s*)\*([A-Za-z0-9_-]+)\s*(?=$|[,\]}])/
  @merge_key ~r/^(\s*(?:-\s+)?)(?:<<|"<<"|'<<')\s*:/
  @mapping_key ~r/^\s*(?:-\s+)?(?:"([^"]*)"|'([^']*)'|([^\s"'#:][^:]*?))\s*:(?:\s|$)/
  @block_scalar_start ~r/(?:^\s*|[:-]\s+)(?:&[A-Za-z0-9_-]+\s+)?[>|][0-9+-]*\s*$/
  @block_scalar_header ~r/^[>|][0-9+-]*$/
  # Nested anchors can multiply copies exponentially (the "billion laughs"
  # file), so expansion stops after this many copied lines per workflow.
  @alias_budget 5_000
  defp expand_aliases(lines) do
    codes = yaml_codes(lines)

    anchors =
      for {{{line, n}, code}, index} <- lines |> Enum.zip(codes) |> Enum.with_index(),
          code != nil,
          [_, {at, len}] <- Regex.scan(@anchor, code, return: :index),
          node_start?(code, at) do
        {binary_part(code, at, len), n,
         anchored(lines, index, strip_yaml_comment(line), at + len)}
      end

    # Only what jobs read is expanded in place: the jobs and the top-level
    # env:. An anchor kept elsewhere is expanded where a job uses it.
    {expanded, _} =
      lines
      |> Enum.zip(codes)
      |> Enum.with_index()
      |> Enum.flat_map_reduce({nil, @alias_budget}, fn {{{line, _} = entry, code}, index},
                                                       {section, budget} ->
        section =
          case Regex.run(~r/^["']?([^\s"':]+)["']?\s*:/, line) do
            [_, key] -> key
            nil -> section
          end

        if section in ["jobs", "env"] do
          {out, budget} = expand_line(entry, code, lines, index, anchors, budget)
          {out, {section, budget}}
        else
          {[entry], {section, budget}}
        end
      end)

    expanded
  end

  defp expand_aliases(lines, anchors, budget) do
    lines
    |> Enum.zip(yaml_codes(lines))
    |> Enum.with_index()
    |> Enum.flat_map_reduce(budget, fn {{entry, code}, index}, budget ->
      expand_line(entry, code, lines, index, anchors, budget)
    end)
  end

  # One line with its aliases expanded. A line of block-scalar text (code nil)
  # holds no alias.
  defp expand_line(entry, nil, _lines, _index, _anchors, budget), do: {[entry], budget}

  defp expand_line(entry, _code, _lines, _index, _anchors, budget) when budget <= 0,
    do: {[entry], budget}

  defp expand_line({line, n} = entry, code, lines, index, anchors, budget) do
    matches =
      for [_, {at, len}] <- Regex.scan(@alias, code, return: :index),
          node_start?(code, at),
          name = binary_part(code, at, len),
          {_, _, node} <-
            anchors |> Enum.filter(fn {a, m, _} -> a == name and m < n end) |> Enum.take(-1),
          do: {at, len, node}

    # The alias that ends the line is the whole value of its key or item.
    {value, in_flow} =
      case List.last(matches) do
        {at, len, _} = last ->
          if String.trim(binary_part(code, at + len, byte_size(code) - at - len)) == "",
            do: {last, Enum.drop(matches, -1)},
            else: {nil, matches}

        nil ->
          {nil, []}
      end

    {flow_copies, budget} =
      Enum.flat_map_reduce(in_flow, budget, fn {at, _, {inline, block}}, budget ->
        indent = indent_of(code)
        inline_lines = if inline == "", do: [], else: [{indent_spaces(indent + 2) <> inline, n}]
        {copy, budget} = anchored_block(inline, inline_lines ++ block, anchors, budget)
        {renumber(reindent(copy, column(code, at - 1)), n), budget}
      end)

    case value do
      nil ->
        {[entry | flow_copies], budget}

      {at, _, {inline, block}} ->
        text = strip_yaml_comment(line)
        head = String.trim_trailing(binary_part(text, 0, at - 1) <> inline)
        {block, budget} = anchored_block(inline, block, anchors, budget - 1)

        block =
          case Regex.run(@merge_key, code) do
            [_, prefix] ->
              merge_column = String.length(prefix)

              block
              |> reindent(merge_column)
              |> drop_entries(merge_column, sibling_keys(lines, index, merge_column))

            nil ->
              reindent(block, column(code, at - 1))
          end

        # The inline part may hold aliases of its own (`&a [*b, *c]`).
        {head_lines, budget} =
          if inline == "" or Regex.match?(@block_scalar_header, inline),
            do: {[{head, n}], budget},
            else: expand_line({head, n}, yaml_code(head), [{head, n}], 0, anchors, budget)

        {head_lines ++ renumber(block, n) ++ flow_copies, budget}
    end
  end

  # An anchor's indented lines, their own aliases expanded. Under a block
  # scalar's header they are text and are copied as they are.
  defp anchored_block(inline, block, anchors, budget) do
    if Regex.match?(@block_scalar_header, inline),
      do: {block, budget - length(block)},
      else: expand_aliases(block, anchors, budget - length(block))
  end

  # The node an anchor names: what follows the anchor on its own line, and
  # every line indented deeper than that line.
  defp anchored(lines, index, text, after_name) do
    inline = text |> binary_part(after_name, byte_size(text) - after_name) |> String.trim()
    indent = indent_of(text)

    block =
      lines
      |> Enum.drop(index + 1)
      |> Enum.take_while(fn {line, _} -> String.trim(line) == "" or indent_of(line) > indent end)

    {inline, block}
  end

  # The keys the mapping holding line `index` sets at `column` itself.
  defp sibling_keys(lines, index, column) do
    {before, [_ | after_line]} = Enum.split(lines, index)

    in_mapping = fn ls ->
      Enum.take_while(ls, fn {line, _} -> String.trim(line) == "" or indent_of(line) >= column end)
    end

    (in_mapping.(Enum.reverse(before)) ++ in_mapping.(after_line))
    |> Enum.filter(fn {line, _} -> indent_of(line) == column end)
    |> Enum.flat_map(fn {line, _} -> mapping_key(line) end)
    |> Enum.reject(&(&1 == "<<"))
  end

  # Merged entries, less those whose key the destination mapping sets.
  defp drop_entries(lines, column, keys) do
    lines
    |> Enum.chunk_while(
      [],
      fn {line, _} = entry, acc ->
        if indent_of(line) == column and String.trim(line) != "" and acc != [],
          do: {:cont, Enum.reverse(acc), [entry]},
          else: {:cont, [entry | acc]}
      end,
      fn
        [] -> {:cont, []}
        acc -> {:cont, Enum.reverse(acc), []}
      end
    )
    |> Enum.reject(fn [{first, _} | _] -> Enum.any?(mapping_key(first), &(&1 in keys)) end)
    |> Enum.concat()
  end

  defp mapping_key(line) do
    case Regex.run(@mapping_key, strip_yaml_comment(line)) do
      nil -> []
      captures -> captures |> tl() |> Enum.reject(&(&1 == "")) |> Enum.take(1)
    end
  end

  # Each line's YAML structure, for finding anchors and aliases: the line
  # without its comment and with quoted scalars blanked, byte offsets kept;
  # nil for a line of block-scalar text.
  defp yaml_codes(lines) do
    {codes, _} =
      Enum.map_reduce(lines, nil, fn {line, _}, scalar_indent ->
        if scalar_indent && (String.trim(line) == "" or indent_of(line) > scalar_indent) do
          {nil, scalar_indent}
        else
          code = yaml_code(line)
          {code, if(Regex.match?(@block_scalar_start, code), do: indent_of(line))}
        end
      end)

    codes
  end

  defp yaml_code(line), do: strip_yaml_comment(line, true)

  # A `,` separates nodes only inside a flow collection; in a plain scalar
  # (`run: echo a, *b`) it is text. The name starts at `at`, after `*` or `&`.
  defp node_start?(code, at) do
    before = code |> binary_part(0, at - 1) |> String.trim_trailing()
    not String.ends_with?(before, ",") or nesting_at(code, at) > 0
  end

  defp column(text, byte_offset), do: String.length(binary_part(text, 0, byte_offset))
  defp indent_spaces(n), do: String.duplicate(" ", n)
  defp renumber(lines, n), do: Enum.map(lines, fn {line, _} -> {line, n} end)

  defp reindent(lines, target) do
    base =
      lines
      |> Enum.map(&elem(&1, 0))
      |> Enum.reject(&(String.trim(&1) == ""))
      |> Enum.map(&indent_of/1)
      |> Enum.min(fn -> 0 end)

    Enum.map(lines, fn {line, n} ->
      if String.trim(line) == "",
        do: {"", n},
        else: {indent_spaces(indent_of(line) - base + target) <> String.trim_leading(line), n}
    end)
  end

  # Every secret a job reads inside ${{ … }}, as {NAME, line}: the property
  # form secrets.NAME and the index form secrets['NAME'] / secrets["NAME"],
  # anywhere in the expression (${{ vars.X || secrets.Y }} reads Y). GitHub
  # matches secret names without regard to case and stores them uppercase; the
  # deed declares that stored form, so names are uppercased here.
  #
  # The job is scanned as one text, not line by line: a folded YAML scalar can
  # split one expression across physical lines, and GitHub evaluates it whole.
  # A valid workflow has no unclosed ${{, so every match is a real expression.
  #
  # Both scans step over string literals ('…', with '' for a quote) whole, so
  # text inside one is never read: an expression ends at the first }} outside
  # a literal (format('{{x}}', …) escapes braces that way), and
  # ${{ 'secrets.X' }} reads nothing. Only the top-level secrets context
  # counts, not a property named secrets (github.event.x.secrets.Y), and its
  # name matches without regard to case, as GitHub's does (SECRETS.X reads X).
  #
  # Any other use of the context reads secrets whose names are not in the text
  # (secrets[env.NAME], toJSON(secrets), secrets.*): it is reported as
  # {:dynamic, line}, which no declaration can cover.
  @expression ~r/\$\{\{((?:'(?:[^']|'')*'|[^'}]|\}(?!\}))*)\}\}/
  @secret_ref ~r/(?<![\w.-])(?i:secrets)(?:\.([A-Za-z_][A-Za-z0-9_]*)\b(?![.\[-])|\[\s*(?:'([A-Za-z_][A-Za-z0-9_]*)'|"([A-Za-z_][A-Za-z0-9_]*)")\s*\]|(?![\w-]))|'(?:[^']|'')*'|"[^"]*"/
  defp secret_refs(job_lines) do
    text = Enum.map_join(job_lines, "\n", &elem(&1, 0))

    # {byte offset where each kept line starts, its physical line number}
    {starts, _} =
      Enum.map_reduce(job_lines, 0, fn {line, n}, at -> {{at, n}, at + byte_size(line) + 1} end)

    line_of = fn at ->
      {_, n} = starts |> Enum.take_while(fn {start, _} -> start <= at end) |> List.last()
      n
    end

    for [_, {body_at, body_len}] <- Regex.scan(@expression, text, return: :index),
        [{m_at, _} | groups] <-
          Regex.scan(@secret_ref, binary_part(text, body_at, body_len), return: :index),
        # A match that opens with a quote is a string literal, read past.
        binary_part(text, body_at + m_at, 1) not in ["'", "\""] do
      case Enum.find(groups, fn {at, _} -> at >= 0 end) do
        {at, len} -> {String.upcase(binary_part(text, body_at + at, len)), line_of.(body_at + at)}
        nil -> {:dynamic, line_of.(body_at + m_at)}
      end
    end
  end

  defp job_id(header) do
    [_, id] = Regex.run(~r/^\s+("[^"]+"|'[^']+'|[A-Za-z0-9_-]+):/, header)
    String.trim(id, "\"") |> String.trim("'")
  end

  # What the job runs next to its credential, for a finding's reason.
  defp exposure_note(%{exposures: []}), do: ""

  defp exposure_note(job) do
    "; it holds the credential next to #{Enum.join(job.exposures, ", ")}" <>
      if(job.shared?, do: " in the same step", else: "")
  end

  # Harden-runner contains a job, so a finding about its attestation is :warn.
  defp re001_severity(job), do: if(job.shared? and not job.hardened?, do: :high, else: :warn)

  defp re001_undeclared(job, deed_file, deed_note) do
    declare =
      "or declare the job in the `(egress …)` clause of " <>
        "#{deed_file || "the repo deed (<repo>_chora.deed)"}: " <>
        "(job :workflow \"#{job.workflow}\" :job \"#{job.id}\" " <>
        ":secrets (#{quoted_list(job.secrets)}) :endpoints () :enforcement none), " <>
        "listing in :endpoints every host outside GitHub the job reaches — standards ADR-009."

    harden =
      "harden-runner as the first step of this job, with egress-policy: block " <>
        "and an allowlist derived from the job's actual required endpoints, "

    fix =
      if job.exposures == [],
        do: "Add " <> harden <> declare,
        else:
          "Move the credential into a job that runs no fetched or untrusted code " <>
            "(pass data between jobs as artifacts), add " <> harden <> declare

    %{
      rule: "RE001",
      file: job.workflow,
      severity: re001_severity(job),
      line: job.line,
      reason:
        "job `#{job.id}` in #{job.workflow} references `secrets.*` but does not install " <>
          "`step-security/harden-runner` or declare its egress — review outbound-egress monitoring" <>
          exposure_note(job) <>
          if(deed_note, do: "; #{deed_note}", else: ""),
      action: :report,
      detail: %{exposures: job.exposures, fix: fix}
    }
  end

  defp re001_dynamic(job, deed_file, declared?) do
    %{
      rule: "RE001",
      file: job.workflow,
      severity: re001_severity(job),
      line: job.dynamic_line,
      reason:
        "job `#{job.id}` in #{job.workflow} reads secrets by a key computed at run time " <>
          "(secrets[…], toJSON(secrets) or similar), which no egress declaration can list" <>
          exposure_note(job) <>
          if(declared?, do: "; its entry in #{deed_file} does not cover it", else: ""),
      action: :report,
      detail: %{
        exposures: job.exposures,
        fix:
          if(job.hardened?,
            do:
              "Read each secret by its literal name (secrets.NAME) and declare the set, " <>
                "or remove the job's entry from #{deed_file}: its harden-runner step " <>
                "already satisfies RE001 — standards ADR-009.",
            else:
              "Read each secret by its literal name (secrets.NAME) and declare the set, " <>
                "or add harden-runner as the first step of this job with egress-policy: block — " <>
                "standards ADR-009."
          )
      }
    }
  end

  defp re001_drift(job, deed_file, declared) do
    unlisted = MapSet.difference(job.secrets, declared)
    unused = MapSet.difference(declared, job.secrets)

    # An incomplete declaration must not lower the finding the job would
    # draw undeclared: drift keeps the job's own severity.
    %{
      rule: "RE001",
      file: job.workflow,
      severity: re001_severity(job),
      line: job.line,
      reason:
        "job `#{job.id}` in #{job.workflow} no longer matches its egress declaration in " <>
          "#{deed_file}: " <>
          Enum.join(
            Enum.reject(
              [
                if(MapSet.size(unlisted) > 0, do: "reads undeclared #{quoted_list(unlisted)}"),
                if(MapSet.size(unused) > 0, do: "declares unread #{quoted_list(unused)}")
              ],
              &is_nil/1
            ),
            "; "
          ) <> exposure_note(job),
      action: :report,
      detail: %{
        exposures: job.exposures,
        fix:
          "Make :secrets of (job :workflow \"#{job.workflow}\" :job \"#{job.id}\") in " <>
            "#{deed_file} equal the secrets the job reads: (#{quoted_list(job.secrets)}), " <>
            "and review :endpoints for the change."
      }
    }
  end

  defp re001_enforcement(job, deed_file, enforcement) do
    %{
      rule: "RE001",
      file: job.workflow,
      severity: re001_severity(job),
      line: job.line,
      reason:
        "job `#{job.id}` in #{job.workflow} is declared in #{deed_file} with " <>
          ":enforcement #{enforcement}, but " <>
          if(job.hardened?,
            do: "its harden-runner step runs `egress-policy: #{job.harden_policy}`",
            else: "does not install `step-security/harden-runner`"
          ) <> exposure_note(job),
      action: :report,
      detail: %{
        exposures: job.exposures,
        fix:
          if job.hardened? do
            "Set `egress-policy: block` on this job's harden-runner step, or declare " <>
              ":enforcement harden-runner-audit on (job :workflow \"#{job.workflow}\" " <>
              ":job \"#{job.id}\") in #{deed_file}."
          else
            "Restore harden-runner as the first step of this job, or set :enforcement none " <>
              "on (job :workflow \"#{job.workflow}\" :job \"#{job.id}\") in #{deed_file} " <>
              "if nothing holds the job to its :endpoints."
          end
      }
    }
  end

  defp re001_stale({workflow, id}, deed_file) do
    %{
      rule: "RE001",
      file: deed_file,
      severity: :warn,
      reason:
        "#{deed_file} declares egress for job `#{id}` in #{workflow}, but no locally run " <>
          "job there reads a real secret — the attestation is stale",
      action: :report,
      detail: %{
        fix:
          "Remove (job :workflow \"#{workflow}\" :job \"#{id}\") from the `(egress …)` clause, " <>
            "or correct its :workflow / :job."
      }
    }
  end

  defp quoted_list(names), do: names |> Enum.sort() |> Enum.map_join(" ", &"\"#{&1}\"")

  defp re001_exposures(steps, untrusted_events) do
    code = Enum.flat_map(steps, &step_code_exposures/1)

    [
      untrusted_events != [] &&
        "an untrusted-input trigger (#{Enum.join(untrusted_events, ", ")})",
      :head in code && "a pull-request or triggering-run head checkout",
      :fetched in code && "fetched code (a piped installer or a dependency install/build)"
    ]
    |> Enum.filter(&is_binary/1)
  end

  # What a step executes that the job cannot vouch for: `:head` when it checks
  # out or fetches the head of a pull request or triggering run, `:fetched`
  # when its `run:` script pipes a download into a shell or installs or builds
  # dependencies. Only executable content counts: an `env:` value or a YAML
  # comment that mentions `npm ci` runs nothing.
  defp step_code_exposures(step) do
    run_lines = step_run_lines(step)
    # Joined with newlines so a folded `run: >-` command split across lines
    # still matches: the patterns' `\s+` spans the line break.
    run = Enum.join(run_lines, "\n")

    # A `ref:`/`repository:` input fetches the head only on a checkout; the
    # same input on a reporting action merely names it.
    checkout? = Enum.any?(step, fn {line, _} -> Regex.match?(@re001_checkout_action, line) end)

    head? =
      (checkout? and
         Enum.any?(step, fn {line, _} ->
           Regex.match?(@re001_checkout_key, line) and Regex.match?(@re001_head_ref, line)
         end)) or Regex.match?(@re001_git_head_fetch, run)

    [head? && :head, Regex.match?(@re001_fetched_code, run) && :fetched]
    |> Enum.filter(& &1)
  end

  # The script of a step's `run:`, inline or as a block scalar, with comments
  # removed (YAML and shell agree that `#` after whitespace starts one).
  defp step_run_lines(step) do
    key_column = step_key_column(step)

    # Only the step's own `run:` property: an `env:` variable named `run` is a
    # nested key, deeper than the step's properties.
    case Enum.split_while(step, fn {line, _} ->
           case Regex.run(~r/^(\s*(?:-\s+)?)(?:run|"run"|'run')\s*:/, line) do
             [_, prefix] -> String.length(prefix) != key_column
             _ -> true
           end
         end) do
      {_, []} ->
        []

      {_, [{run_line, _} | rest]} ->
        [prefix, value] =
          Regex.run(~r/^(\s*(?:-\s+)?)(?:run|"run"|'run')\s*:\s*(.*)$/, run_line,
            capture: :all_but_first
          )

        key_column = String.length(prefix)

        # A block scalar, its header after any anchor (`run: &script |`).
        body =
          if Regex.match?(~r/^(?:&[A-Za-z0-9_-]+\s+)?[>|][0-9+-]*\s*(?:#.*)?$/, value) do
            rest
            |> Enum.take_while(fn {line, _} ->
              String.trim(line) == "" or indent_of(line) > key_column
            end)
            |> Enum.map(&elem(&1, 0))
          else
            [value]
          end

        Enum.map(body, &String.replace(&1, ~r/(?:^|\s)#.*$/, ""))
    end
  end

  # The column of a step's own properties: just past `- ` on its opening line,
  # or the indent of the first property under a bare `-`.
  defp step_key_column([{first, _} | rest]) do
    case Regex.run(~r/^(\s*-\s+)\S/, first) do
      [_, prefix] ->
        String.length(prefix)

      _ ->
        Enum.find_value(rest, indent_of(first) + 2, fn {line, _} ->
          String.trim(line) != "" && indent_of(line)
        end)
    end
  end

  defp step_key_column([]), do: 0

  # True when the step sets the same variable as the job-level `env:` line,
  # so the step-level value replaces the inherited credential.
  defp overrides_env?(step, {job_line, _}) do
    case Regex.run(~r/^\s*([A-Za-z_][A-Za-z0-9_]*)\s*:/, job_line) do
      [_, key] ->
        Enum.any?(step, fn {line, _} -> Regex.match?(~r/^\s*#{key}\s*:/, line) end)

      _ ->
        false
    end
  end

  # The workflow's top-level `env:` (key at column 0) with its indented body.
  # Every job, and every step in it, inherits these variables.
  defp workflow_env_lines(active_lines) do
    case Enum.drop_while(active_lines, fn {line, _} ->
           not (indent_of(line) == 0 and key_line?(line, "env"))
         end) do
      [env_line | rest] ->
        body = Enum.take_while(rest, fn {line, _} -> line == "" or indent_of(line) > 0 end)
        [env_line | body]

      [] ->
        []
    end
  end

  # The lines under the job's own `env:` key.
  defp job_env_lines(job_lines) do
    with field_indent when is_integer(field_indent) <- job_field_indent(job_lines),
         index when is_integer(index) <-
           Enum.find_index(job_lines, fn {line, _} ->
             indent_of(line) == field_indent and key_line?(line, "env")
           end) do
      job_lines
      |> Enum.drop(index)
      |> then(fn [env_line | rest] ->
        [env_line | Enum.take_while(rest, fn {line, _} -> indent_of(line) > field_indent end)]
      end)
    else
      _ -> []
    end
  end

  # The events a job-level `if:` restricts the job to, when the condition is
  # nothing but `github.event_name == '<event>'` tests joined by `||`.
  # Anything more complex is not interpreted, and the job is judged on all the
  # workflow's events.
  defp job_event_gate(job_lines) do
    with field_indent when is_integer(field_indent) <- job_field_indent(job_lines),
         {line, _} <-
           Enum.find(job_lines, fn {line, _} ->
             indent_of(line) == field_indent and key_line?(line, "if")
           end),
         [_, condition] <- Regex.run(~r/^\s*["']?if["']?\s*:\s*(.*?)\s*$/, line),
         condition = condition |> String.replace(~r/^\$\{\{\s*|\s*\}\}$/, ""),
         test = ~S"github\.event_name\s*==\s*'([a-z_]+)'",
         true <- Regex.match?(~r/^\s*#{test}(?:\s*\|\|\s*#{test})*\s*$/, condition) do
      ~r/github\.event_name\s*==\s*'([a-z_]+)'/
      |> Regex.scan(condition, capture: :all_but_first)
      |> List.flatten()
    else
      _ -> nil
    end
  end

  # The job's steps, each as its own physical lines.
  defp job_steps(job_lines) do
    with field_indent when is_integer(field_indent) <- job_field_indent(job_lines),
         step_lines when step_lines != [] <- lines_in_steps(job_lines, field_indent),
         step_indent when is_integer(step_indent) <- step_indent(step_lines, field_indent) do
      step_lines
      |> Enum.chunk_while(
        [],
        fn {line, _} = entry, acc ->
          if indent_of(line) == step_indent and Regex.match?(~r/^\s*-(?:\s|$)/, line) and
               acc != [] do
            {:cont, Enum.reverse(acc), [entry]}
          else
            {:cont, [entry | acc]}
          end
        end,
        fn
          [] -> {:cont, []}
          acc -> {:cont, Enum.reverse(acc), []}
        end
      )
    else
      _ -> []
    end
  end

  # The event names under the workflow's top-level `on:`, in any of its forms:
  # `on: push`, `on: [push, issues]`, or a block of event keys or list items.
  defp workflow_events(lines) do
    case Enum.split_while(lines, fn {line, _} ->
           not Regex.match?(~r/^["']?on["']?:/, line)
         end) do
      {_, []} ->
        []

      {_, [{on_line, _} | rest]} ->
        inline =
          on_line
          |> String.split(":", parts: 2)
          |> List.last()
          |> String.replace(~r/\s+#.*$/, "")
          # `on: &events` anchors the block mapping that follows.
          |> String.replace(~r/^\s*&[A-Za-z0-9_-]+/, "")
          |> String.trim()

        if inline != "" do
          # A flow collection may continue past the `on:` line: read on until
          # its brackets balance.
          rest
          |> Enum.map(&elem(&1, 0))
          |> Enum.reduce_while(inline, fn line, acc ->
            if nesting_at(acc, byte_size(acc)) > 0,
              do: {:cont, acc <> " " <> String.trim(line)},
              else: {:halt, acc}
          end)
          |> inline_events()
        else
          block =
            Enum.take_while(rest, fn {line, _} ->
              String.trim(line) == "" or indent_of(line) > 0
            end)
            |> Enum.reject(fn {line, _} -> String.trim(line) == "" end)

          event_indent =
            block |> Enum.map(fn {line, _} -> indent_of(line) end) |> Enum.min(fn -> nil end)

          block
          |> Enum.filter(fn {line, _} -> indent_of(line) == event_indent end)
          |> Enum.flat_map(fn {line, _} ->
            case Regex.run(~r/^\s*(?:-\s+)?["']?([A-Za-z_]+)["']?\s*:?/, line) do
              [_, event] -> [event]
              _ -> []
            end
          end)
        end
    end
  end

  # `on: {push: {paths: [issues/**]}}` names one event, push: only the map's
  # top-level keys are events. A scalar or list names each of its words.
  defp inline_events("{" <> _ = map) do
    ~r/["']?([A-Za-z_]+)["']?\s*:/
    |> Regex.scan(map, return: :index)
    |> Enum.flat_map(fn [{start, _}, {from, len}] ->
      if nesting_at(map, start) == 1, do: [binary_part(map, from, len)], else: []
    end)
  end

  defp inline_events(scalar_or_list) do
    ~r/[A-Za-z_]+/ |> Regex.scan(scalar_or_list) |> List.flatten()
  end

  defp nesting_at(text, position) do
    text
    |> binary_part(0, position)
    |> String.graphemes()
    |> Enum.reduce(0, fn
      c, depth when c in ["{", "["] -> depth + 1
      c, depth when c in ["}", "]"] -> depth - 1
      _, depth -> depth
    end)
  end

  defp direct_job_field?(job_lines, field) do
    case job_field_indent(job_lines) do
      nil ->
        false

      field_indent ->
        Enum.any?(job_lines, fn {line, _} ->
          indent_of(line) == field_indent and key_line?(line, field)
        end)
    end
  end

  # A line that opens the mapping key `key`: YAML reads `env:`, `"env":` and
  # `'env':` as the same key, and so does GitHub.
  defp key_line?(line, key) do
    key = Regex.escape(key)
    Regex.match?(~r/^\s*(?:#{key}|"#{key}"|'#{key}')\s*:(?:\s|$)/, line)
  end

  defp harden_runner_step?(job_lines) do
    with field_indent when is_integer(field_indent) <- job_field_indent(job_lines),
         step_lines when step_lines != [] <- lines_in_steps(job_lines, field_indent),
         step_indent when is_integer(step_indent) <- step_indent(step_lines, field_indent) do
      case Enum.reduce_while(step_lines, {false, nil, nil}, fn {line, _},
                                                               {in_step, property_indent,
                                                                scalar_indent} ->
             indent = indent_of(line)

             cond do
               scalar_indent && indent > scalar_indent ->
                 {:cont, {in_step, property_indent, scalar_indent}}

               indent == step_indent and Regex.match?(~r/^\s*-\s+/, line) ->
                 if harden_runner_uses?(line) do
                   {:halt, true}
                 else
                   {:cont, {true, nil, block_scalar?(line) && indent}}
                 end

               in_step and indent > step_indent ->
                 direct_property? = is_nil(property_indent) or indent == property_indent
                 property_indent = property_indent || indent

                 if direct_property? and harden_runner_uses?(line) do
                   {:halt, true}
                 else
                   {:cont, {in_step, property_indent, block_scalar?(line) && indent}}
                 end

               true ->
                 {:cont, {in_step, property_indent, nil}}
             end
           end) do
        true -> true
        _ -> false
      end
    else
      _ -> false
    end
  end

  defp job_field_indent([_header | job_lines]) do
    job_lines
    |> Enum.map(&elem(&1, 0))
    |> Enum.reject(&(String.trim(&1) == ""))
    |> Enum.map(&indent_of/1)
    |> Enum.min(fn -> nil end)
  end

  defp job_field_indent([]), do: nil

  defp lines_in_steps(job_lines, field_indent) do
    case Enum.find_index(job_lines, fn {line, _} ->
           indent_of(line) == field_indent and key_line?(line, "steps")
         end) do
      nil ->
        []

      index ->
        job_lines
        |> Enum.drop(index + 1)
        |> Enum.take_while(fn {line, _} ->
          String.trim(line) == "" or indent_of(line) > field_indent
        end)
    end
  end

  defp step_indent(step_lines, field_indent) do
    step_lines
    |> Enum.filter(fn {line, _} ->
      indent_of(line) > field_indent and Regex.match?(~r/^\s*-(?:\s|$)/, line)
    end)
    |> Enum.map(fn {line, _} -> indent_of(line) end)
    |> Enum.min(fn -> nil end)
  end

  # The egress-policy input of the job's harden-runner step; harden-runner
  # defaults to audit when none is set. nil without the step.
  defp harden_runner_policy(steps) do
    case Enum.find(steps, fn step ->
           Enum.any?(step, fn {line, _} -> harden_runner_uses?(line) end)
         end) do
      nil -> nil
      step -> step |> step_inputs() |> Map.get("egress-policy", "audit")
    end
  end

  # One step's `with:` inputs as %{name => value}, from a block mapping under
  # the key or a flow mapping on its line. A YAML comment sets no input, so
  # comments are stripped first: `uses: …@v2 # egress-policy: block` leaves
  # the policy at its default.
  @with_key ~r/^(\s*(?:-\s+)?)(?:with|"with"|'with')\s*:(?:\s(.*))?$/
  @block_input ~r/^\s*["']?([A-Za-z0-9_-]+)["']?\s*:(?:\s+(.*))?$/
  @flow_input ~r/[{,]\s*["']?([A-Za-z0-9_-]+)["']?\s*:\s*("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^,}]*)/
  defp step_inputs(step) do
    lines = Enum.map(step, fn {line, _} -> strip_yaml_comment(line) end)

    case Enum.find_index(lines, &Regex.match?(@with_key, &1)) do
      nil ->
        %{}

      index ->
        [with_line | rest] = Enum.drop(lines, index)
        [lead, inline] = with_key_parts(with_line)

        if String.starts_with?(inline, "{") do
          # A flow mapping may continue past the `with:` line.
          rest
          |> Enum.reduce_while(inline, fn line, acc ->
            if nesting_at(acc, byte_size(acc)) > 0,
              do: {:cont, acc <> " " <> String.trim(line)},
              else: {:halt, acc}
          end)
          |> then(&Regex.scan(@flow_input, &1, capture: :all_but_first))
          |> Map.new(fn [name, value] -> {name, scalar_value(value)} end)
        else
          column = String.length(lead)
          body = Enum.take_while(rest, &(String.trim(&1) == "" or indent_of(&1) > column))

          child_indent =
            body
            |> Enum.reject(&(String.trim(&1) == ""))
            |> Enum.map(&indent_of/1)
            |> Enum.min(fn -> nil end)

          for line <- body,
              indent_of(line) == child_indent,
              [_ | captures] <- [Regex.run(@block_input, line)],
              into: %{} do
            case captures do
              [name, value] -> {name, scalar_value(value)}
              [name] -> {name, ""}
            end
          end
        end
    end
  end

  defp with_key_parts(line) do
    case Regex.run(@with_key, line) do
      [_, lead, inline] -> [lead, String.trim(inline)]
      [_, lead] -> [lead, ""]
    end
  end

  # A plain or quoted scalar's text.
  defp scalar_value(value) do
    value = String.trim(value)

    cond do
      Regex.match?(~r/^"(?:[^"\\]|\\.)*"$/, value) ->
        String.slice(value, 1..-2//1)

      Regex.match?(~r/^'(?:[^']|'')*'$/, value) ->
        value |> String.slice(1..-2//1) |> String.replace("''", "'")

      true ->
        value
    end
  end

  # A line without its YAML comment: a `#` at the start or after whitespace,
  # outside a quoted scalar. A quote opens a scalar only where one can start
  # (after whitespace or a flow indicator), so the apostrophe in `it's` does not.
  # With `blank_quoted`, each quoted scalar's text becomes spaces of the same
  # byte length, so what remains is structure at its original offsets.
  defp strip_yaml_comment(line, blank_quoted \\ false) do
    text = fn c -> if blank_quoted, do: String.duplicate(" ", byte_size(c)), else: c end

    line
    |> String.graphemes()
    |> Enum.reduce_while({[], nil, " ", false}, fn c, {acc, quote, prev, escaped} ->
      cond do
        quote == "\"" and escaped ->
          {:cont, {[text.(c) | acc], quote, c, false}}

        quote == "\"" and c == "\\" ->
          {:cont, {[text.(c) | acc], quote, c, true}}

        quote != nil and c == quote ->
          {:cont, {[c | acc], nil, c, false}}

        quote != nil ->
          {:cont, {[text.(c) | acc], quote, c, false}}

        c == "#" and String.trim(prev) == "" ->
          {:halt, {acc, nil, c, false}}

        c in ["'", "\""] and (String.trim(prev) == "" or prev in ["{", "[", ","]) ->
          {:cont, {[c | acc], c, c, false}}

        true ->
          {:cont, {[c | acc], nil, c, false}}
      end
    end)
    |> elem(0)
    |> Enum.reverse()
    |> Enum.join()
    |> String.trim_trailing()
  end

  # GitHub resolves an action's owner/repo without regard to case
  # (Step-Security/Harden-Runner@v2 installs the same action); the `uses:`
  # key itself is case-sensitive YAML.
  defp harden_runner_uses?(line) do
    Regex.match?(
      ~r/^\s*(?:-\s+)?(?:uses|"uses"|'uses')\s*:\s*["']?(?i:step-security\/harden-runner)@/,
      line
    )
  end

  defp block_scalar?(line),
    do: Regex.match?(~r/:\s*(?:&[A-Za-z0-9_-]+\s+)?[>|][0-9+-]*\s*(?:#.*)?$/, line)

  # Follow block-style jobs by indentation, retaining physical source lines.
  # As with the other research rules, this is a local static text analysis.
  defp workflow_job_lines(lines) do
    {_in_jobs, _indent, groups} =
      Enum.reduce(lines, {false, nil, []}, fn {line, _} = entry, {in_jobs, indent, groups} ->
        cond do
          Regex.match?(~r/^(?:jobs|"jobs"|'jobs')\s*:\s*(?:#.*)?$/, line) ->
            {true, nil, groups}

          not in_jobs ->
            {false, indent, groups}

          Regex.match?(~r/^\S/, line) ->
            {false, nil, groups}

          true ->
            header = Regex.run(~r/^(\s+)(?:[A-Za-z0-9_-]+|"[^"]+"|'[^']+'):/, line)
            width = if header, do: String.length(Enum.at(header, 1)), else: nil

            cond do
              width && (is_nil(indent) || width == indent) ->
                {true, width, [[entry] | groups]}

              groups != [] ->
                [current | rest] = groups
                {true, indent, [[entry | current] | rest]}

              true ->
                {true, indent, groups}
            end
        end
      end)

    groups |> Enum.reverse() |> Enum.map(&Enum.reverse/1)
  end

  # ─── RE002: Harden-Runner in audit-only mode ─────────────────────────

  @doc """
  RE002: Harden-Runner is installed but configured with
  `egress-policy: audit`. Provenance: StepSecurity production-deployment
  guidance — audit mode is a deliberate first-rollout setting that
  becomes a finding when it persists on a workflow that touches
  protected branches (or runs on `push` to default).

  Severity: `:high`. Action: `:report`.
  """
  def re002_harden_runner_audit_mode(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      installs_harden? = Regex.match?(~r/uses:\s*step-security\/harden-runner/, content)

      audit_mode? = Regex.match?(~r/egress-policy:\s*audit\b/, content)

      protected_branch_trigger? =
        Regex.match?(~r/^\s*on:\s*push\b/m, content) or
          Regex.match?(~r/^\s+push:/m, content) or
          Regex.match?(~r/^\s+release:/m, content)

      if installs_harden? and audit_mode? and protected_branch_trigger? do
        [
          %{
            rule: "RE002",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel} runs harden-runner in `egress-policy: audit` — " <>
                "telemetry-only, never blocks; on a protected-branch trigger this is not containment",
            action: :report,
            detail: %{
              fix:
                "Switch `egress-policy: audit` to `egress-policy: block` once " <>
                  "the allow-list is stable. Keep `audit` only for first-rollout windows."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── RE003: cache key interpolates attacker-controlled input ─────────

  @doc """
  RE003: `actions/cache` key interpolates `github.head_ref` (or PR
  title/body / branch-name fragment). Forked PRs can choose a head
  ref that overlaps a privileged restore key — cache poisoning.
  Provenance: Snyk dataflow auditor + 2022 GitHub cache-poisoning
  advisory.

  Severity: `:high`. Action: `:report`.
  """
  def re003_cache_key_poisoning(repo_path) do
    untrusted_key_re =
      ~r/key:\s*[^\n]*\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref))/

    restore_keys_re =
      ~r/restore-keys:[\s\S]{0,200}?\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref))/

    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = read_workflow(path)
      rel = Path.relative_to(path, repo_path)

      uses_cache? = Regex.match?(~r/uses:\s*actions\/cache(?:@|\s)/, content)

      if uses_cache? and
           (Regex.match?(untrusted_key_re, content) or
              Regex.match?(restore_keys_re, content)) do
        # Locate first match for line number.
        idx =
          case Regex.run(untrusted_key_re, content, return: :index) do
            [{i, _} | _] ->
              i

            _ ->
              case Regex.run(restore_keys_re, content, return: :index) do
                [{i, _} | _] -> i
                _ -> 0
              end
          end

        line_no = line_number_for_offset(content, idx)

        [
          %{
            rule: "RE003",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel}:#{line_no} caches with a key derived from an " <>
                "attacker-controlled context (`github.head_ref` / PR title/body) — cache poisoning vector",
            action: :report,
            detail: %{
              line: line_no,
              fix:
                "Derive cache key from immutable inputs only: " <>
                  "`${{ hashFiles('**/Cargo.lock') }}` or `${{ github.sha }}`. " <>
                  "Never key off head_ref / PR title / branch name."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── RE004: container action pinned by tag, not digest ────────────────

  @doc """
  RE004: A `docker://image:tag` reference (in `uses:` or `container:`)
  is pinned by tag rather than a SHA256 digest. Tag mutation re-points
  the image at attacker-controlled bytes. Provenance: Snyk container
  scanning's tag-pin warning + GitHub's pin-by-digest hardening guide.

  Severity: `:warn`. Action: `:report`.
  """
  def re004_container_tag_pin(repo_path) do
    # `docker://owner/image:tag` (no @sha256:)
    docker_uses_re = ~r/uses:\s*(docker:\/\/\S+?)(?:\s|$)/

    container_re = ~r/^\s*(?:image|container):\s*(\S+)\s*$/m

    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      uses_findings =
        Regex.scan(docker_uses_re, content, return: :index)
        |> Enum.flat_map(fn [{full_start, _}, {ref_start, ref_len}] ->
          ref = binary_part(content, ref_start, ref_len)

          if String.contains?(ref, "@sha256:") do
            []
          else
            line_no = line_number_for_offset(content, full_start)
            [finding_re004(rel, line_no, ref)]
          end
        end)

      container_findings =
        Regex.scan(container_re, content, return: :index)
        |> Enum.flat_map(fn [{full_start, _}, {ref_start, ref_len}] ->
          ref = binary_part(content, ref_start, ref_len)

          cond do
            String.contains?(ref, "@sha256:") ->
              []

            # Plausibility filter: must contain ":" (tag) and look like
            # an image reference, not a free-text YAML value.
            not String.contains?(ref, ":") ->
              []

            String.starts_with?(ref, "${{") ->
              []

            true ->
              line_no = line_number_for_offset(content, full_start)
              [finding_re004(rel, line_no, ref)]
          end
        end)

      uses_findings ++ container_findings
    end)
  end

  defp finding_re004(file, line_no, ref) do
    %{
      rule: "RE004",
      file: file,
      severity: :warn,
      reason:
        "workflow #{file}:#{line_no} pins container `#{ref}` by tag — " <>
          "tag is mutable; pin by sha256 digest instead",
      action: :report,
      detail: %{
        line: line_no,
        image: ref,
        fix:
          "Resolve the tag to a digest:\n" <>
            "  docker buildx imagetools inspect #{ref} --format '{{.Manifest.Digest}}'\n" <>
            "Then pin as `#{ref}@sha256:<digest>` (drop or keep the tag for readability)."
      }
    }
  end

  # ─── RE005: test step swallows non-zero exit ─────────────────────────

  @doc """
  RE005: A step whose name contains `test`/`spec`/`check` either uses
  `continue-on-error: true` or pipes the command to `|| true`. The
  suite goes green while real failures pile up. Provenance: Datadog
  CI Visibility flake-masking patterns.

  `|| true` on a `trap …` line is not counted: the handler runs after the
  step's status is decided and cannot mask it.

  Severity: `:warn`. Action: `:report`.
  """
  def re005_test_swallows_exit(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = read_workflow(path)
      rel = Path.relative_to(path, repo_path)

      content
      |> step_blocks()
      |> Enum.flat_map(fn {name, line_no, body} ->
        is_test? =
          is_binary(name) and
            Regex.match?(~r/(?:test|spec|check|lint|verify)/i, name)

        # Strip YAML comments before matching (issue #834): a commented-out
        # `# continue-on-error: true` or a trailing note containing `|| true`
        # is not a swallow. Whole-line comments and whitespace-preceded
        # trailing comments — YAML and bash agree that `#` begins a comment
        # only after whitespace or at line start, so `a#b` is untouched.
        stripped_body =
          body
          |> String.split("\n")
          |> Enum.map_join("\n", fn line ->
            line
            |> String.replace(~r/^[ \t]*#.*$/m, "")
            |> String.replace(~r/[ \t]#.*$/m, "")
          end)

        has_continue_on_error? = Regex.match?(~r/continue-on-error:\s*true\b/, stripped_body)

        # A `trap` handler's `|| true` is not a swallow. The step's exit status
        # is decided before the EXIT trap runs, and a failing handler command
        # under `set -e` (e.g. `kill` of an already-exited PID) would itself
        # turn a PASSING step red, so the `|| true` is what keeps it honest.
        # Only the quoted handler is dropped, so a command after it on the
        # same line (`trap '…' EXIT; cargo test || true`) is still judged.
        has_or_true? =
          stripped_body
          |> String.replace(~r/\btrap[ \t]+(?:'[^'\n]*'|"(?:[^"\\\n]|\\.)*")/, "trap")
          |> then(&Regex.match?(~r/\|\|\s*true\b/, &1))

        # A step carrying `hypatia:ignore RE005 -- <reason>` (or the
        # `hypatia: allow research_extensions/RE005` spelling) is reviewed —
        # the swallow is deliberate and the reason is on the record. The
        # directive is read from the RAW step text, comments included, so it
        # can live in a YAML comment next to the swallow it blesses.
        inside_step_lines =
          [name, body]
          |> Enum.filter(&is_binary/1)
          |> Enum.flat_map(&String.split(&1, "\n"))

        # ... plus the three physical lines above the step opener, so the
        # directive can also sit where a reviewer's eye lands first.
        above_step_lines =
          content
          |> String.split("\n")
          |> Enum.slice(max(line_no - 4, 0), 3)

        reviewed? =
          (inside_step_lines ++ above_step_lines)
          |> Enum.any?(fn line ->
            Hypatia.ScannerSuppression.inline_allowed?(
              line,
              nil,
              "research_extensions",
              "RE005"
            )
          end)

        if is_test? and (has_continue_on_error? or has_or_true?) and not reviewed? do
          mechanism =
            cond do
              has_continue_on_error? and has_or_true? ->
                "`continue-on-error: true` AND `|| true`"

              has_continue_on_error? ->
                "`continue-on-error: true`"

              true ->
                "`|| true`"
            end

          [
            %{
              rule: "RE005",
              file: rel,
              severity: :warn,
              reason:
                "workflow #{rel}:#{line_no} step `#{name}` swallows non-zero " <>
                  "exit via #{mechanism} — failures will be masked",
              action: :report,
              detail: %{
                line: line_no,
                step_name: name,
                fix:
                  "Remove the failure-swallow. If a single test is genuinely " <>
                    "flaky, retry it explicitly (e.g. `nextest --retries 2`) — never " <>
                    "swallow the exit of the whole step."
              }
            }
          ]
        else
          []
        end
      end)
    end)
  end

  # ─── RE006: composite action calls unpinned third-party action ───────

  @doc """
  RE006: A composite `action.yml` (with `runs.using: composite`) has a
  `uses:` reference to a third-party action that is not SHA-pinned.
  The composite's consumers may pin the composite itself, but the
  composite then transitively loads a tag. Provenance: Endor Labs
  R-END-05.

  Severity: `:warn`. Action: `:report`.
  """
  def re006_composite_unpinned_nested(repo_path) do
    repo_path
    |> composite_action_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      composite? = Regex.match?(~r/using:\s*['"]?composite['"]?/, content)

      if composite? do
        Regex.scan(~r/^\s*-?\s*uses:\s*(\S+)/m, content, return: :index)
        |> Enum.flat_map(fn [{full_start, _}, {slug_start, slug_len}] ->
          # byte offsets from return: :index (see WorkflowHardening.wh004)
          slug = binary_part(content, slug_start, slug_len)

          cond do
            String.starts_with?(slug, "./") or String.starts_with?(slug, "docker://") ->
              []

            Regex.match?(~r/@[a-fA-F0-9]{40}\b/, slug) ->
              []

            String.contains?(slug, "@") ->
              line_no = line_number_for_offset(content, full_start)

              [
                %{
                  rule: "RE006",
                  file: rel,
                  severity: :warn,
                  reason:
                    "composite action #{rel}:#{line_no} loads `#{slug}` by " <>
                      "tag/branch — nested-pinning gap; the composite's consumers " <>
                      "cannot enforce immutability transitively",
                  action: :report,
                  detail: %{
                    line: line_no,
                    uses: slug,
                    fix:
                      "Replace tag/branch with a 40-char commit SHA in the composite's " <>
                        "`uses:`. Add `# <tag>` as a trailing comment for readability."
                  }
                }
              ]

            true ->
              []
          end
        end)
      else
        []
      end
    end)
  end

  # ─── RE007: workflow-level env: exposes secrets to all steps ─────────

  @doc """
  RE007: Workflow-level (top-level) `env:` block contains a
  `${{ secrets.* }}` reference, exposing the secret to every step
  including untrusted third-party actions. Provenance: Endor Labs
  R-END-01 (least-privilege secrets scope).

  Severity: `:warn`. Action: `:report`.
  """
  def re007_workflow_env_secret_exposure(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      # Detect top-level (column-0) `env:` block. Capture indented body
      # until the next column-0 key.
      case Regex.run(~r/^env:\s*\n((?:[ \t]+[^\n]*\n?)+)/m, content, return: :index) do
        [{full_start, _}, {body_start, body_len}] ->
          body = binary_part(content, body_start, body_len)

          if Regex.match?(~r/\$\{\{\s*secrets\.[A-Za-z_][A-Za-z0-9_]*/, body) do
            line_no = line_number_for_offset(content, full_start)

            [
              %{
                rule: "RE007",
                file: rel,
                severity: :warn,
                reason:
                  "workflow #{rel}:#{line_no} declares a top-level `env:` " <>
                    "block that references `secrets.*` — every step (including " <>
                    "third-party actions) inherits the secret",
                action: :report,
                detail: %{
                  line: line_no,
                  fix:
                    "Move the `secrets.*` references into the specific step's " <>
                      "`env:` block, or into the specific job's `env:`. Reserve the " <>
                      "top-level `env:` for non-secret values."
                }
              }
            ]
          else
            []
          end

        _ ->
          []
      end
    end)
  end

  # ─── RE008: spoofable bot-identity gate ──────────────────────────────

  @doc """
  RE008: Find `github.actor` comparisons using `==` or `!=` with a quoted
  bot login ending in `[bot]` in workflow text.
  Provenance: zizmor `bot-conditions` + Koishybayev et al. (USENIX
  Security 2022).

  Suppress a match when the same line contains an `&&`-separated equality
  between `github.event.pull_request.user.login` and the same bot login,
  with no `||` or logical `!` (`!=` is allowed). Expression wrappers and
  parentheses around that equality are accepted.

  Return one finding per remaining match, with a repo-relative file path
  and a one-based line number in `detail.line`, or `[]` when none remain.
  Severity: `:critical`. Action: `:report`.
  Raises `File.Error` if workflow directory listing or file reading fails.
  """
  def re008_spoofable_bot_gate(repo_path) do
    # github.actor compared to any bot-identity string. Catch both
    # `==` and `!=` orientations and either quote style.
    bot_gate_re = ~r/github\.actor\s*(?:==|!=)\s*['"]([a-zA-Z0-9_-]+\[bot\])['"]/

    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      lines = String.split(content, "\n")

      Regex.scan(bot_gate_re, content, return: :index)
      |> Enum.map(fn [{idx, _}, {name_start, name_len}] ->
        name = binary_part(content, name_start, name_len)
        {name, line_number_for_offset(content, idx)}
      end)
      |> Enum.reject(fn {name, line_no} ->
        author_pinned_gate?(Enum.at(lines, line_no - 1, ""), name)
      end)
      |> Enum.map(fn {name, line_no} ->
        %{
          rule: "RE008",
          file: rel,
          severity: :critical,
          reason:
            "workflow #{rel}:#{line_no} gates on `github.actor == '#{name}'` — " <>
              "`github.actor` is the run-triggering user, which an attacker controls " <>
              "on `pull_request_target` from a fork",
          action: :report,
          detail: %{
            line: line_no,
            spoofable_login: name,
            fix:
              "Compare against the numeric `github.actor_id`, or against " <>
                "`github.event.pull_request.user.login` (the PR author), or " <>
                "verify via the GitHub App token — never the human-readable login alone."
          }
        }
      end)
    end)
  end

  # The rule's own recommended fix, already applied: the same condition ANDs in
  # the PR author (`github.event.pull_request.user.login`), which a fork cannot
  # forge. With `&&` and no `||` the spoofable `github.actor` half can only
  # narrow the gate, never open it (panoply / nextgen-typing
  # dependabot-automerge.yml were reported CRITICAL for exactly this shape).
  #
  # Fail-safe: the author equality must be a whole, positive, top-level `&&`
  # conjunct. Any logical `!` (not `!=`) anywhere in the expression keeps the
  # finding, since `!(user.login == 'bot')` admits every non-bot author.
  defp author_pinned_gate?(line, name) do
    author_re =
      ~r/^github\.event\.pull_request\.user\.login\s*==\s*['"]#{Regex.escape(name)}['"]$/

    expr =
      line
      |> String.replace(~r/^\s*(?:-\s*)?if:\s*/, "")
      |> String.replace(~r/\$\{\{|\}\}/, "")

    String.contains?(expr, "&&") and not String.contains?(expr, "||") and
      not Regex.match?(~r/!(?!=)/, expr) and
      expr
      |> String.split("&&")
      |> Enum.map(&strip_wrapping_parens/1)
      |> Enum.any?(&Regex.match?(author_re, &1))
  end

  # Remove whitespace and matched outer parentheses: `( (a == b) )` → `a == b`.
  # Unmatched parentheses are left in place, so the conjunct cannot match.
  defp strip_wrapping_parens(conjunct) do
    trimmed = String.trim(conjunct)

    case Regex.run(~r/^\((.*)\)$/s, trimmed) do
      [_, inner] -> strip_wrapping_parens(inner)
      nil -> trimmed
    end
  end

  # ─── RE009: fromJSON(secrets.X) bypasses runner redaction ────────────

  @doc """
  RE009: `${{ fromJSON(secrets.X) }}` — parses a secret as structured
  JSON. Runner-side redaction operates on the *literal* secret text,
  not on the parsed individual fields; once split, the inner values
  leak to logs unredacted. Provenance: zizmor `unredacted-secrets`.

  Severity: `:critical`. Action: `:report`.
  """
  def re009_fromjson_secret(repo_path) do
    re = ~r/\$\{\{\s*fromJSON\(\s*secrets\.[A-Za-z_][A-Za-z0-9_]*\s*\)/

    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(re, content, return: :index)
      |> Enum.map(fn [{idx, _} | _] ->
        line_no = line_number_for_offset(content, idx)
        slice = byte_preview(content, idx, 60)

        %{
          rule: "RE009",
          file: rel,
          severity: :critical,
          reason:
            "workflow #{rel}:#{line_no} parses a secret as JSON (`#{slice}…`) — " <>
              "individual parsed fields are NOT covered by runner-side log redaction",
          action: :report,
          detail: %{
            line: line_no,
            fix:
              "Store each piece of structured data as a separate secret " <>
                "(`AWS_KEY`, `AWS_SECRET`, etc.) rather than a single JSON blob. " <>
                "If you must store JSON, parse it inside a step rather than via fromJSON()."
          }
        }
      end)
    end)
  end

  # ─── RE010: workflow_run consumes artifact without provenance check ──

  @doc """
  RE010: Workflow_run trigger that calls `actions/download-artifact`
  without specifying a `run-id` constrained to the triggering run,
  AND without subsequent provenance verification. The artifact may
  have been written by a malicious PR workflow that ran in unprivileged
  context but whose output is now being trusted by a privileged run.
  Provenance: EuroS&P 2023 (Kermabon-Bobinnec et al.).

  Severity: `:warn`. Action: `:report`.
  """
  def re010_workflow_run_artifact_no_provenance(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      workflow_run? =
        Regex.match?(~r/^\s*on:\s*workflow_run\b/m, content) or
          Regex.match?(~r/^\s+workflow_run:/m, content)

      downloads_artifact? = Regex.match?(~r/uses:\s*actions\/download-artifact/, content)

      run_id_constrained? =
        Regex.match?(~r/run-id:\s*\$\{\{\s*github\.event\.workflow_run\.id/, content) or
          Regex.match?(~r/run-id:\s*\$\{\{\s*github\.event\.workflow_run\.id\s*\}\}/, content)

      has_provenance_verify? =
        Regex.match?(~r/(?i)\b(cosign|slsa-verifier|gh attestation verify)\b/, content) or
          Regex.match?(~r/(?i)attestations:\s*read/, content)

      if workflow_run? and downloads_artifact? and
           not run_id_constrained? and not has_provenance_verify? do
        [
          %{
            rule: "RE010",
            file: rel,
            severity: :warn,
            reason:
              "workflow #{rel} runs on `workflow_run` and downloads artifacts " <>
                "without constraining `run-id` or verifying provenance — cross-workflow trust crossing",
            action: :report,
            detail: %{
              fix:
                "Either:\n" <>
                  "  1. Set `run-id: ${{ github.event.workflow_run.id }}` and " <>
                  "`github-token: ${{ secrets.GITHUB_TOKEN }}` on the download-artifact step, OR\n" <>
                  "  2. Verify provenance with `gh attestation verify` / `cosign verify` " <>
                  "before consuming the artifact."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── Scan facade ────────────────────────────────────────────────────

  @doc """
  Run every research-extension check on `repo_path` and return the
  standard scan-result map.
  """
  def scan(repo_path, _opts \\ []) do
    findings =
      re001_missing_harden_runner(repo_path) ++
        re002_harden_runner_audit_mode(repo_path) ++
        re003_cache_key_poisoning(repo_path) ++
        re004_container_tag_pin(repo_path) ++
        re005_test_swallows_exit(repo_path) ++
        re006_composite_unpinned_nested(repo_path) ++
        re007_workflow_env_secret_exposure(repo_path) ++
        re008_spoofable_bot_gate(repo_path) ++
        re009_fromjson_secret(repo_path) ++
        re010_workflow_run_artifact_no_provenance(repo_path)

    %{
      findings: findings,
      total: length(findings),
      by_severity: group_by_severity(findings),
      dispatch: dispatch_recommendations(findings)
    }
  end

  # ─── KYAML (flow-style) workflows ───────────────────────────────────

  # Read a workflow for the line- and indent-based rules. A KYAML workflow
  # (YAML-POLICY Y-3: the estate's base form, `yq -o kyaml`) is rewritten to
  # the equivalent block layout one line for one line, so physical line
  # numbers in findings still point at the source. Block YAML is returned
  # byte-identical.
  @doc false
  def read_workflow(path), do: path |> File.read!() |> flow_to_block()

  # Rewrite a KYAML document to block layout, preserving the line count:
  # the root `{` and every closer become blank lines, `key: {`/`key: [`
  # lose their opener, the first key of a `{` sequence item and each scalar
  # sequence item gain `- `, trailing commas go, and plain double-quoted
  # scalars are unquoted. Anything whose first significant line is not a
  # bare `{` is not KYAML and passes through untouched — block YAML's own
  # `on: [push]` and `permissions: {}` are never rewritten.
  @doc false
  def flow_to_block(content) when is_binary(content) do
    lines = String.split(content, "\n")

    if kyaml_root?(lines) do
      {out, _stack, _dash} = Enum.reduce(lines, {[], [], false}, &flow_line/2)
      out |> Enum.reverse() |> Enum.join("\n")
    else
      content
    end
  end

  # True when the first line that is neither blank nor a comment is a bare `{`.
  defp kyaml_root?(lines) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.find(&(&1 != "" and not String.starts_with?(&1, "#")))
    |> Kernel.==("{")
  end

  # Rewrite one KYAML line. The stack holds the open collections (`:map` or
  # `:seq`); `dash` marks that the next property opens a sequence item.
  defp flow_line(line, {out, stack, dash}) do
    trimmed = String.trim(line)

    if trimmed == "" or String.starts_with?(trimmed, "#") do
      {[dedent(line) | out], stack, dash}
    else
      {head, comment} =
        case Regex.run(~r/^(.*?),?(\s+#[^"]*)?$/, String.trim_trailing(line)) do
          [_, head, comment] -> {head, comment}
          [_, head] -> {head, ""}
        end

      indent = String.duplicate(" ", indent_of(head))
      body = String.trim(head)
      in_seq? = match?([:seq | _], stack)

      cond do
        body in ["}", "]"] ->
          {["" | out], Enum.drop(stack, 1), false}

        body == "{" ->
          {["" | out], [:map | stack], in_seq?}

        String.ends_with?(body, ": {") or String.ends_with?(body, ": [") ->
          kind = if String.ends_with?(body, "{"), do: :map, else: :seq
          key = String.slice(body, 0..-3//1)
          {[emit(indent, key, comment, dash) | out], [kind | stack], false}

        in_seq? ->
          item = Regex.replace(~r/^\{\s*(.*?)\s*\}$/, body, "\\1")
          {[emit(indent <> "  ", unquote_scalar(item), comment, true) | out], stack, false}

        true ->
          {[emit(indent, unquote_scalar(body), comment, dash) | out], stack, false}
      end
    end
  end

  # Assemble one block-layout line, two columns left of the KYAML one, with a
  # `- ` sequence marker in the two columns just above the key when `dash`.
  defp emit(indent, body, comment, dash) do
    prefix = if dash, do: String.slice(indent, 2..-1//1) <> "- ", else: indent
    dedent(prefix <> body <> (comment || ""))
  end

  # Drop the two columns of indentation the KYAML root mapping adds.
  defp dedent("  " <> rest), do: rest
  defp dedent(line), do: line

  # Unquote a `key: "value"` or bare `"value"` whose value holds no quote or
  # escape; anything else keeps its quotes.
  defp unquote_scalar(body) do
    Regex.replace(~r/^((?:[^\s"':]+:\s+)?)"([^"\\]*)"$/, body, "\\1\\2")
  end

  # ─── Internals ──────────────────────────────────────────────────────

  # Extract step entries from a workflow's `steps:` blocks. Returns
  # `[{name_or_nil, line_no, body_text}]`. Approximate — a step starts
  # on a `- ` token at the steps-indent and extends until the next
  # `- ` at the same indent or until indent drops below.
  @doc false
  def step_blocks(content) do
    lines =
      content
      |> String.split("\n")
      |> Enum.with_index(1)

    {finished, last} =
      Enum.reduce(lines, {[], nil}, fn {line, no}, {acc, current} ->
        # A new step begins on a `- ` token. We detect both
        # `- name: ...`, `- run: ...`, `- uses: ...` as openers.
        new_step? = Regex.match?(~r/^\s+-\s+(?:name|run|uses):/, line)

        cond do
          new_step? ->
            acc2 = flush_step(acc, current)
            indent = indent_of(line)
            name = extract_step_name(line)
            {acc2, {name, no, [line], indent}}

          current != nil ->
            {n, l, body, start_indent} = current

            cond do
              # blank line: keep accumulating, doesn't terminate
              String.trim(line) == "" ->
                {acc, {n, l, [line | body], start_indent}}

              indent_of(line) > start_indent ->
                # Still inside the step body. If we see a name: line
                # before the name was discovered, capture it.
                n2 =
                  if n == nil do
                    case Regex.run(~r/^\s+name:\s*['"]?([^'"\n]+?)['"]?\s*$/, line) do
                      [_, capture] -> String.trim(capture)
                      _ -> n
                    end
                  else
                    n
                  end

                {acc, {n2, l, [line | body], start_indent}}

              true ->
                # Indent dropped — close out the current step.
                acc2 = flush_step(acc, current)
                {acc2, nil}
            end

          true ->
            {acc, nil}
        end
      end)

    finished
    |> flush_step(last)
    |> Enum.reverse()
  end

  defp flush_step(acc, nil), do: acc

  defp flush_step(acc, {name, line_no, body_lines, _indent}) do
    body = body_lines |> Enum.reverse() |> Enum.join("\n")
    [{name, line_no, body} | acc]
  end

  defp extract_step_name(line) do
    case Regex.run(~r/^\s+-\s+name:\s*['"]?([^'"\n]+?)['"]?\s*$/, line) do
      [_, capture] -> String.trim(capture)
      _ -> nil
    end
  end

  defp indent_of(line) do
    case Regex.run(~r/^(\s*)/, line) do
      [_, ws] -> String.length(ws)
      _ -> 0
    end
  end

  defp line_number_for_offset(content, offset) when offset >= 0 do
    content
    |> binary_part(0, min(offset, byte_size(content)))
    |> String.graphemes()
    |> Enum.count(&(&1 == "\n"))
    |> Kernel.+(1)
  end

  # Fixed-length excerpt starting at a BYTE offset (regex match start, so
  # always on a character boundary). Clamps to the remaining bytes and
  # trims any trailing partial UTF-8 sequence (mirrors
  # WorkflowHardening.byte_preview/3).
  defp byte_preview(content, idx, len) do
    avail = max(byte_size(content) - idx, 0)
    trim_partial_utf8(binary_part(content, idx, min(len, avail)))
  end

  defp trim_partial_utf8(bin) do
    if String.valid?(bin) or byte_size(bin) == 0 do
      bin
    else
      trim_partial_utf8(binary_part(bin, 0, byte_size(bin) - 1))
    end
  end

  defp group_by_severity(findings) do
    findings
    |> Enum.group_by(& &1.severity)
    |> Enum.map(fn {sev, items} -> {sev, length(items)} end)
    |> Map.new()
  end

  defp dispatch_recommendations(findings) do
    Enum.map(findings, fn finding ->
      # Research-extension findings are advisory in nature — fixes
      # vary by defect class (cache-key rewrite vs. step-restructure
      # vs. key-management). Route all to sustainabot.
      confidence =
        case finding.severity do
          :critical -> 0.92
          :high -> 0.85
          :warn -> 0.75
          :info -> 0.60
          _ -> 0.50
        end

      %{
        bot: :sustainabot,
        confidence: confidence,
        rule: finding.rule,
        action: finding.action,
        reason: finding.reason
      }
    end)
  end
end

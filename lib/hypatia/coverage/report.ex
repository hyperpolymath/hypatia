# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.Report do
  @moduledoc """
  The SHADOW "proven vs raw" coverage report (design §3.4, §5.3, §8).

  **Not a gate.** It computes and explains; nothing here blocks anything.

  Inputs (`build/1`):

    * `:raw` — result of `load_coverage/2` for the run of *all* tests
    * `:proven` — the same for the run of *only the counted* tests
    * `:evidence` — result of `load_evidence/1` (or `{:ok, records}`)
    * `:repo` — `"owner/name"`, `:sha` — commit, `:root` — optional checkout dir
    * `:min_cases` — property-test floor (default 100)
    * `:raw_meta`, `:proven_meta` — `{:ok, map}` or `{:error, _}` from `load_meta/1`
      (`Hypatia.Coverage.ReportMeta`); an absent key counts as missing (COV-E013)

  Each of `:raw`, `:proven`, `:evidence` is `{:ok, value}` or
  `{:error, {:missing, path}}` / `{:error, {:parse, path, where}}`.

  ## Outcome

  `UNJUDGED` if any of COV-E001, E002, E003, E005, E007, E011, E012, E013 applies. Otherwise
  `PASS` when proven lines are 100% of the raw run's executable lines and (if
  branches are measurable) proven branches are 100% too, else `FAIL`. That is
  the design's Stage-2 rule applied to a single run; in this increment it is
  only reported. COV-E010 (overclaim) and the "branches not measurable" note
  do not change the outcome.

  Anything not judged shows `null` in the verdict and "not judged" in
  Markdown, never 0% or 100%.

  ## Reason codes

  | Code | Meaning |
  |---|---|
  | COV-E001 | a report or the evidence list is missing |
  | COV-E002 | unparseable (file and location given), or the raw run has no executable lines |
  | COV-E003 | report paths not found under the root (sample shown) |
  | COV-E005 | zero counted evidence |
  | COV-E010 | evidence record claims a kind it cannot back (flagged, not fatal) |
  | COV-E006 | a proof (kind P) record was checked against different source: a covered range changed, moved or is missing (flagged, not counted, not fatal) |
  | COV-E007 | a report's sidecar names a different repo, commit, role or run than the one judged |
  | COV-E011 | proven run covers something the raw run does not (proven must be a subset of raw) |
  | COV-E012 | raw and proven runs were instrumented differently (branch data in one only) |
  | COV-E013 | coverage report is not bound to a commit (no readable metadata sidecar) |
  | COV-E014 | a test ran in the proven run but does not count (proven run must be exactly the counted tests) |

  COV-E012 is raised when exactly one of the raw and proven runs has branch data.

  COV-E004 (branches not measurable) is reported as a note, not applied as a
  verdict reason, in this increment.
  """

  alias Hypatia.Coverage.{Evidence, Facts, LlvmCov, Lcov, Merge, ProofBinding}

  @different "coverage report is from a different repo, commit or run"
  @cap 50
  @sample 5
  @unjudging [
    "COV-E001",
    "COV-E002",
    "COV-E003",
    "COV-E005",
    "COV-E007",
    "COV-E011",
    "COV-E012",
    "COV-E013",
    "COV-E014"
  ]

  # -- loading -------------------------------------------------------------

  @doc """
  Load a coverage report, sniffing the format: JSON (first non-blank char
  `{`) is llvm-cov, anything else is LCOV. Options: `:root`.
  """
  @spec load_coverage(Path.t(), keyword()) ::
          {:ok, Facts.t()} | {:error, {:missing, Path.t()} | {:parse, Path.t(), String.t()}}
  def load_coverage(path, opts \\ []) do
    case File.read(path) do
      {:ok, text} ->
        parsed =
          if String.starts_with?(String.trim_leading(text), "{"),
            do: LlvmCov.parse(text, opts),
            else: Lcov.parse(text, opts)

        case parsed do
          {:ok, facts} -> {:ok, facts}
          {:error, {:parse, where}} -> {:error, {:parse, path, where}}
        end

      {:error, _} ->
        {:error, {:missing, path}}
    end
  end

  @doc "Load the evidence list (see `Hypatia.Coverage.Evidence.load/1`)."
  defdelegate load_evidence(path), to: Evidence, as: :load

  # -- counting ------------------------------------------------------------

  @doc """
  Pure counts for the line and branch universe of `raw`, given `proven`.

  Returns `%{line: %{proven, raw, total}, branch: %{proven, raw, total, measurable}}`.
  `proven` counts only units that raw also lists (the universe is raw's).
  """
  @spec compute(Facts.t(), Facts.t()) :: map()
  def compute(raw, proven) do
    rl = Facts.line_hits(raw)
    pl = Facts.line_hits(proven)
    rb = Facts.branch_hits(raw)
    pb = Facts.branch_hits(proven)

    %{
      line: %{
        proven: count_covered(rl, pl),
        raw: Enum.count(rl, fn {_k, h} -> h > 0 end),
        total: map_size(rl)
      },
      branch: %{
        proven: count_covered(rb, pb),
        raw: Enum.count(rb, fn {_k, h} -> h > 0 end),
        total: map_size(rb),
        measurable: raw.branch_measurable and proven.branch_measurable
      }
    }
  end

  defp count_covered(universe, proven),
    do: Enum.count(universe, fn {k, _} -> Map.get(proven, k, 0) > 0 end)

  @doc "Percentage rounded to one decimal, or nil when the total is 0 or unknown."
  @spec percent(integer() | nil, integer() | nil) :: float() | nil
  def percent(_n, t) when t in [nil, 0], do: nil
  def percent(nil, _t), do: nil
  def percent(n, t), do: Float.round(n * 100 / t, 1)

  # -- verdict -------------------------------------------------------------

  @doc "Build the verdict map (JSON-safe: strings, numbers, booleans, nil, lists, maps)."
  @spec build(map()) :: map()
  def build(input) do
    repo = input.repo
    sha = input.sha
    root = Map.get(input, :root)
    min_cases = Map.get(input, :min_cases, Evidence.default_min_cases())

    {raw, raw_reasons} = take(input.raw, "raw")
    {proven, proven_reasons} = take(input.proven, "proven")
    {records, ev_reasons} = take(input.evidence, "evidence")

    classified =
      if records,
        do:
          Evidence.classify_all(records,
            min_cases: min_cases,
            repo: repo,
            commit: sha,
            root: root
          ),
        else: []

    ev_reasons = ev_reasons ++ evidence_reasons(records, classified)

    # E011 compares the proven RUN with the raw run, so it is judged before proof coverage is
    # added: a proof covers lines without running them, which need not be lines raw ran.
    proven_run = proven
    proven = add_proof_coverage(raw, proven, classified, root)

    raw_reasons = raw_reasons ++ raw_sanity(raw)
    path_reasons = path_reasons(root, raw, proven)
    incons = if raw && proven_run, do: inconsistency(raw, proven_run), else: nil
    incons_reasons = if incons, do: [incons.reason], else: []

    instr_reasons =
      if raw && proven && raw.branch_measurable != proven.branch_measurable,
        do: [
          reason(
            "COV-E012",
            "raw and proven runs were instrumented differently (branch data in one only)",
            %{raw_branches: raw.branch_measurable, proven_branches: proven.branch_measurable}
          )
        ],
        else: []

    meta_reasons = meta_reasons(input, repo, sha)

    reasons =
      raw_reasons ++
        proven_reasons ++
        ev_reasons ++ path_reasons ++ incons_reasons ++ instr_reasons ++ meta_reasons

    unjudged? = Enum.any?(reasons, &(&1.code in @unjudging))

    both? = raw != nil and proven != nil
    counts = if raw && proven, do: compute(raw, proven), else: nil
    raw_ok? = raw != nil and raw_has_lines?(raw)

    {line, branch} = numbers(counts, raw, raw_ok?, unjudged?)

    outcome =
      cond do
        unjudged? -> "UNJUDGED"
        passes?(line, branch) -> "PASS"
        true -> "FAIL"
      end

    notes = notes(branch, raw, proven, root, classified)

    sections =
      if outcome == "UNJUDGED" or not both? do
        empty_sections()
      else
        sections(raw, proven, repo, sha)
      end

    %{
      "schema" => "hypatia.proven-coverage.verdict/1",
      "shadow" => true,
      "repo" => repo,
      "commit" => sha,
      "outcome" => outcome,
      "reasons" => Enum.map(reasons, &stringify/1),
      "line" => line,
      "branch" => branch,
      "sections" => sections,
      "evidence" => evidence_summary(records, classified),
      "notes" => notes
    }
  end

  @doc "Load a report sidecar (see `Hypatia.Coverage.ReportMeta.read/1`)."
  defdelegate load_meta(path), to: Hypatia.Coverage.ReportMeta, as: :read

  defp meta_reasons(input, repo, sha) do
    raw = Map.get(input, :raw_meta)
    proven = Map.get(input, :proven_meta)

    per =
      for {name, m} <- [{"raw", raw}, {"proven", proven}] do
        case m do
          {:ok, %{} = meta} -> binding_reason(name, meta, repo, sha)
          other -> [unbound(name, other)]
        end
      end

    run_ids =
      case {raw, proven} do
        {{:ok, %{"run_id" => a}}, {:ok, %{"run_id" => b}}} when a != b ->
          [
            reason("COV-E007", @different, %{
              input: "raw/proven",
              raw_run_id: a,
              proven_run_id: b
            })
          ]

        _ ->
          []
      end

    List.flatten(per) ++ run_ids
  end

  defp unbound(name, other) do
    detail =
      case other do
        {:error, {kind, path, why}} -> %{input: name, path: path, why: "#{kind}: #{why}"}
        _ -> %{input: name, why: "no metadata supplied"}
      end

    reason("COV-E013", "coverage report is not bound to a commit (no metadata sidecar)", detail)
  end

  defp binding_reason(name, meta, repo, sha) do
    problems =
      [
        String.downcase(to_string(meta["repo"])) != String.downcase(to_string(repo)) &&
          {:repo, meta["repo"], repo},
        String.downcase(to_string(meta["commit"])) != String.downcase(to_string(sha)) &&
          {:commit, meta["commit"], sha},
        meta["role"] != name && {:role, meta["role"], name}
      ]
      |> Enum.filter(& &1)

    if problems == [],
      do: [],
      else: [
        reason("COV-E007", @different, %{
          input: name,
          mismatches: Enum.map(problems, fn {k, got, want} -> "#{k}: #{got} != #{want}" end)
        })
      ]
  end

  # Counted P records (already bound by Evidence) add proven coverage without execution:
  # every raw-executable line, and every raw branch arm on those lines, in each covered range.
  defp add_proof_coverage(raw, proven, classified, root) when raw != nil and proven != nil do
    ranges =
      for {r, :counted} <- classified,
          Evidence.kind(r) == "P",
          {:ok, rs} <- [ProofBinding.bind(r, root)],
          range <- rs,
          do: range

    if ranges == [], do: proven, else: Merge.merge(proven, ProofBinding.coverage(raw, ranges))
  end

  defp add_proof_coverage(_raw, proven, _classified, _root), do: proven

  defp take({:ok, v}, _name), do: {v, []}

  defp take({:error, {:missing, path}}, name),
    do: {nil, [reason("COV-E001", "#{name} input missing: #{path}", %{input: name, path: path})]}

  defp take({:error, {:parse, path, where}}, name),
    do:
      {nil,
       [
         reason("COV-E002", "#{name} input did not parse: #{path} (#{where})", %{
           input: name,
           path: path,
           where: where
         })
       ]}

  defp reason(code, message, detail), do: %{code: code, message: message, detail: detail}

  defp stringify(%{code: c, message: m, detail: d}),
    do: %{"code" => c, "message" => m, "detail" => json_safe(d)}

  defp json_safe(%{} = m), do: Map.new(m, fn {k, v} -> {to_string(k), json_safe(v)} end)
  defp json_safe(l) when is_list(l), do: Enum.map(l, &json_safe/1)
  defp json_safe(v) when is_atom(v) and v not in [nil, true, false], do: Atom.to_string(v)
  defp json_safe(v), do: v

  defp raw_has_lines?(raw), do: map_size(Facts.line_hits(raw)) > 0

  defp raw_sanity(nil), do: []

  defp raw_sanity(raw) do
    if raw_has_lines?(raw),
      do: [],
      else: [
        reason("COV-E002", "raw report parsed but has no executable lines; nothing to judge", %{
          input: "raw",
          where: "no executable lines"
        })
      ]
  end

  defp evidence_reasons(nil, _), do: []

  defp evidence_reasons(_records, classified) do
    counted = Enum.count(classified, &(elem(&1, 1) == :counted))

    zero =
      if counted == 0,
        do: [
          reason(
            "COV-E005",
            "zero counted evidence: #{length(classified)} record(s), none qualified (see 'Why not counted')",
            %{records: length(classified)}
          )
        ],
        else: []

    over = for {r, d} <- classified, Evidence.overclaim?(d), do: {r, d}

    overclaim =
      if over == [],
        do: [],
        else: [
          reason(
            "COV-E010",
            "#{length(over)} record(s) claim a kind they cannot back (overclaim); flagged, not counted",
            %{
              records:
                for(
                  {r, {:not_counted, why}} <- over,
                  do: %{name: Evidence.name(r), kind: Evidence.kind(r), reason: why}
                )
            }
          )
        ]

    # COV-E014: the proven run must be exactly the counted tests. A record that says it
    # ran in the proven run but does not count put unproven coverage into that run.
    intruders =
      for {r, d} <- classified,
          r["in_proven_run"] == true,
          Evidence.kind(r) != "P",
          d != :counted,
          do: Evidence.name(r)

    mixed =
      if intruders == [],
        do: [],
        else: [
          reason(
            "COV-E014",
            "#{length(intruders)} test(s) ran in the proven run but do not count; proven coverage would include their lines",
            %{tests: Enum.take(intruders, 50)}
          )
        ]

    # COV-E006: a proof whose covered source changed since it was checked. Not unjudging:
    # the stale proof simply does not count (like COV-E010).
    stale = for {r, {:not_counted, :proof_stale}} <- classified, do: Evidence.name(r)

    stale_reason =
      if stale == [],
        do: [],
        else: [
          reason(
            "COV-E006",
            "#{length(stale)} proof record(s) were checked against different source (stale); not counted. Re-run the proof checker.",
            %{records: Enum.take(stale, @cap)}
          )
        ]

    zero ++ overclaim ++ stale_reason ++ mixed
  end

  # COV-E003: report paths that do not exist under the root.
  defp path_reasons(nil, _raw, _proven), do: []

  defp path_reasons(root, raw, proven) do
    paths =
      [raw, proven]
      |> Enum.reject(&is_nil/1)
      |> Enum.flat_map(fn f ->
        for {p, d} <- f.files, d.lines != %{} or d.branches != %{}, do: p
      end)
      |> Enum.uniq()
      |> Enum.sort()

    missing = Enum.reject(paths, &under_root_file?(root, &1))

    if missing == [],
      do: [],
      else: [
        reason(
          "COV-E003",
          "#{length(missing)} of #{length(paths)} report path(s) not found under #{root}; sample: " <>
            Enum.join(Enum.take(missing, @sample), ", ") <>
            ". Set the path prefix (--root / collector).",
          %{
            root: root,
            missing: length(missing),
            of: length(paths),
            sample: Enum.take(missing, @sample)
          }
        )
      ]
  end

  # A report path maps only if it stays under the (expanded) root; `..` and absolute paths
  # that escape it count as unmapped.
  defp under_root_file?(root, path), do: ProofBinding.under_root_file?(root, path)

  # COV-E011: proven must be a subset of raw.
  defp inconsistency(raw, proven) do
    lines = MapSet.difference(Facts.covered_lines(proven), Facts.covered_lines(raw))
    branches = MapSet.difference(Facts.covered_branches(proven), Facts.covered_branches(raw))

    if MapSet.size(lines) == 0 and MapSet.size(branches) == 0 do
      nil
    else
      ls = lines |> Enum.sort() |> Enum.take(@sample) |> Enum.map(fn {p, l} -> "#{p}:#{l}" end)

      bs =
        branches
        |> Enum.sort()
        |> Enum.take(@sample)
        |> Enum.map(fn {p, l, _b, _a} -> "#{p}:#{l}" end)

      %{
        reason:
          reason(
            "COV-E011",
            "inconsistent runs: the proven run covers #{MapSet.size(lines)} line(s) and " <>
              "#{MapSet.size(branches)} branch arm(s) that the raw run does not " <>
              "(proven must be a subset of raw). Sample: #{Enum.join(ls ++ bs, ", ")}",
            %{lines: MapSet.size(lines), branches: MapSet.size(branches), sample: ls ++ bs}
          )
      }
    end
  end

  defp numbers(counts, raw, raw_ok?, unjudged?) do
    line = %{"proven" => nil, "raw" => nil, "total" => nil, "measurable" => true}
    no_branch = %{"proven" => nil, "raw" => nil, "total" => nil, "measurable" => false}

    cond do
      not raw_ok? ->
        {line, no_branch}

      counts == nil ->
        # raw alone: raw numbers are true, proven is unknown
        c = compute(raw, Facts.empty())

        {%{line | "raw" => c.line.raw, "total" => c.line.total},
         branch_map(nil, c.branch.raw, c.branch.total, raw.branch_measurable)}

      unjudged? ->
        {%{line | "raw" => counts.line.raw, "total" => counts.line.total},
         branch_map(nil, counts.branch.raw, counts.branch.total, counts.branch.measurable)}

      true ->
        {%{
           line
           | "proven" => counts.line.proven,
             "raw" => counts.line.raw,
             "total" => counts.line.total
         },
         branch_map(
           counts.branch.proven,
           counts.branch.raw,
           counts.branch.total,
           counts.branch.measurable
         )}
    end
  end

  defp branch_map(_p, _r, _t, false),
    do: %{"proven" => nil, "raw" => nil, "total" => nil, "measurable" => false}

  defp branch_map(p, r, t, true),
    do: %{"proven" => p, "raw" => r, "total" => t, "measurable" => true}

  defp passes?(line, branch) do
    line["proven"] == line["total"] and
      (branch["measurable"] != true or branch["proven"] == branch["total"])
  end

  defp notes(branch, raw, proven, root, classified) do
    n = []

    n =
      if branch["measurable"] != true and (raw != nil or proven != nil),
        do: [
          "Branches: not measurable (no branch data in the report; COV-E004 substitution is not applied in this increment)."
          | n
        ],
        else: n

    n =
      if root == nil,
        do: [
          "Paths were not checked against a checkout (no --root), so COV-E003 could not be evaluated."
          | n
        ],
        else: n

    mutation_notes =
      for {r, :counted} <- classified,
          Evidence.kind(r) in ["R", "C", "B"],
          lang = Evidence.language(r),
          lang not in Evidence.mutation_languages(),
          uniq: true,
          do: "mutation: not measurable for #{lang}"

    Enum.reverse(n) ++ Enum.sort(mutation_notes)
  end

  # -- location sections ---------------------------------------------------

  defp empty_sections do
    for k <- ~w(raw_only_lines unrun_lines raw_only_branches unrun_branches), into: %{} do
      {k, %{"items" => [], "total" => 0, "cut" => 0}}
    end
  end

  defp sections(raw, proven, repo, sha) do
    rl = Facts.line_hits(raw)
    pl = Facts.line_hits(proven)
    rb = Facts.branch_hits(raw)
    pb = Facts.branch_hits(proven)

    raw_only_l = for {{p, l} = k, h} <- rl, h > 0, Map.get(pl, k, 0) == 0, do: {p, l}
    unrun_l = for {{p, l}, 0} <- rl, do: {p, l}
    raw_only_b = for {k, h} <- rb, h > 0, Map.get(pb, k, 0) == 0, do: k
    unrun_b = for {k, 0} <- rb, do: k

    %{
      "raw_only_lines" => section(raw_only_l, repo, sha),
      "unrun_lines" => section(unrun_l, repo, sha),
      "raw_only_branches" => arm_section(raw_only_b, repo, sha),
      "unrun_branches" => arm_section(unrun_b, repo, sha)
    }
  end

  defp section(locs, repo, sha) do
    ranges = ranges(locs)
    shown = Enum.take(ranges, @cap)

    %{
      "items" =>
        Enum.map(shown, fn {p, a, b} ->
          %{"path" => p, "from" => a, "to" => b, "url" => permalink(repo, sha, p, a, b)}
        end),
      "total" => length(ranges),
      "cut" => max(length(ranges) - @cap, 0)
    }
  end

  # One entry per branch arm: two uncovered arms on one line are two entries (and count twice
  # in `total` and in the cap), both linking to the shared line.
  defp arm_section(arms, repo, sha) do
    sorted = arms |> Enum.uniq() |> Enum.sort()

    %{
      "items" =>
        sorted
        |> Enum.take(@cap)
        |> Enum.map(fn {p, l, b, a} ->
          %{
            "path" => p,
            "from" => l,
            "to" => l,
            "block" => b,
            "arm" => a,
            "url" => permalink(repo, sha, p, l, l)
          }
        end),
      "total" => length(sorted),
      "cut" => max(length(sorted) - @cap, 0)
    }
  end

  @doc "Group `{path, line}` pairs into sorted `{path, from, to}` ranges of consecutive lines."
  @spec ranges([{String.t(), pos_integer()}]) :: [{String.t(), pos_integer(), pos_integer()}]
  def ranges(locs) do
    locs
    |> Enum.uniq()
    |> Enum.sort()
    |> Enum.chunk_while(
      nil,
      fn
        {p, l}, nil -> {:cont, {p, l, l}}
        {p, l}, {p, a, b} when l == b + 1 -> {:cont, {p, a, l}}
        {p, l}, acc -> {:cont, acc, {p, l, l}}
      end,
      fn
        nil -> {:cont, nil}
        acc -> {:cont, acc, nil}
      end
    )
  end

  @doc "GitHub permalink pinned to the commit (design §5.5)."
  @spec permalink(String.t(), String.t(), String.t(), pos_integer(), pos_integer()) :: String.t()
  def permalink(repo, sha, path, a, a),
    do: "https://github.com/#{repo}/blob/#{sha}/#{enc(path)}#L#{a}"

  def permalink(repo, sha, path, a, b),
    do: "https://github.com/#{repo}/blob/#{sha}/#{enc(path)}#L#{a}-L#{b}"

  defp enc(path), do: URI.encode(path, &(URI.char_unreserved?(&1) or &1 == ?/))

  # -- evidence summary ----------------------------------------------------

  defp evidence_summary(nil, _), do: %{"total" => nil, "counted" => nil, "not_counted" => []}

  defp evidence_summary(_records, classified) do
    not_counted =
      for {r, {:not_counted, why} = d} <- classified do
        %{
          "name" => Evidence.name(r),
          "kind" => Evidence.kind(r),
          "reason" => Atom.to_string(why),
          "text" => Evidence.reason_text(why),
          "overclaim" => Evidence.overclaim?(d)
        }
      end

    %{
      "total" => length(classified),
      "counted" => Enum.count(classified, &(elem(&1, 1) == :counted)),
      "not_counted" => not_counted
    }
  end

  # -- rendering -----------------------------------------------------------

  @doc "Render the verdict as Markdown (design §5.3 layout). Labelled SHADOW."
  @spec to_markdown(map()) :: String.t()
  def to_markdown(v) do
    short = String.slice(v["commit"] || "", 0, 7)
    l = v["line"]
    b = v["branch"]

    [
      "## Hypatia coverage — #{v["outcome"]}  (commit #{short})\n\n",
      "> **SHADOW — not a gate.** Computed beside Codecov for comparison; nothing is blocked or enforced.\n\n",
      "| | proven | raw |\n|---|---|---|\n",
      "| Lines | #{cell(l["proven"], l["total"])} | #{cell(l["raw"], l["total"])} |\n",
      "| Branches | #{branch_cell(b, "proven")} | #{branch_cell(b, "raw")} |\n\n",
      "_Patch numbers and change against the base commit are not part of this increment._\n\n",
      reasons_md(v["reasons"]),
      section_md(
        "Lines run only by unproven tests (raw only)",
        v["sections"]["raw_only_lines"],
        v
      ),
      section_md("Lines not run by any test", v["sections"]["unrun_lines"], v),
      section_md(
        "Branch arms run only by unproven tests (raw only)",
        v["sections"]["raw_only_branches"],
        v
      ),
      section_md("Branch arms not run by any test", v["sections"]["unrun_branches"], v),
      why_md(v["evidence"]),
      notes_md(v["notes"]),
      "Evidence base: #{v["repo"]}@#{short} · hypatia.proven-coverage.evidence/1\n"
    ]
    |> IO.iodata_to_binary()
  end

  defp cell(nil, _t), do: "not judged"
  defp cell(_n, nil), do: "not judged"
  defp cell(n, 0), do: "#{n}/0 (n/a)"
  defp cell(n, t), do: "#{n}/#{t} (#{percent(n, t)}%)"

  defp branch_cell(%{"measurable" => false}, _k), do: "not measurable"
  defp branch_cell(b, k), do: cell(b[k], b["total"])

  defp reasons_md([]), do: ""

  defp reasons_md(reasons) do
    ["### Reasons\n\n", Enum.map(reasons, &"- **#{&1["code"]}** #{&1["message"]}\n"), "\n"]
  end

  defp section_md(_title, %{"items" => []}, _v), do: ""

  defp section_md(title, %{"items" => items, "total" => total, "cut" => cut}, _v) do
    [
      "### #{title} (#{total})\n\n",
      Enum.map(items, fn i ->
        label =
          if i["from"] == i["to"],
            do: "#{i["path"]}#L#{i["from"]}",
            else: "#{i["path"]}#L#{i["from"]}-L#{i["to"]}"

        label = if i["arm"], do: "#{label} (block #{i["block"]} arm #{i["arm"]})", else: label

        "- [#{label}](#{i["url"]})\n"
      end),
      if(cut > 0, do: "- … #{cut} more not shown (cap #{@cap})\n", else: ""),
      "\n"
    ]
  end

  defp why_md(%{"total" => nil}), do: "### Why not counted\n\nevidence list not available.\n\n"

  defp why_md(%{"not_counted" => []} = e),
    do: "### Why not counted (0)\n\nAll #{e["total"]} evidence record(s) counted.\n\n"

  defp why_md(%{"not_counted" => nc, "total" => total, "counted" => counted}) do
    shown = Enum.take(nc, @cap)

    [
      "### Why not counted (#{length(nc)} of #{total}; #{counted} counted)\n\n",
      Enum.map(shown, fn r ->
        flag = if r["overclaim"], do: " [COV-E010]", else: ""
        "- `#{r["name"]}` (kind #{r["kind"] || "?"}): #{r["text"]}#{flag}\n"
      end),
      if(length(nc) > @cap,
        do: "- … #{length(nc) - @cap} more not shown (cap #{@cap})\n",
        else: ""
      ),
      "\n"
    ]
  end

  defp notes_md([]), do: ""
  defp notes_md(notes), do: ["### Notes\n\n", Enum.map(notes, &"- #{&1}\n"), "\n"]

  @doc "Render the verdict as pretty JSON."
  @spec to_json(map()) :: String.t()
  def to_json(v), do: Jason.encode!(v, pretty: true)

  @doc "Exit code convention (design §8.1): 0 judged pass, 1 judged fail, 2 unjudged."
  @spec exit_code(map()) :: 0 | 1 | 2
  def exit_code(%{"outcome" => "PASS"}), do: 0
  def exit_code(%{"outcome" => "FAIL"}), do: 1
  def exit_code(%{"outcome" => "UNJUDGED"}), do: 2
end

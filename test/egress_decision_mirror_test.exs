# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.EgressDecisionMirrorTest do
  @moduledoc """
  Holds RE001 to the decision proved in `src/Hypatia/ABI/Egress.idr`.

  `decClean` there decides whether RE001 accepts a secret-bearing job with a
  deed declaration (or with none). `clean?/2` below is that function written
  out clause for clause. Every case of a small domain is generated as a real
  workflow and deed, RE001 is run on it, and RE001 must report nothing exactly
  when `clean?/2` says the job is clean:

    * the job reads one or two secrets, by name, and possibly also by a key
      computed at run time;
    * it has no hardener, or harden-runner with `egress-policy` unset,
      `audit` or `block`;
    * it is undeclared, or declared with any non-empty subset of the two
      secrets (egress.abnf refuses an empty list) and each of the three
      enforcements.

  Endpoints are outside the model, so the cases name no host and declare
  none.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ResearchExtensions

  @secrets ["ALPHA", "BETA"]

  # ─── Egress.idr's decClean, clause for clause ──────────────────────

  # decClean (MkJob s dyn (Just p)) Nothing = Yes Hardened
  # decClean (MkJob s dyn Nothing)  Nothing = No
  defp clean?(job, nil), do: job.hardener != nil
  # decClean (MkJob s True h) (Just _) = No
  defp clean?(%{dynamic: true}, _decl), do: false
  # decClean (MkJob s False h) (Just (MkDecl ds e)) = decSame s ds && decHolds e h
  defp clean?(job, decl),
    do:
      MapSet.new(job.secrets) == MapSet.new(decl.secrets) and
        holds?(decl.enforcement, job.hardener)

  # decHolds: None holds of anything; HardenRunnerAudit of any hardener;
  # HardenRunnerBlock only of a hardener whose policy is Block.
  defp holds?(:none, _), do: true
  defp holds?(:audit, hardener), do: hardener != nil
  defp holds?(:block, hardener), do: hardener == :block

  # ─── The domain ──────────────────────────────────────────────────

  defp subsets(xs), do: Enum.reduce(xs, [[]], fn x, acc -> acc ++ Enum.map(acc, &(&1 ++ [x])) end)

  defp jobs do
    for secrets <- subsets(@secrets),
        secrets != [],
        dynamic <- [false, true],
        hardener <- [nil, :default, :audit, :block],
        do: %{secrets: secrets, dynamic: dynamic, hardener: hardener}
  end

  defp decls do
    [nil] ++
      for secrets <- subsets(@secrets),
          # egress.abnf refuses an empty `:secrets ()`.
          secrets != [],
          enforcement <- [:none, :audit, :block],
          do: %{secrets: secrets, enforcement: enforcement}
  end

  # harden-runner's default policy is audit (Egress.idr's Policy).
  defp policy(:default), do: :audit
  defp policy(other), do: other

  # ─── The case as files ───────────────────────────────────────────

  defp workflow(job) do
    hardener =
      case job.hardener do
        nil ->
          []

        :default ->
          ["      - uses: step-security/harden-runner@v2"]

        p ->
          [
            "      - uses: step-security/harden-runner@v2",
            "        with:",
            "          egress-policy: #{p}"
          ]
      end

    reads = Enum.map(job.secrets, &"      - run: send ${{ secrets.#{&1} }}")
    dynamic = if job.dynamic, do: ["      - run: send ${{ secrets[env.NAME] }}"], else: []

    Enum.join(
      ["jobs:", "  notify:", "    runs-on: ubuntu-24.04", "    steps:"] ++
        hardener ++ reads ++ dynamic,
      "\n"
    ) <> "\n"
  end

  @enforcement %{none: "none", audit: "harden-runner-audit", block: "harden-runner-block"}

  defp deed(nil), do: ""

  defp deed(decl) do
    secrets = Enum.map_join(decl.secrets, " ", &~s|"#{&1}"|)

    ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
      ~s|:secrets (#{secrets}) :endpoints () :enforcement #{@enforcement[decl.enforcement]}))|
  end

  defp re001(job, decl, n) do
    repo =
      Path.join(System.tmp_dir!(), "egress_mirror_#{System.unique_integer([:positive])}_#{n}")

    File.rm_rf!(repo)
    File.mkdir_p!(Path.join([repo, ".github", "workflows"]))
    File.write!(Path.join([repo, ".github", "workflows", "notify.yml"]), workflow(job))

    File.write!(Path.join(repo, "x_chora.deed"), """
    ;; SPDX-License-Identifier: CC-BY-SA-4.0
    (repo-deed :schema-version "1.0.0" :canonical-name "x"
      :repo-uuid #u5"github.com/o/x" :beholding-chora #u5"estate/chora"
      #{deed(decl)})
    """)

    try do
      # The deed must say what the case says, or the comparison is vacuous.
      declared =
        case Hypatia.Deed.egress_declarations(repo) do
          {:ok, _, jobs} -> length(jobs)
          :none -> 0
        end

      assert declared == if(decl, do: 1, else: 0)
      ResearchExtensions.re001_missing_harden_runner(repo)
    after
      File.rm_rf!(repo)
    end
  end

  test "RE001 reports a job exactly when Egress.idr's decClean refuses it" do
    cases = for job <- jobs(), decl <- decls(), do: {job, decl}
    # 3 secret sets x 2 x 4 hardeners, each with no declaration or 3 x 3.
    assert length(cases) == 24 * 10

    mismatches =
      cases
      |> Enum.with_index()
      |> Enum.flat_map(fn {{job, decl}, n} ->
        model_job = %{job | hardener: job.hardener && policy(job.hardener)}
        expected_clean = clean?(model_job, decl)
        findings = re001(job, decl, n)

        if findings == [] == expected_clean,
          do: [],
          else: [{job, decl, expected_clean, Enum.map(findings, & &1.reason)}]
      end)

    assert mismatches == []

    # Both verdicts occur, so neither side is constant.
    verdicts =
      for {job, decl} <- cases,
          do: clean?(%{job | hardener: job.hardener && policy(job.hardener)}, decl)

    assert Enum.count(verdicts, & &1) > 0 and Enum.count(verdicts, &(!&1)) > 0
  end
end

# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Rules.AlwaysReadsCancelledNeedsTest do
  # WF033 (CASE-066, Apodeixis.jl#28). Owner ruling 2026-11-14: advisory info.
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowAudit

  defp check(files), do: WorkflowAudit.check_always_reads_cancelled_needs(files)

  # Shape of rsr-template-repo d183b32 fork-governance.yml `verdict`.
  @verdict """
  on: pull_request
  concurrency:
    group: ${{ github.workflow }}-${{ github.ref }}
    cancel-in-progress: true
  jobs:
    deed:
      runs-on: ubuntu-24.04
      steps:
        - run: echo deed
    k9:
      runs-on: ubuntu-24.04
      steps:
        - run: echo k9
    verdict:
      name: Fork governance verdict
      needs: [deed, k9]
      if: always()
      runs-on: ubuntu-24.04
      env:
        DEED_RESULT: ${{ needs.deed.result }}
        K9_RESULT: ${{ needs.k9.result }}
      steps:
        - run: |
            [ "$DEED_RESULT" = success ] && [ "$K9_RESULT" = success ] || exit 1
  """

  test "the template's verdict job is reported as advisory info" do
    assert [
             %{
               rule: "WF033",
               job: "verdict",
               severity: :info,
               advisory: true,
               action: :report
             }
           ] = check(%{"fork-governance.yml" => @verdict})
  end

  test "a !cancelled() guard is clean" do
    wf = String.replace(@verdict, "if: always()", "if: ${{ always() && !cancelled() }}")
    assert check(%{"f.yml" => wf}) == []
  end

  test "a job that handles cancelled itself is left alone" do
    wf =
      String.replace(
        @verdict,
        "|| exit 1",
        "|| { [ \"$DEED_RESULT\" = cancelled ] && exit 0; exit 1; }"
      )

    assert check(%{"f.yml" => wf}) == []
  end

  test "an always() job that never reads needs results is not reported" do
    wf =
      @verdict
      |> String.replace("DEED_RESULT: ${{ needs.deed.result }}", "DEED_RESULT: x")
      |> String.replace("K9_RESULT: ${{ needs.k9.result }}", "K9_RESULT: y")

    assert check(%{"f.yml" => wf}) == []
  end

  test "a result read inside a step's if: counts" do
    wf = """
    jobs:
      a:
        runs-on: ubuntu-24.04
        steps:
          - run: echo a
      b:
        needs: a
        if: always()
        runs-on: ubuntu-24.04
        steps:
          - if: needs.a.result != 'success'
            run: exit 1
    """

    assert [%{rule: "WF033", job: "b"}] = check(%{"b.yml" => wf})
  end

  test "continue-on-error: true on the job cannot turn it red" do
    wf =
      String.replace(
        @verdict,
        "  if: always()\n",
        "  if: always()\n    continue-on-error: true\n"
      )

    assert check(%{"f.yml" => wf}) == []
  end

  # CodeRabbit on #1105: a success conjunct protects only its own need.
  test "a success guard on one need does not cover a cancelled sibling" do
    wf =
      String.replace(
        @verdict,
        "if: always()",
        "if: always() && needs.deed.result == 'success'"
      )

    assert [%{rule: "WF033", job: "verdict"}] = check(%{"f.yml" => wf})
  end

  test "success guards on every need are a guard" do
    wf =
      String.replace(
        @verdict,
        "if: always()",
        "if: always() && needs.deed.result == 'success' && needs.k9.result == 'success'"
      )

    assert check(%{"f.yml" => wf}) == []
  end

  # CodeRabbit on #1105: `::error` is an annotation, not a failure.
  test "an ::error annotation alone cannot turn the job red" do
    wf =
      String.replace(@verdict, "|| exit 1", "|| echo \"::error::lanes $DEED_RESULT $K9_RESULT\"")

    assert check(%{"f.yml" => wf}) == []
  end

  # CodeRabbit on #1105: a tolerated step cannot fail the job.
  test "continue-on-error on the only failing step cannot turn the job red" do
    wf =
      String.replace(
        @verdict,
        "      - run: |\n",
        "      - continue-on-error: true\n        run: |\n"
      )

    assert check(%{"f.yml" => wf}) == []
  end

  test "a job that only prints its needs' results cannot turn red" do
    wf = String.replace(@verdict, "|| exit 1", "|| echo \"some lane did not pass\"")
    assert check(%{"f.yml" => wf}) == []
  end

  test "it is part of the full audit" do
    %{findings: fs} =
      WorkflowAudit.audit(["fork-governance.yml"], %{"fork-governance.yml" => @verdict})

    assert Enum.any?(fs, &(Map.get(&1, :rule) == "WF033"))
  end
end

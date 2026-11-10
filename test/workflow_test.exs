# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.WorkflowTest do
  use ExUnit.Case, async: true

  alias Hypatia.Workflow
  alias Hypatia.Workflow.{Job, Step}

  @block """
  name: ci
  on:
    push:
      branches: [main]
    pull_request: {}
  env:
    GLOBAL: ${{ secrets.TOP }}
  permissions:
    contents: read
  jobs:
    build:
      runs-on: ubuntu-24.04
      if: github.event_name == 'push'
      env:
        KEY: ${{ secrets.API_KEY }}
      steps:
        - uses: actions/checkout@v4
          with:
            ref: main
        - name: build
          run: >-
            make
            all
          env:
            LOCAL: x
          if: success()
    call:
      uses: o/r/.github/workflows/w.yml@v1
  """

  @flow """
  {
    name: "ci",
    on: {push: {branches: ["main"]}, pull_request: {}},
    env: {GLOBAL: "${{ secrets.TOP }}"},
    permissions: {contents: "read"},
    jobs: {
      build: {
        runs-on: "ubuntu-24.04",
        if: "github.event_name == 'push'",
        env: {KEY: "${{ secrets.API_KEY }}"},
        steps: [
          {uses: "actions/checkout@v4", with: {ref: "main"}},
          {name: "build", run: "make all", env: {LOCAL: "x"}, if: "success()"},
        ],
      },
      call: {uses: "o/r/.github/workflows/w.yml@v1"},
    },
  }
  """

  test "reads a workflow's jobs and steps, with values as YAML gives them" do
    assert {:ok, %Workflow{path: "ci.yml"} = wf} = Workflow.parse(@block, "ci.yml")
    assert wf.triggers == ["push", "pull_request"]
    assert [{"GLOBAL", _}] = for(e <- Workflow.entries(wf.env), do: {e.key.value, e})
    assert Workflow.text(Hypatia.Kyaml.get(wf.permissions, "contents")) == "read"

    assert [%Job{id: "build"} = build, %Job{id: "call"} = call] = wf.jobs
    assert build.line == 11
    assert Workflow.text(build.runs_on) == "ubuntu-24.04"
    assert build.if == "github.event_name == 'push'"
    assert call.uses == "o/r/.github/workflows/w.yml@v1"
    assert call.runs_on == nil

    assert [%Step{} = checkout, %Step{} = make] = build.steps
    assert checkout.uses == "actions/checkout@v4"
    assert Workflow.input(checkout, "ref") == "main"
    assert checkout.run == nil
    assert {checkout.line, make.line} == {17, 20}
    # A folded block scalar is folded and chomped: `>-` joins the lines.
    assert make.run == "make all"
    assert make.run_style == :block
    assert make.if == "success()"
    assert [%{key: %{value: "LOCAL"}}] = Workflow.entries(make.env)
  end

  test "block and flow layouts give the same model, apart from lines" do
    assert {:ok, block} = Workflow.parse(@block)
    assert {:ok, flow} = Workflow.parse(@flow)
    assert shape(block) == shape(flow)
  end

  defp shape(%Workflow{} = wf) do
    {wf.triggers, env_names(wf.env),
     for job <- wf.jobs do
       {job.id, Workflow.text(job.runs_on), job.uses, job.if, env_names(job.env),
        for step <- job.steps do
          {step.uses, step.run, step.if, Workflow.input(step, "ref"), env_names(step.env)}
        end}
     end}
  end

  defp env_names(env), do: for(e <- Workflow.entries(env), do: e.key.value)

  test "every string under a node keeps its own source line, through block scalars and aliases" do
    yaml = """
    x-script: &script |
      echo one
      echo ${{ secrets.TWO }}
    jobs:
      a:
        runs-on: ubuntu-24.04
        steps:
          - run: *script
    """

    assert {:ok, wf} = Workflow.parse(yaml)
    [%Job{steps: [step]}] = wf.jobs
    # The step's run is the anchored script, read through the alias...
    assert step.run == "echo one\necho ${{ secrets.TWO }}\n"
    # ...and every byte of it is reported at the alias, line 8.
    assert [{value, line_of}] = Workflow.strings(step.node)
    assert value == step.run
    assert line_of.(0) == 8 and line_of.(byte_size(value) - 1) == 8

    # The same scalar read where it is written maps each line to its own.
    anchor = Hypatia.Kyaml.get(wf.doc, "x-script")
    assert [{_, own_line}] = Workflow.strings(anchor)
    assert own_line.(0) == 2
    assert own_line.(byte_size("echo one\n")) == 3
  end

  test "`on:` is read as events in its three forms" do
    for {on, events} <- [
          {"push", ["push"]},
          {"[push, release]", ["push", "release"]},
          {"{workflow_dispatch: {}, schedule: [{cron: '0 0 * * *'}]}",
           ["workflow_dispatch", "schedule"]}
        ] do
      assert {:ok, wf} = Workflow.parse("on: #{on}\njobs: {}\n")
      assert wf.triggers == events, on
    end
  end

  test "an empty stream is no workflow" do
    assert Workflow.parse("") == {:ok, nil}
    assert Workflow.parse("# only a comment\n") == {:ok, nil}
  end

  test "what the reader refuses is an error at its location, never a guess" do
    merge = """
    x: &d
      runs-on: ubuntu-24.04
    jobs:
      a:
        <<: [*d]
    """

    assert {:error, %Hypatia.Kyaml.Error{code: "UNSUPPORTED_MERGE_KEY", line: 5}} =
             Workflow.parse(merge)

    # The anchor bomb: past kyq's node budget the workflow is refused whole.
    bomb =
      ["a: &a [x, x, x, x, x, x, x, x, x, x]"] ++
        for(
          i <- ?b..?h,
          do: "#{<<i>>}: &#{<<i>>} [#{List.duplicate("*#{<<i - 1>>}", 10) |> Enum.join(", ")}]"
        ) ++
        ["jobs: {}"]

    assert {:error, %Hypatia.Kyaml.Error{code: "NODE_BUDGET_EXCEEDED", line: 1}} =
             Workflow.parse(Enum.join(bomb, "\n") <> "\n")
  end
end

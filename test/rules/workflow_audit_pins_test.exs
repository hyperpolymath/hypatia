# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.WorkflowAuditPinsTest do
  use ExUnit.Case, async: true
  alias Hypatia.Rules.WorkflowAudit

  # The two anchors of the 2026-09-29..10-14 incident (plasma-parser-writer#105):
  # the SHA standards' rust-ci-reusable.yml pinned dtolnay/rust-toolchain at,
  # and the standards revision plasma was pinned to.
  @toolchain_sha "6c977a6ca4077a0ceb28ffbe03f59d46e9ac8772"
  @broken_standards "d135b05bfc647d0c0fbfedc7e80f37ea50f49236"
  @good_standards "75bd5994470661a099df562d6d5ad2fe744dbd66"
  @rust_ci "hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml"

  # A SHA-pinned toolchain step with no inputs, for a `with:` to follow.
  @rust_step "jobs:\n  b:\n    steps:\n      - uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n"

  defp wf026(wf), do: WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

  describe "check_sha_pinned_toolchain_without_input/1 (WF026)" do
    test "fires on a SHA-pinned step with no with: block at all" do
      wf = """
      jobs:
        check:
          steps:
            - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
            - run: cargo check --locked
      """

      [f] = WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
      assert f.rule == "WF026"
      # Advisory: below the CLI's default `--severity medium`, so a scan
      # reports and routes it without failing (AGENTIC must-not-promote-to-blocking).
      assert f.severity == :low
      assert f.advisory == true
      assert f.file == "rust.yml"
      assert f.line == 5
      assert f.current_sha == @toolchain_sha
      assert f.auto_fixable
      assert f.fix_script == "fix-rust-toolchain-input.sh"
      assert f.reason =~ "'toolchain' is a required input"
    end

    test "fires when the with: block names components but no toolchain" do
      wf = """
      jobs:
        check:
          steps:
            - name: Install Rust toolchain
              uses: dtolnay/rust-toolchain@#{@toolchain_sha} # stable
              with:
                components: clippy, rustfmt
            - run: cargo check
      """

      assert [%{rule: "WF026", line: 5}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "silent when the step passes a toolchain" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain: stable
                components: clippy
      """

      assert WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf}) == []
    end

    test "silent on a tag-pinned step: the ref carries the channel" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@stable
              with:
                components: clippy
            - uses: dtolnay/rust-toolchain@1.85.0
      """

      assert WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf}) == []
    end

    test "a later step's toolchain: does not rescue an earlier bare step" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain: stable
      """

      assert [%{line: 4}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a shallower key ends the step: a job-level toolchain env is not an input" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
          env:
            toolchain: stable
      """

      assert [%{line: 4}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "silent when with: is written above uses: (YAML keys carry no order)" do
      wf = """
      jobs:
        check:
          steps:
            - with:
                toolchain: stable
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      assert [] = WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "silent on a flow with: that names a toolchain" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { toolchain: stable, components: clippy }
      """

      assert [] = WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "fires on a flow with: without a toolchain" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { components: clippy }
      """

      assert [%{rule: "WF026", line: 4}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "env.toolchain is not an input, below or above uses:" do
      wf = """
      jobs:
        check:
          steps:
            - name: toolchain via env only
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              env:
                toolchain: stable
            - env:
                toolchain: beta
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                components: clippy
      """

      assert [%{line: 5}, %{line: 10}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a quoted \"uses\" key is the same key: it fires like the bare one" do
      wf = """
      jobs:
        check:
          steps:
            - "uses": "dtolnay/rust-toolchain@#{@toolchain_sha}"
      """

      assert [%{rule: "WF026", line: 4}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "the same text inside a run: | script or under env: is not the step's action" do
      wf = """
      jobs:
        check:
          steps:
            - name: write a fixture
              run: |
                cat > fixture.yml <<'EOF'
                uses: dtolnay/rust-toolchain@#{@toolchain_sha}
                EOF
            - uses: dtolnay/rust-toolchain@stable
              env:
                uses: dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      assert [] = WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a bare - opener is a step: with: above uses: rescues it, a bare uses: still fires" do
      wf = """
      jobs:
        check:
          steps:
            -
              with:
                toolchain: nightly
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
            -
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      assert [%{line: 9}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "toolchain: inside another input's block scalar is text, not an input: still fires" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                components: |
                  toolchain: stable
      """

      assert [%{rule: "WF026", line: 4}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a flow with: spread over several lines is read to its closing brace" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: {
                toolchain: nightly,
              }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: {
                components: clippy,
              }
      """

      assert [%{line: 8}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "an expression's braces inside a multi-line flow with: do not close it early" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: {
                components: ${{ matrix.component }},
                toolchain: nightly,
              }
      """

      assert [] = WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a list item is a step only under steps: — fixtures in env: and run: | are text" do
      wf = """
      jobs:
        check:
          env:
            FIXTURE: |
              jobs:
                a:
                  steps:
                    - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
          steps:
            - run: |
                - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      assert [%{line: 12}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a KYAML workflow is judged as its block equivalent, line for line" do
      # The literal shape of `yq -o kyaml` (standards YAML-POLICY Y-3): every
      # value quoted, every mapping braced, step items opening with `{`.
      wf = """
      {
        jobs: {
          check: {
            runs-on: "ubuntu-latest",
            steps: [
              {
                uses: "dtolnay/rust-toolchain@#{@toolchain_sha}",
                with: {
                  toolchain: "stable",
                },
              },
              {
                uses: "dtolnay/rust-toolchain@#{@toolchain_sha}",
              },
              { uses: "dtolnay/rust-toolchain@#{@toolchain_sha}" },
            ],
          },
        },
      }
      """

      assert [%{line: 13} = a, %{line: 15} = b] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      # The repair script rewrites block-style steps only: a KYAML finding is
      # real but routes to review, never to the fixer.
      refute a.auto_fixable
      assert a.fix_script == nil
      assert a.reason =~ "KYAML"
      refute b.auto_fixable
    end

    test "a flow-style step inline in a block document is read through its braces" do
      wf = """
      jobs:
        check:
          steps:
            - { uses: dtolnay/rust-toolchain@#{@toolchain_sha}, with: { toolchain: stable } }
            - { name: "rust", uses: "dtolnay/rust-toolchain@#{@toolchain_sha}" }
            - { with: { components: "clippy, rustfmt" }, uses: dtolnay/rust-toolchain@#{@toolchain_sha} }
            - run: echo "{ uses: dtolnay/rust-toolchain@#{@toolchain_sha} }"
      """

      assert [%{line: 5} = a, %{line: 6}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      refute a.auto_fixable
      assert a.reason =~ "flow style"
    end

    test "steps: [ { uses: ... } ] on one line is a step; a flow list in env: is not" do
      wf = """
      jobs:
        check:
          env:
            fixture: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ]
          steps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ]
      """

      assert [%{line: 5, auto_fixable: false}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "the action is matched in either case; a spelling the fixer cannot match routes to review" do
      wf = """
      jobs:
        check:
          steps:
            - uses: Dtolnay/Rust-Toolchain@#{@toolchain_sha}
      """

      assert [%{rule: "WF026", line: 4, auto_fixable: false, fix_script: nil, reason: reason}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      assert reason =~ "not spelled"
    end

    test "a workflow written in flow style on one line is read through its keys" do
      sha2 = String.duplicate("a", 40)

      docs = %{
        "jobs.yml" =>
          "jobs: { build: { steps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ] } }\n",
        "root.yml" =>
          "{ on: push, jobs: { build: { runs-on: ubuntu-latest, steps: [ " <>
            "{ uses: dtolnay/rust-toolchain@#{@toolchain_sha}, with: { toolchain: stable } }, " <>
            "{ name: \"CI, build\", uses: dtolnay/rust-toolchain@#{sha2} } ] } } }\n",
        "jobline.yml" =>
          "jobs:\n  build: { steps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ] }\n"
      }

      findings = WorkflowAudit.check_sha_pinned_toolchain_without_input(docs)

      assert [%{line: 1, auto_fixable: false}] = Enum.filter(findings, &(&1.file == "jobs.yml"))

      assert [%{line: 1, current_sha: ^sha2, auto_fixable: false}] =
               Enum.filter(findings, &(&1.file == "root.yml"))

      assert [%{line: 2, auto_fixable: false}] =
               Enum.filter(findings, &(&1.file == "jobline.yml"))
    end

    test "a one-line flow workflow inside a run heredoc is not a toolchain step" do
      for opener <- ["", "{ "], closer = if(opener == "", do: "", else: " }") do
        wf = """
        jobs:
          build:
            steps:
              - run: |
                  cat <<'YAML'
                  #{opener}jobs: { fixture: { steps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ] } }#{closer}
                  YAML
        """

        assert WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"x.yml" => wf}) == []
      end
    end

    test "a one-line flow path that is not steps in a job in jobs is not a step" do
      docs = %{
        "env.yml" =>
          "env: { build: { steps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ] } }\n",
        "fixture.yml" =>
          "jobs: { build: { fixture: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ] } }\n",
        "quoted.yml" =>
          "jobs: { build: { steps: [ { run: \"jobs: { b: { steps: [ { uses: " <>
            "dtolnay/rust-toolchain@#{@toolchain_sha} } ] } }\" } ] } }\n"
      }

      assert WorkflowAudit.check_sha_pinned_toolchain_without_input(docs) == []
    end

    test "a - { item spread over lines: its with entry decides, nested braces respected" do
      wf = """
      jobs:
        check:
          steps:
            - {
                uses: dtolnay/rust-toolchain@#{@toolchain_sha},
                with: { components: "${{ matrix.component }}", toolchain: nightly },
              }
            - {
                uses: dtolnay/rust-toolchain@#{@toolchain_sha},
                with: { components: "${{ matrix.component }}" },
              }
            - {
                env: { uses: dtolnay/rust-toolchain@#{@toolchain_sha} },
                uses: dtolnay/rust-toolchain@#{@toolchain_sha},
              }
      """

      assert [%{line: 9, auto_fixable: false}, %{line: 14, auto_fixable: false}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "with: through an anchor or alias is resolved; the finding is not auto-fixable" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: &rust-inputs
                toolchain: stable
                components: clippy
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: *rust-inputs
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: &bare-inputs { components: clippy }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: *bare-inputs
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                <<: *rust-inputs
                components: rustfmt
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                <<: *bare-inputs
      """

      assert [%{line: 10} = a, %{line: 12} = b, %{line: 18} = c] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      refute a.auto_fixable
      assert a.reason =~ "anchor or alias"
      refute b.auto_fixable
      # A block `with:` whose merged mapping lacks the input is still the
      # fixer's: an explicit `toolchain:` child overrides a merge key, so
      # inserting one is a sound repair.
      assert c.auto_fixable
    end

    test "a merge sequence is resolved alias by alias; a merge it cannot read routes to review" do
      wf = """
      x-base: &base { components: clippy }
      x-rust: &rust { toolchain: nightly }
      x-wasm: &wasm { targets: wasm32-unknown-unknown }
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                <<: [*base, *rust]
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                <<: [*base, *wasm]
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                <<: { toolchain: nightly }
      """

      # The first step's channel comes from `*rust`: no finding, and nothing
      # for the fixer to override with `stable`.
      assert [%{line: 10} = none, %{line: 13} = inline] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      assert none.auto_fixable
      refute inline.auto_fixable
    end

    test "in a merge sequence the first mapping that defines toolchain decides, null included" do
      wf = """
      x-empty: &empty { toolchain: null }
      x-rust: &rust { toolchain: nightly }
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                <<: [*empty, *rust]
      """

      assert [%{line: 6}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a toolchain value continued on the next line is the value, not null" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain:
                  nightly
      """

      assert [] == WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "an anchor written inside a run: | script is text, not the anchor an alias names" do
      wf = """
      jobs:
        check:
          steps:
            - run: |
                fake: &inputs
                  toolchain: nightly
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: &inputs {}
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: *inputs
      """

      assert [%{line: 7}, %{line: 9}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "an anchor written after a tag (`with: !!map &inputs`) is still an anchored mapping" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: !!map &inputs
                components: clippy
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: &inputs2 !!map
                components: clippy
      """

      assert [%{line: 4} = a, %{line: 7} = b] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      refute a.auto_fixable
      refute b.auto_fixable
    end

    test "a uses: line inside a quoted scalar spread over lines is string text" do
      # The `run:` value opens a double-quoted scalar that runs to `echo end"`;
      # the `uses:` line between is part of the string, not the step's key.
      quoted = """
      jobs:
        check:
          steps:
            - run: "echo start
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              echo end"
      """

      assert [] == WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"q.yml" => quoted})

      # The same string opened JSON-style in a flow step (`{"run":"echo`), or
      # past an anchor (`run: &cmd "echo`), is a string all the same.
      for text <- [
            "jobs:\n  check:\n    steps: [{\"run\":\"echo start\n" <>
              "      uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n      echo end\"}]\n",
            "jobs:\n  check:\n    steps: [{\"run\": &cmd \"echo start\n" <>
              "      uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n      echo end\"}]\n",
            "jobs:\n  check:\n    steps:\n      - run: &cmd \"echo start\n" <>
              "        uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n        echo end\"\n",
            # The flow collections open on earlier lines (`steps: [`, `{`).
            "jobs:\n  check:\n    steps: [\n      {\n        \"run\":\"echo start\n" <>
              "        uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n        echo end\"\n" <>
              "      }\n    ]\n"
          ] do
        assert [] == WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"q.yml" => text})
      end

      # Controls: a quote closed on its own line, and a quote inside a
      # `run: |` script, open nothing — the bare step still fires.
      closed = """
      jobs:
        check:
          steps:
            - name: "it's the toolchain"
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      script = """
      jobs:
        check:
          steps:
            - run: |
                echo "unterminated
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      for wf <- [closed, script] do
        assert [%{rule: "WF026"}] =
                 WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"c.yml" => wf})
      end
    end

    test "anchor text inside a quoted scalar or a comment is not the anchor" do
      # The quoted `&rust-inputs` comes first; reading children from its line
      # found none and reported the aliased step as missing `toolchain`.
      wf = """
      env:
        NOTE: "see &rust-inputs below" # and &rust-inputs here
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: &rust-inputs
                toolchain: stable
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: *rust-inputs
      """

      assert [] == WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "an alias with no anchor cannot be read: reported, never passed silently" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: *nowhere
      """

      assert [%{line: 4, auto_fixable: false} = f] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      assert f.reason =~ "no `&nowhere` anchor"
    end

    test "an alias names the latest anchor before it, not the first in the file" do
      # YAML lets a document define `&inputs` again; `*inputs` is the
      # empty mapping here, so the step passes no toolchain.
      wf = """
      defaults: &inputs {toolchain: nightly}
      other: &inputs {}
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: *inputs
      """

      assert [%{line: 6, auto_fixable: false}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      # Controls: the other order is configured, and an anchor defined only
      # after the alias defines nothing for it.
      configured = String.replace(wf, "&inputs {toolchain: nightly}", "&inputs {}", global: false)

      configured =
        String.replace(configured, "other: &inputs {}", "other: &inputs {toolchain: nightly}")

      assert [] ==
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => configured})

      later = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: *inputs
      defaults: &inputs {toolchain: nightly}
      """

      assert [%{line: 4} = f] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => later})

      assert f.reason =~ "no `&inputs` anchor is defined before it"
    end

    test "a uses: value continued below its key, past a comment, is the step's pin" do
      wf = """
      jobs:
        check:
          steps:
            - uses:
                # pinned
                dtolnay/rust-toolchain@#{@toolchain_sha}
            - run: cargo test
      """

      assert [%{line: 4, auto_fixable: false, fix_script: nil} = f] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      assert f.reason =~ "continued below its key"

      # Controls: the same step with its input is configured, and the
      # reference inside a `run: |` script continues nothing.
      with_input =
        String.replace(
          wf,
          "    - run: cargo test",
          "      with: { toolchain: stable }\n    - run: cargo test"
        )

      assert [] ==
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => with_input})

      script = """
      jobs:
        check:
          steps:
            - run: |
                uses:
                  dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      assert [] == WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => script})
    end

    test "a uses: value carrying an anchor or tag is the step's pin; routed to review" do
      wf = """
      jobs:
        check:
          steps:
            - uses: &rust dtolnay/rust-toolchain@#{@toolchain_sha}
            - uses: !!str dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      assert [
               %{line: 4, auto_fixable: false, fix_script: nil} = a,
               %{line: 5, auto_fixable: false} = b
             ] = WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      assert a.reason =~ "anchor or tag"
      assert b.reason =~ "anchor or tag"
    end

    test "a flow step whose `{` stands on its own line is a flow step; routed to review" do
      wf = """
      jobs:
        check:
          steps: [
            {
              uses: dtolnay/rust-toolchain@#{@toolchain_sha}
            }
          ]
      """

      assert [%{line: 5, auto_fixable: false, fix_script: nil} = f] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      assert f.reason =~ "flow style"
    end

    test "a flow step's uses: continued on the next line is the step's pin" do
      wf = """
      jobs:
        check:
          steps: [ { uses:
              dtolnay/rust-toolchain@#{@toolchain_sha} } ]
      """

      assert [%{line: 3, auto_fixable: false}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a uses: alias of an anchored toolchain reference is the step's pin; routed to review" do
      wf = """
      x: &rust dtolnay/rust-toolchain@#{@toolchain_sha}
      jobs:
        check:
          steps:
            - uses: *rust
            - run: cargo test
      """

      assert [%{line: 5, auto_fixable: false, fix_script: nil} = f] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      assert f.current_sha == @toolchain_sha
      assert f.reason =~ "alias of an anchored reference"

      # Controls: with its input the aliased step is configured; an alias
      # of another action, or of an anchor defined after it, is not this pin.
      with_input =
        String.replace(wf, "      - run:", "        with: { toolchain: stable }\n      - run:")

      assert [] ==
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => with_input})

      other = String.replace(wf, "dtolnay/rust-toolchain", "actions/checkout")
      assert [] == WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => other})

      later = """
      jobs:
        check:
          steps:
            - uses: *rust
      x: &rust dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      assert [] == WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => later})
    end

    test "a flow step written without indentation, opener by opener, is still a step" do
      # Flow content may start in column 0 on every line: indentation says
      # nothing, so the ancestry is the openers the lines above left open.
      wf = """
      jobs: {
      build: {
      steps: [
      {
      uses: dtolnay/rust-toolchain@#{@toolchain_sha}
      }
      ]
      }
      }
      """

      assert [%{line: 5, auto_fixable: false, fix_script: nil} = f] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      assert f.reason =~ "flow style"

      # Controls: the step's own input configures it, and the same layout
      # under `env:` or a job's non-step key is data, not a step.
      with_input =
        String.replace(
          wf,
          "rust-toolchain@#{@toolchain_sha}\n",
          "rust-toolchain@#{@toolchain_sha},\nwith: { toolchain: stable }\n"
        )

      assert [] ==
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => with_input})

      env = String.replace(wf, "jobs: {", "env: {")
      assert [] == WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => env})

      fixture = String.replace(wf, "steps: [", "fixture: [")

      assert [] ==
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => fixture})

      # The step's `uses:` after its `{`, or after a sibling entry, on a
      # column-0 line is the same step.
      for {step, line} <- [
            {"{ uses: dtolnay/rust-toolchain@#{@toolchain_sha} }\n", 4},
            {"{\nname: x, uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n}\n", 5}
          ] do
        inline = "jobs: {\nbuild: {\nsteps: [\n" <> step <> "]\n}\n}\n"

        assert [%{line: ^line, auto_fixable: false}] =
                 WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => inline})

        configured =
          String.replace(
            inline,
            "@#{@toolchain_sha}",
            "@#{@toolchain_sha}, with: { toolchain: stable }"
          )

        assert [] ==
                 WorkflowAudit.check_sha_pinned_toolchain_without_input(%{
                   "rust.yml" => configured
                 })
      end

      shared =
        "jobs: {\nbuild: {\nsteps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ]\n}\n}\n"

      assert [%{line: 3, auto_fixable: false}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => shared})

      assert [] ==
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{
                 "rust.yml" => String.replace(shared, "steps: [", "fixture: [")
               })
    end

    test "node properties before a step's mapping leave it a flow step" do
      for {wf, line} <- [
            {"jobs:\n  b:\n    steps: [ &step { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ]\n",
             3},
            {"jobs:\n  b:\n    steps:\n      - &step { uses: dtolnay/rust-toolchain@#{@toolchain_sha} }\n",
             4}
          ] do
        assert [%{line: ^line, auto_fixable: false}] =
                 WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
      end
    end

    test "a quoted sibling continued past a `#` line does not hide the step" do
      # `# world",` continues the string; it is no comment, and its quote
      # closes the scalar before the step's own `uses:`.
      wf =
        "jobs: {\nb: {\nsteps: [\n{\nname: \"hello\n# world\",\n" <>
          "uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n}\n]\n}\n}\n"

      assert [%{line: 7, auto_fixable: false}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      # The same quote closing on the `uses:` line's own start.
      inline =
        "jobs:\n  b:\n    steps: [{ name: \"hello\n      world\", " <>
          "uses: dtolnay/rust-toolchain@#{@toolchain_sha} }]\n"

      assert [%{line: 4, auto_fixable: false}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => inline})
    end

    test "a bare sequence item's anchor holds the mapping under its dash" do
      item =
        "x:\n  - &inputs\n    toolchain: nightly\n" <> @rust_step <> "        with: *inputs\n"

      assert [] == wf026(item)

      unconfigured =
        "x:\n  - &inputs\n    components: clippy\n" <> @rust_step <> "        with: *inputs\n"

      assert [%{line: 7}] = wf026(unconfigured)
    end

    test "an & inside a plain scalar is text, not the anchor an alias names" do
      # `*channel` is the empty value anchored before `foo&channel`.
      text =
        "x: &channel \"\"\ny: foo&channel nightly\n" <>
          @rust_step <> "        with:\n          toolchain: *channel\n"

      assert [%{line: 6}] = wf026(text)
    end

    test "a block input's alias names the anchor on a child line above it" do
      sibling =
        @rust_step <>
          "        with:\n          source: &channel nightly\n          toolchain: *channel\n"

      assert [] == wf026(sibling)
    end

    test "an empty block scalar toolchain is the empty string; one with content is set" do
      empty =
        @rust_step <> "        with:\n          toolchain: >-\n          components: clippy\n"

      assert [%{line: 4}] = wf026(empty)

      folded = @rust_step <> "        with:\n          toolchain: >-\n            stable\n"
      assert [] == wf026(folded)
    end

    test "toolchain: *alias is the scalar its anchor holds" do
      step = fn tc ->
        "x: &empty \"\"\ny: &chan stable\njobs:\n  b:\n    steps:\n" <>
          "      - uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n        with:\n" <>
          "          toolchain: #{tc}\n"
      end

      # An empty anchored value is no input, as `toolchain: ""` is not.
      assert [%{line: 6, auto_fixable: true}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{
                 "rust.yml" => step.("*empty")
               })

      # An alias of no anchor cannot be read: reported, routed to review.
      assert [%{line: 6, auto_fixable: false, fix_script: nil} = f] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{
                 "rust.yml" => step.("*nope")
               })

      assert f.reason =~ "`toolchain:` input is the alias `*nope`"

      # Control: an anchored channel is the input.
      assert [] ==
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{
                 "rust.yml" => step.("*chan")
               })

      flow =
        "x: &empty ~\njobs:\n  b:\n    steps:\n" <>
          "      - { uses: dtolnay/rust-toolchain@#{@toolchain_sha}, with: { toolchain: *empty } }\n"

      assert [%{line: 5}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => flow})
    end

    test "a flow step's alias names the anchor before it, not one later on its line" do
      # `*inputs` is the empty mapping defined first; the configured
      # `&inputs` after it on the same line is not what it names.
      wf = """
      x: &inputs {}
      jobs:
        check:
          steps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha}, with: *inputs }, { run: echo, env: &inputs {toolchain: nightly} } ]
      """

      assert [%{line: 4}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})

      # Control: with the configured anchor first, the alias names it.
      configured = String.replace(wf, "x: &inputs {}", "x: &inputs {toolchain: stable}")

      assert [] ==
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => configured})
    end

    test "node properties on a toolchain value are not the value" do
      # An anchor names the node and `!!str ""` is still the empty string:
      # none of these is an input. `!!str null` is the string null, one.
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain: &channel ""
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain: !!str ""
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { toolchain: &channel null }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain: !!str null
      """

      assert [%{line: 4}, %{line: 7}, %{line: 10}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a toolchain value of nothing but an anchor is null, or the value below it" do
      bare = @rust_step <> "        with:\n          toolchain: &channel\n"
      assert [%{line: 4}] = wf026(bare)

      # Control: the anchored scalar continued on the line below is the input.
      assert [] == wf026(bare <> "            nightly\n")
    end

    test "an anchored toolchain value keeps its tag through an alias" do
      aliased = fn value ->
        "x: &e #{value}\n" <> @rust_step <> "        with:\n          toolchain: *e\n"
      end

      # `!!str null` is the string null, an input; `!!str ""` is still empty.
      assert [] == wf026(aliased.("!!str null"))
      assert [%{line: 5}] = wf026(aliased.(~s(!!str "")))
    end

    test "an escaped quote in a flow step's name hides none of its entries" do
      # `"a\"b, c"` is one scalar: `with: *inputs` is the step's entry and
      # names the empty mapping before it, not the configured `&inputs`
      # defined later on the same line.
      wf = """
      x: &inputs {}
      jobs:
        check:
          steps: [ { name: "a\\"b, c", uses: dtolnay/rust-toolchain@#{@toolchain_sha}, with: *inputs }, { run: echo, env: &inputs {toolchain: nightly} } ]
      """

      assert [%{line: 4}] = wf026(wf)

      # Control: with the configured anchor first, the alias names it.
      assert [] == wf026(String.replace(wf, "x: &inputs {}", "x: &inputs {toolchain: stable}"))
    end

    test "a steps: list some other mapping holds is data, not a job's steps" do
      wf = """
      jobs:
        check:
          strategy:
            matrix:
              include:
                - fixture:
                    steps:
                      - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
          steps:
            - run: echo
      """

      assert [] == wf026(wf)

      # Control: the job's own steps.
      assert [%{line: 4}] = wf026(@rust_step)
    end

    test "an alias name runs to whitespace or a flow indicator" do
      wf = "x: &e/x \"\"\n" <> @rust_step <> "        with:\n          toolchain: *e/x\n"

      assert [%{line: 5, auto_fixable: true}] = wf026(wf)
    end

    test "an anchor directly after a quoted key's colon in a flow mapping is the anchor" do
      wf = "x: {\"e\":&e \"\"}\n" <> @rust_step <> "        with:\n          toolchain: *e\n"

      # The anchor is read, so the empty value it holds routes to the fixer
      # rather than to review as an alias of no anchor.
      assert [%{line: 5, auto_fixable: true}] = wf026(wf)
    end

    test "a step given whole as an alias is judged where its mapping is anchored" do
      wf = """
      jobs:
        check:
          strategy:
            matrix:
              include:
                - &step
                  uses: dtolnay/rust-toolchain@#{@toolchain_sha}
          steps:
            - *step
      """

      # The anchored mapping is matrix data until the alias runs it: then it
      # is a live step with no input, routed to review (not the fixer).
      assert [%{line: 7, auto_fixable: false} = f] = wf026(wf)
      assert f.reason =~ "given as an alias"

      # Control: with no alias, the matrix entry is data.
      assert [] == wf026(String.replace(wf, "      - *step\n", "      - run: echo\n"))
    end

    test "a step alias whose mapping names the action by alias is judged at its uses: key" do
      wf = """
      env:
        TC: &tc dtolnay/rust-toolchain@#{@toolchain_sha}
      jobs:
        check:
          strategy:
            matrix:
              include:
                - &step
                  uses: *tc
          steps:
            - *step
      """

      assert [%{line: 9, auto_fixable: false}] = wf026(wf)
      assert [] == wf026(String.replace(wf, "      - *step\n", "      - run: echo\n"))
    end

    test "a uses: block scalar whose one line is the reference is the step's key; routed to review" do
      wf = """
      jobs:
        check:
          steps:
            - uses: >-
                dtolnay/rust-toolchain@#{@toolchain_sha}
      """

      assert [%{line: 4, auto_fixable: false} = f] = wf026(wf)
      assert f.reason =~ "continued below its key"

      # Control: with the input given, the block-scalar step is silent.
      assert [] == wf026(wf <> "        with:\n          toolchain: stable\n")
    end

    test "a toolchain: key with no value is no input: null and ~ included" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain:
                components: clippy
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { components: clippy, toolchain: null }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain: ~ # chosen later
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { toolchain: "1.85.0" }
      """

      assert [%{line: 4, auto_fixable: true}, %{line: 8, auto_fixable: true}, %{line: 10}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "YAML null is unquoted: \"null\" is a string, Null/NULL/\"\"/'' are not inputs" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { toolchain: "null" }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { toolchain: Null }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain: NULL
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                toolchain: ""
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { toolchain: '' }
      """

      assert [%{line: 6}, %{line: 8}, %{line: 11}, %{line: 14}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "an explicit toolchain: key wins over a merged anchor, empty or not" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: &inputs { toolchain: stable, components: clippy }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with:
                <<: *inputs
                toolchain:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { <<: *inputs, toolchain: null }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { <<: *inputs, toolchain: nightly }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { <<: *inputs }
      """

      assert [%{line: 6, auto_fixable: true}, %{line: 10, auto_fixable: true}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "every flow-style step sharing a line is judged, not only the first" do
      wf = """
      jobs:
        check:
          steps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha}, with: { toolchain: stable } }, { uses: dtolnay/rust-toolchain@#{@toolchain_sha} } ]
        other:
          steps: [ { uses: dtolnay/rust-toolchain@#{@toolchain_sha} }, { uses: dtolnay/rust-toolchain@#{@toolchain_sha}, with: { components: clippy } } ]
      """

      assert [%{line: 3, auto_fixable: false}, %{line: 5}, %{line: 5}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "upper-case hex is the same commit: it fires like lower-case" do
      sha = String.upcase(@toolchain_sha)

      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{sha}
            - { uses: dtolnay/rust-toolchain@#{sha} }
      """

      assert [%{line: 4, current_sha: ^sha}, %{line: 5}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a tag that merely begins with a SHA is a tag: it carries its channel" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}-patched
            - { uses: dtolnay/rust-toolchain@#{@toolchain_sha}-patched }
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha} # pinned
      """

      assert [%{line: 6}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "a plain block-style finding is still the fixer's" do
      wf = """
      jobs:
        check:
          steps:
            - uses: dtolnay/rust-toolchain@#{@toolchain_sha}
              with: { components: clippy } # a flow mapping the fixer extends
      """

      assert [%{line: 4, auto_fixable: true, fix_script: "fix-rust-toolchain-input.sh"}] =
               WorkflowAudit.check_sha_pinned_toolchain_without_input(%{"rust.yml" => wf})
    end

    test "the audit carries WF026 findings and a count" do
      wf = "jobs:\n  a:\n    steps:\n      - uses: dtolnay/rust-toolchain@#{@toolchain_sha}\n"
      result = WorkflowAudit.audit(["rust.yml"], %{"rust.yml" => wf})
      assert Enum.any?(result.findings, &(&1[:rule] == "WF026"))
      assert result.toolchain_input_missing_count == 1
    end
  end

  describe "check_known_broken_reusable_revision/1 (WF027)" do
    test "fires on rust-ci-reusable at a revision inside the toolchain window" do
      wf = "jobs:\n  rust-ci:\n    uses: #{@rust_ci}@#{@broken_standards}\n"
      [f] = WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})
      assert f.rule == "WF027"
      assert f.severity == :low
      assert f.advisory == true
      assert f.reusable == @rust_ci
      assert f.current_sha == @broken_standards
      assert f.replacement_sha == @good_standards
      assert f.auto_fixable
      assert f.fix_script == "fix-stale-reusable-pin.sh"
      assert f.reason =~ "standards#439"
    end

    test "fires on the same call under the repository's current name" do
      new_name = String.replace_prefix(@rust_ci, "hyperpolymath/", "metadatastician/")
      wf = "jobs:\n  rust-ci:\n    uses: #{new_name}@#{@broken_standards}\n"
      [f] = WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})
      assert f.rule == "WF027"
      assert f.current_sha == @broken_standards
    end

    test "WF016 fires on an orphan pin under the repository's current name" do
      wf =
        "jobs:\n  scan:\n    uses: metadatastician/standards/.github/workflows/" <>
          "hypatia-scan-reusable.yml@97df76210d966e0e1a89d5e5e9bcf66e16cea5e8\n"

      assert [%{rule: "WF016"}] =
               WorkflowAudit.check_orphan_standards_reusable_pin(%{"scan.yml" => wf})
    end

    test "silent at the replacement revision" do
      wf = "jobs:\n  rust-ci:\n    uses: #{@rust_ci}@#{@good_standards}\n"
      assert WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf}) == []
    end

    test "a job's uses: value continued below its key is the call; routed to review" do
      # The fleet fixer refuses a continued value (exit 2), so the finding
      # is not handed to it.
      wf =
        "jobs:\n  rust-ci:\n    uses:\n\n      # dead window\n      #{@rust_ci}@#{@broken_standards}\n"

      assert [%{line: 3, auto_fixable: false, fix_script: nil} = f] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})

      assert f.current_sha == @broken_standards
      assert f.reason =~ "continued below its key"

      # Control: under `env:` the same layout is data, not a call.
      env = "env:\n  CALL:\n    #{@rust_ci}@#{@broken_standards}\n"
      assert WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => env}) == []
    end

    test "a job's uses: with an anchor or tag, or a flow-style job, is the call; routed to review" do
      # The fleet fixer refuses all three (exit 2), so none is handed to it.
      wf =
        "jobs:\n  a:\n    uses: !!str #{@rust_ci}@#{@broken_standards}\n" <>
          "  b:\n    uses: &call #{@rust_ci}@#{@broken_standards}\n" <>
          "  c: { uses: #{@rust_ci}@#{@broken_standards} }\n"

      assert [
               %{line: 3, auto_fixable: false, fix_script: nil} = a,
               %{line: 5, auto_fixable: false} = b,
               %{line: 6, auto_fixable: false} = c
             ] = WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})

      assert a.reason =~ "anchor or tag"
      assert b.reason =~ "anchor or tag"
      assert c.reason =~ "flow mapping"

      # Control: the plain block call is still handed to the fixer.
      plain = "jobs:\n  a:\n    uses: #{@rust_ci}@#{@broken_standards}\n"

      assert [%{auto_fixable: true, fix_script: "fix-stale-reusable-pin.sh"}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => plain})
    end

    test "a job's uses: alias of an anchored reusable reference is the call; routed to review" do
      wf =
        "x: &call #{@rust_ci}@#{@broken_standards}\n" <>
          "jobs:\n  rust-ci:\n    uses: *call\n"

      assert [%{line: 4, auto_fixable: false, fix_script: nil} = f] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})

      assert f.current_sha == @broken_standards
      assert f.reason =~ "alias of an anchored reference"

      # Control: an alias of the replacement revision is silent.
      good = String.replace(wf, @broken_standards, @good_standards)
      assert WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => good}) == []
    end

    test "a job given whole as an alias calls the reusable its anchored mapping names; routed to review" do
      for template <- [
            "template: &job\n  uses: #{@rust_ci}@#{@broken_standards}\n",
            "template: &job { uses: #{@rust_ci}@#{@broken_standards} }\n"
          ] do
        wf = template <> "jobs:\n  rust: *job\n"

        assert [%{auto_fixable: false, fix_script: nil} = f] =
                 WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})

        assert f.reason =~ "given as an alias"

        # Control: with no job alias the anchored mapping is data.
        plain = template <> "jobs:\n  rust:\n    uses: #{@rust_ci}@#{@good_standards}\n"
        assert WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => plain}) == []
      end

      # A job anchored under `jobs:` and aliased is still the fixer's: the
      # anchored job's own key is the one it rewrites.
      own = "jobs:\n  base: &job\n    uses: #{@rust_ci}@#{@broken_standards}\n  rust: *job\n"

      assert [%{line: 3, auto_fixable: true}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => own})
    end

    test "a job's uses: block scalar whose one line is the call is the call; routed to review" do
      wf = "jobs:\n  rust-ci:\n    uses: >-\n      #{@rust_ci}@#{@broken_standards}\n"

      assert [%{line: 3, auto_fixable: false} = f] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})

      assert f.reason =~ "continued below its key"
    end

    test "a flow-style job written without indentation is still a job; routed to review" do
      # Every line in column 0: the brackets, not the indentation, say the
      # `uses:` is a job's key. The fleet fixer places a key by indentation
      # and cannot place these, so none is handed to it.
      own_line = "jobs: {\nrust: {\nuses: #{@rust_ci}@#{@broken_standards}\n}\n}\n"
      inline = "jobs: {\nrust: { uses: #{@rust_ci}@#{@broken_standards} }\n}\n"

      sibling =
        "jobs: {\na: { runs-on: x }, rust: { uses: #{@rust_ci}@#{@broken_standards} }\n}\n"

      for {wf, line} <- [{own_line, 3}, {inline, 2}, {sibling, 2}] do
        assert [%{line: ^line, current_sha: @broken_standards, auto_fixable: false} = f] =
                 WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})

        assert f.reason =~ "flow mapping"
      end

      # Controls: indented as its nesting reads, the own-line key is one the
      # fixer rewrites; under `env:`, or inside a job's `with: { }`, the
      # same layout is data, not a call.
      indented = "jobs: {\n  rust: {\n    uses: #{@rust_ci}@#{@broken_standards}\n  }\n}\n"

      assert [%{auto_fixable: true, fix_script: "fix-stale-reusable-pin.sh"}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => indented})

      for wf <- [own_line, inline],
          data <- [
            String.replace(wf, "jobs: {", "env: {"),
            String.replace(wf, "rust: {", "rust: {\nwith: {") <> "}\n"
          ] do
        assert WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => data}) == []
      end
    end

    test "a job's mapping carrying node properties, or past a `#` string line, is the call" do
      anchored = "jobs: { rust: &job { uses: #{@rust_ci}@#{@broken_standards} } }\n"

      assert [%{line: 1, auto_fixable: false}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => anchored})

      quoted =
        "jobs: {\nrust: {\nname: \"hello\n# world\",\nuses: #{@rust_ci}@#{@broken_standards}\n}\n}\n"

      assert [%{line: 5, auto_fixable: false}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => quoted})

      # Control: a `uses:` line inside that string is text.
      text =
        "jobs: {\nrust: {\nname: \"hello\nuses: #{@rust_ci}@#{@broken_standards}\n\",\n}\n}\n"

      assert WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => text}) == []
    end

    test "a flow-style job's uses: continued on the next line is the call" do
      wf = "jobs: { rust: { uses:\n    #{@rust_ci}@#{@broken_standards} } }\n"

      assert [%{line: 1, auto_fixable: false, current_sha: @broken_standards}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf})
    end

    test "silent for another reusable at the same revision: the window is per reusable" do
      wf =
        "jobs:\n  mirror:\n    uses: hyperpolymath/standards/.github/workflows/mirror-reusable.yml@#{@broken_standards}\n"

      assert WorkflowAudit.check_known_broken_reusable_revision(%{"mirror.yml" => wf}) == []
    end

    test "a commented-out pin does not fire" do
      wf =
        "jobs:\n  rust-ci:\n    # uses: #{@rust_ci}@#{@broken_standards}\n    uses: #{@rust_ci}@#{@good_standards}\n"

      assert WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf}) == []
    end

    test "the table is the measured window: 83 revisions, edges included, replacement excluded" do
      [row] = WorkflowAudit.broken_reusable_revisions()
      assert row.reusable == @rust_ci
      assert length(row.shas) == 83
      assert "d73c93e0e7d38f673b68e616d51a62f5860081fd" in row.shas
      assert "49ed4715" |> then(&Enum.any?(row.shas, fn s -> String.starts_with?(s, &1) end))
      refute row.replacement in row.shas
      assert Enum.all?(row.shas, &Regex.match?(~r/^[0-9a-f]{40}$/, &1))
    end

    test "the path inside a run: script or a string value is not a pin" do
      wf = """
      jobs:
        note:
          steps:
            - run: echo "uses: #{@rust_ci}@#{@broken_standards}"
            - name: "uses: #{@rust_ci}@#{@broken_standards}"
              run: true
      """

      assert [] = WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})
    end

    test "a quoted uses: key with a quoted value still fires" do
      wf = """
      jobs:
        rust:
          "uses": "#{@rust_ci}@#{@broken_standards}"
      """

      assert [%{rule: "WF027", current_sha: @broken_standards, line: 3}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})
    end

    test "only a key of a job is a call: run: | and env: copies are not" do
      wf = """
      jobs:
        fixture:
          env:
            uses: #{@rust_ci}@#{@broken_standards}
          steps:
            - run: |
                cat > caller.yml <<'EOF'
                uses: #{@rust_ci}@#{@broken_standards}
                EOF
        rust-ci:
          uses: #{@rust_ci}@#{@broken_standards}
      """

      assert [%{rule: "WF027", line: 11}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})
    end

    test "a tag that merely begins with a broken SHA is a tag, not the pin" do
      wf = """
      jobs:
        rust-ci:
          uses: #{@rust_ci}@#{@broken_standards}-patched
        other:
          uses: #{@rust_ci}@#{@broken_standards}
      """

      assert [%{rule: "WF027", line: 5}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})
    end

    test "upper-case hex is the same revision: it fires, reported as written" do
      sha = String.upcase(@broken_standards)
      wf = "jobs:\n  rust-ci:\n    uses: #{@rust_ci}@#{sha}\n"

      assert [%{rule: "WF027", current_sha: ^sha, replacement_sha: @good_standards}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})
    end

    test "a job-level uses: inside a quoted scalar spread over lines is string text" do
      wf = """
      jobs:
        rust-ci:
          name: "first line
          uses: #{@rust_ci}@#{@broken_standards}
          last line"
      """

      assert [] == WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})
    end

    test "the owner and repository match in either case; the workflow path as written" do
      # GitHub resolves `Hyperpolymath/Standards` to the same repository.
      mixed = "Hyperpolymath/Standards/.github/workflows/rust-ci-reusable.yml"
      wf = "jobs:\n  rust-ci:\n    uses: #{mixed}@#{@broken_standards}\n"

      assert [%{rule: "WF027", line: 3, replacement_sha: @good_standards, auto_fixable: true}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})

      inline = "jobs: { rust: { uses: #{mixed}@#{@broken_standards} } }\n"

      assert [%{rule: "WF027"}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => inline})

      # A file path inside the repository is case-sensitive: another file.
      other = "hyperpolymath/standards/.github/workflows/Rust-CI-Reusable.yml"
      wf2 = "jobs:\n  rust-ci:\n    uses: #{other}@#{@broken_standards}\n"
      assert [] == WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf2})
    end

    test "a job written as a flow mapping is a call: on its own line or inside jobs: { }" do
      wf = """
      jobs:
        rust-ci: { uses: #{@rust_ci}@#{@broken_standards}, secrets: inherit }
        note:
          steps:
            - run: echo "{ uses: #{@rust_ci}@#{@broken_standards} }"
      """

      assert [%{rule: "WF027", line: 2, current_sha: @broken_standards}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})

      one_line =
        "jobs: { a: { uses: #{@rust_ci}@#{@good_standards} }, b: { uses: #{@rust_ci}@#{@broken_standards} } }\n"

      assert [%{rule: "WF027", line: 1, current_sha: @broken_standards}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => one_line})

      nested_under_env =
        "jobs:\n  fixture:\n    env: { uses: #{@rust_ci}@#{@broken_standards} }\n"

      assert [] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => nested_under_env})
    end

    test "a quoted comma or brace in a sibling entry does not hide a flow-style job" do
      one_line =
        "jobs: { a: { name: \"x, y }\", uses: #{@rust_ci}@#{@good_standards} }, " <>
          "b: { uses: #{@rust_ci}@#{@broken_standards} } }\n"

      assert [%{rule: "WF027", line: 1, current_sha: @broken_standards}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => one_line})

      own_line = """
      jobs:
        rust-ci: { name: "CI, build", uses: #{@rust_ci}@#{@broken_standards} }
        note:
          env: { name: "x, y", uses: #{@rust_ci}@#{@broken_standards} }
      """

      assert [%{rule: "WF027", line: 2}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => own_line})
    end

    test "a one-line flow workflow inside a run heredoc is not a reusable call" do
      for opener <- ["", "{ "], closer = if(opener == "", do: "", else: " }") do
        wf = """
        jobs:
          build:
            steps:
              - run: |
                  cat <<'YAML'
                  #{opener}jobs: { fixture: { uses: #{@rust_ci}@#{@broken_standards} } }#{closer}
                  YAML
        """

        assert WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf}) == []
      end
    end

    test "a root-braced one-line workflow is read through its keys" do
      root =
        "{ on: push, jobs: { rust: { uses: #{@rust_ci}@#{@broken_standards} }, " <>
          "build: { steps: [ { uses: #{@rust_ci}@#{@broken_standards} } ] } } }\n"

      assert [%{rule: "WF027", line: 1, current_sha: @broken_standards}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => root})
    end

    test "a KYAML caller is read as written: the quoted job-level uses: fires, fixtures do not" do
      wf = """
      {
        name: "Rust",
        on: { push: { branches: ["main"] } },
        jobs: {
          note: {
            "runs-on": "ubuntu-latest",
            env: { FIXTURE: "uses: #{@rust_ci}@#{@broken_standards}" },
            steps: [{
              run: "echo uses: #{@rust_ci}@#{@broken_standards}",
            }],
          },
          "rust-ci": {
            uses: "#{@rust_ci}@#{@broken_standards}",
            secrets: "inherit",
          },
        },
      }
      """

      assert [%{rule: "WF027", line: 13, current_sha: @broken_standards, auto_fixable: true}] =
               WorkflowAudit.check_known_broken_reusable_revision(%{"x.yml" => wf})
    end

    test "the audit carries WF027 findings and a count" do
      wf = "jobs:\n  rust-ci:\n    uses: #{@rust_ci}@#{@broken_standards}\n"
      result = WorkflowAudit.audit(["rust-ci.yml"], %{"rust-ci.yml" => wf})
      assert Enum.any?(result.findings, &(&1[:rule] == "WF027"))
      assert result.broken_reusable_revision_count == 1
    end
  end

  describe "check_reusable_call_to_moved_repository/1 (WF028)" do
    @gov ".github/workflows/governance-reusable.yml"
    @pin "333b06a01db1874ffedb2de1bfabb195640927c7"

    defp wf028(wf), do: WorkflowAudit.check_reusable_call_to_moved_repository(%{"g.yml" => wf})

    test "fires on a job calling a standards reusable by its former owner" do
      wf = "on: push\njobs:\n  governance:\n    uses: hyperpolymath/standards/#{@gov}@#{@pin}\n"
      [f] = wf028(wf)
      assert f.rule == "WF028"
      assert f.line == 4
      assert f.severity == :low
      assert f.advisory == true
      assert f.auto_fixable == false
      assert f.fix_script == nil
      assert f.current_sha == @pin
      assert f.replacement == "metadatastician/standards/#{@gov}@#{@pin}"
      assert f.reason =~ "startup_failure"
    end

    test "matches the owner and repository in any case, quoted or not" do
      wf = "jobs:\n  g:\n    uses: \"Hyperpolymath/Standards/#{@gov}@#{@pin}\"\n"
      assert [%{rule: "WF028"}] = wf028(wf)
    end

    test "fires on a flow-style job and on a value continued below its key" do
      assert [_] = wf028("jobs:\n  g: { uses: hyperpolymath/standards/#{@gov}@#{@pin} }\n")

      assert [_] =
               wf028("jobs:\n  g:\n    uses:\n      hyperpolymath/standards/#{@gov}@#{@pin}\n")
    end

    test "keeps a block-style branch ref whole, punctuation included" do
      wf = "jobs:\n  g:\n    uses: hyperpolymath/standards/#{@gov}@release,one]\n"
      [f] = wf028(wf)
      assert f.current_sha == "release,one]"
      assert f.replacement == "metadatastician/standards/#{@gov}@release,one]"
    end

    test "reads a quoted flow value through its closing quote" do
      wf = "jobs: { g: { uses: \"hyperpolymath/standards/#{@gov}@release,one\" } }\n"
      [f] = wf028(wf)
      assert f.current_sha == "release,one"
      assert f.replacement == "metadatastician/standards/#{@gov}@release,one"
    end

    test "judges each alias where it stands, not by its line" do
      ref = "hyperpolymath/standards/#{@gov}@#{@pin}"

      wf =
        "x-job: &job\n  uses: metadatastician/standards/#{@gov}@#{@pin}\n" <>
          "x-step: &step\n  uses: #{ref}\n" <>
          "jobs: { a: *job, b: { runs-on: ubuntu-latest, steps: [*step] } }\n"

      assert [] == wf028(wf)
    end

    test "reports a flow sibling on the line a leading uses: opens" do
      ref = "hyperpolymath/standards/#{@gov}"
      wf = "jobs: {\n  a: {\n    uses: #{ref}@x }, b: { uses: #{ref}@y\n  }\n}\n"
      assert ["x", "y"] == wf |> wf028() |> Enum.map(& &1.current_sha) |> Enum.sort()
    end

    test "is silent on an anchored mapping reused only as a step" do
      ref = "hyperpolymath/standards/#{@gov}@#{@pin}"

      wf =
        "x-shared: &shared\n  uses: #{ref}\njobs:\n  b:\n    runs-on: ubuntu-latest\n" <>
          "    steps:\n      - *shared\n"

      assert [] == wf028(wf)
    end

    test "fires on an anchored mapping a job is given whole" do
      ref = "hyperpolymath/standards/#{@gov}@#{@pin}"
      wf = "x-shared: &shared\n  uses: #{ref}\njobs:\n  g: *shared\n"
      assert [%{line: 2}] = wf028(wf)
    end

    test "is silent on the current name" do
      assert [] == wf028("jobs:\n  g:\n    uses: metadatastician/standards/#{@gov}@#{@pin}\n")
    end

    test "is silent where the path is not a job's reusable call" do
      ref = "hyperpolymath/standards/#{@gov}@#{@pin}"

      wf =
        "env:\n  REF: #{ref}\njobs:\n  b:\n    runs-on: ubuntu-latest\n    steps:\n" <>
          "      # uses: #{ref}\n      - run: echo uses: #{ref}\n"

      assert [] == wf028(wf)
    end

    test "is silent on a step's uses: and on a uses: line inside a run: script" do
      ref = "hyperpolymath/standards/#{@gov}@#{@pin}"
      step = "jobs:\n  b:\n    runs-on: ubuntu-latest\n    steps:\n      - uses: #{ref}\n"
      script = "jobs:\n  b:\n    steps:\n      - run: |\n          uses: #{ref}\n"
      assert [] == wf028(step)
      assert [] == wf028(script)
    end

    test "is silent on another owner's repository of the same name" do
      assert [] == wf028("jobs:\n  g:\n    uses: someone/standards/#{@gov}@#{@pin}\n")
    end

    test "is reported by audit/3" do
      wf = "jobs:\n  g:\n    uses: hyperpolymath/standards/#{@gov}@#{@pin}\n"
      result = WorkflowAudit.audit(["g.yml"], %{"g.yml" => wf})
      assert result.moved_reusable_call_count == 1
      assert Enum.any?(result.findings, &(&1[:rule] == "WF028"))
    end

    test "hypatia's own workflows call no moved repository" do
      contents =
        for path <- Path.wildcard(".github/workflows/*.yml"),
            into: %{},
            do: {Path.basename(path), File.read!(path)}

      assert map_size(contents) > 0
      assert [] == WorkflowAudit.check_reusable_call_to_moved_repository(contents)
    end
  end
end

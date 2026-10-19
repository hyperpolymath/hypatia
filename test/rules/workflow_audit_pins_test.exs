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
      assert f.severity == :medium
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
      assert f.severity == :medium
      assert f.reusable == @rust_ci
      assert f.current_sha == @broken_standards
      assert f.replacement_sha == @good_standards
      assert f.auto_fixable
      assert f.fix_script == "fix-stale-reusable-pin.sh"
      assert f.reason =~ "standards#439"
    end

    test "silent at the replacement revision" do
      wf = "jobs:\n  rust-ci:\n    uses: #{@rust_ci}@#{@good_standards}\n"
      assert WorkflowAudit.check_known_broken_reusable_revision(%{"rust-ci.yml" => wf}) == []
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

    test "the audit carries WF027 findings and a count" do
      wf = "jobs:\n  rust-ci:\n    uses: #{@rust_ci}@#{@broken_standards}\n"
      result = WorkflowAudit.audit(["rust-ci.yml"], %{"rust-ci.yml" => wf})
      assert Enum.any?(result.findings, &(&1[:rule] == "WF027"))
      assert result.broken_reusable_revision_count == 1
    end
  end
end

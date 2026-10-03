# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.KyamlWorkflowTest do
  @moduledoc """
  KYAML (flow-style) workflows must be judged exactly like their block-style
  equivalents (standards YAML-POLICY Y-3). The fixtures are the literal output
  of `yq -p yaml -o kyaml '.'`: every value is quoted, every mapping is braced,
  `run: |` bodies become one quoted string, and step items open with a bare `{`.
  Each rule gets a positive (must flag), a control (must not flag) and, where
  a line scan could be fooled, a decoy.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Rules.{PinIntegrity, ResearchExtensions}

  @sha "0123456789012345678901234567890123456789"

  defp repo_with(content) do
    repo = Path.join(System.tmp_dir!(), "kyaml_test_#{System.unique_integer([:positive])}")
    wf = Path.join([repo, ".github", "workflows"])
    File.mkdir_p!(wf)
    File.write!(Path.join(wf, "ci.yml"), content)
    repo
  end

  defp secrets_job(hardener) do
    """
    # SPDX-License-Identifier: MPL-2.0
    {
      name: "deploy",
      on: "push",
      jobs: {
        deploy: {
          runs-on: "ubuntu-latest",
          steps: [
    #{hardener}        {
              name: "deploy",
              run: "deploy --token=${{ secrets.DEPLOY_KEY }}\\n",
            },
          ],
        },
      },
    }
    """
  end

  @hardener """
          {
            uses: "step-security/harden-runner@#{@sha}", # v2
            with: {
              egress-policy: "block",
            },
          },
  """

  describe "RE001 on KYAML" do
    test "a secrets job without harden-runner is flagged" do
      findings = ResearchExtensions.re001_missing_harden_runner(repo_with(secrets_job("")))
      assert [%{rule: "RE001", line: 11}] = findings
    end

    test "control: the same job with harden-runner is not flagged" do
      assert [] ==
               ResearchExtensions.re001_missing_harden_runner(repo_with(secrets_job(@hardener)))
    end
  end

  describe "RE003 on KYAML" do
    test "a quoted actions/cache with a head_ref key is flagged" do
      repo =
        repo_with("""
        {
          on: "pull_request",
          jobs: {
            b: {
              runs-on: "ubuntu-latest",
              steps: [
                {
                  uses: "actions/cache@#{@sha}", # v4
                  with: {
                    key: "x-${{ github.head_ref }}",
                  },
                },
              ],
            },
          },
        }
        """)

      assert [%{rule: "RE003"}] = ResearchExtensions.re003_cache_key_poisoning(repo)
    end
  end

  describe "RE005 on KYAML" do
    test "a test step whose quoted run body ends in || true is flagged" do
      repo =
        repo_with("""
        {
          on: "push",
          jobs: {
            t: {
              runs-on: "ubuntu-latest",
              steps: [
                {
                  name: "run tests",
                  run: "npm test || true\\n",
                },
              ],
            },
          },
        }
        """)

      assert [%{rule: "RE005"}] = ResearchExtensions.re005_test_swallows_exit(repo)
    end

    test "control: a test step without a swallow is not flagged" do
      repo =
        repo_with("""
        {
          jobs: {
            t: {
              runs-on: "ubuntu-latest",
              steps: [
                {
                  name: "run tests",
                  run: "npm test\\n",
                },
              ],
            },
          },
        }
        """)

      assert [] == ResearchExtensions.re005_test_swallows_exit(repo)
    end
  end

  describe "PinIntegrity.pin_sites/1 on KYAML" do
    test "a quoted pin with a trailing comma and comment is one site, unquoted" do
      assert [%{line: 1, action: "actions/checkout", ref: @sha, comment: "v4.2.2"}] =
               PinIntegrity.pin_sites(~s(          uses: "actions/checkout@#{@sha}", # v4.2.2))
    end

    test "a one-line flow step is a site" do
      assert [%{action: "actions/setup-node", ref: "v4"}] =
               PinIntegrity.pin_sites(~s(        { uses: "actions/setup-node@v4" },))
    end

    test "a single-quoted pin is a site" do
      assert [%{ref: @sha}] = PinIntegrity.pin_sites("  - uses: 'actions/checkout@#{@sha}'")
    end

    test "decoy: a uses: inside a quoted run string is not a site" do
      assert [] == PinIntegrity.pin_sites(~s(          run: "echo uses: actions/checkout@v4\\n",))
    end

    test "control: block-style pins are unchanged" do
      assert [%{action: "actions/checkout", ref: @sha, comment: "v4"}] =
               PinIntegrity.pin_sites("      - uses: actions/checkout@#{@sha} # v4")
    end
  end
end

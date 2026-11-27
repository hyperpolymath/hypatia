# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ResearchExtensionsReviewRegressionTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ResearchExtensions

  defp indent(text, width) do
    text
    |> String.trim_trailing()
    |> String.split("\n")
    |> Enum.map_join("\n", &(String.duplicate(" ", width) <> &1))
    |> Kernel.<>("\n")
  end

  defp findings(steps, endpoints \\ [], options \\ []) do
    repo = Path.join(System.tmp_dir!(), "re001_review_#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(repo, ".github/workflows"))
    on_exit(fn -> File.rm_rf!(repo) end)

    workflow_env =
      if env = options[:workflow_env], do: "env:\n" <> indent(env, 2), else: ""

    job_env = if env = options[:job_env], do: "    env:\n" <> indent(env, 6), else: ""

    File.write!(
      Path.join(repo, ".github/workflows/ship.yml"),
      "on: push\n" <>
        workflow_env <>
        "jobs:\n  ship:\n    runs-on: ubuntu-latest\n" <>
        job_env <>
        "    steps:\n" <> indent(steps, 6)
    )

    quoted = fn values -> Enum.map_join(values, " ", &inspect/1) end

    File.write!(Path.join(repo, "test_chora.deed"), """
    ;; SPDX-License-Identifier: CC-BY-SA-4.0
    (repo-deed
      :schema-version "1.0.0"
      :canonical-name "test"
      :repo-uuid #u8"chora-registry:github.com/o/test"
      :beholding-chora #u8"chora:estate/github.com/o"
      (egress
        (job :workflow ".github/workflows/ship.yml" :job "ship"
          :secrets (#{quoted.(options[:secrets] || ["TOKEN"])})
          :endpoints (#{quoted.(endpoints)}) :enforcement none)))
    """)

    ResearchExtensions.re001_missing_harden_runner(repo)
  end

  defp run(script),
    do: "- run: |\n" <> indent(script, 4) <> "  env:\n    K: ${{ secrets.TOKEN }}\n"

  test "only numeric loopback addresses are exempt, not 127-prefixed hostnames" do
    assert [finding] = findings(run("curl https://127.example.com"))
    assert finding.reason =~ ~s|"127.example.com:443"|
    assert findings(run("curl https://127.0.0.1 https://127.255.255.254 https://0.0.0.0")) == []
    assert [_] = findings(run("curl https://127.999.0.1"))
  end

  test "GitHub wildcard allows are exempt only within GitHub's domain boundary" do
    steps = """
    - uses: step-security/harden-runner@v2
      with:
        egress-policy: block
        allowed-endpoints: '*.github.com:443 *.githubusercontent.com:443'
    - run: echo ${{ secrets.TOKEN }}
    """

    assert findings(steps) == []

    assert [finding] =
             findings(
               String.replace(steps, "*.github.com:443", "*.github.com.attacker.invalid:443")
             )

    assert finding.reason =~ "github.com.attacker.invalid"
  end

  test "punycode hostnames and their ports are matched in full" do
    steps = run("curl https://example.xn--p1ai:8443/upload")
    assert [finding] = findings(steps)
    assert finding.reason =~ ~s|"example.xn--p1ai:8443"|
    assert findings(steps, ["example.xn--p1ai:8443"]) == []
    assert [_] = findings(steps, ["example.xn:8443"])

    assert findings(run("curl https://EXAMPLE.XN--P1AI.:8443/upload"), ["example.xn--p1ai:8443"]) ==
             []

    assert findings(run("curl https://example.com.:8443/upload"), ["example.com:8443"]) == []
  end

  test "Docker action references name explicit external registries" do
    for {image, endpoint} <- [
          {"registry.example.com/org/action:tag", "registry.example.com:443"},
          {"registry.example.com:5000/org/action:tag", "registry.example.com:5000"}
        ] do
      steps = "- uses: docker://#{image}\n  env:\n    K: ${{ secrets.TOKEN }}\n"
      assert [finding] = findings(steps)
      assert finding.reason =~ endpoint
      assert findings(steps, [endpoint]) == []
    end
  end

  test "a dotted Docker image without an explicit registry is not an endpoint" do
    steps = "- uses: docker://my.action:1\n  env:\n    K: ${{ secrets.TOKEN }}\n"
    assert findings(steps) == []
  end

  test "job environment overrides remove the workflow's shadowed secret" do
    assert findings("- run: echo ready", [],
             workflow_env: "TOKEN: ${{ secrets.OLD }}",
             job_env: "TOKEN: ${{ secrets.NEW }}",
             secrets: ["NEW"]
           ) == []

    assert [finding] =
             findings("- run: echo ready", [],
               workflow_env: "TOKEN: ${{ secrets.OLD }}",
               job_env: "OTHER: ${{ secrets.NEW }}",
               secrets: ["NEW"]
             )

    assert finding.reason =~ ~s|reads undeclared "OLD"|
  end

  test "job overrides also remove shadowed credentials from code exposure" do
    assert [finding] =
             findings("- run: npm ci\n- run: send\n  env:\n    K: ${{ secrets.TOKEN }}", [],
               workflow_env: "SHADOW: ${{ secrets.OLD }}",
               job_env: "SHADOW: public",
               secrets: ["DECLARED"]
             )

    assert finding.severity == :warn
    assert finding.reason =~ ~s|reads undeclared "TOKEN"|
    refute finding.reason =~ "OLD"
    refute finding.reason =~ "in the same step"
  end

  test "a command substitution closes inside its word before a literal hash" do
    for prefix <- [
          "echo $(printf x)#tag;",
          ~S|echo $(printf '%s' "$(printf x)")#tag;|,
          ~S|echo "$(printf x)"#tag;|
        ] do
      assert [finding] = findings(run(prefix <> " curl https://upload.example.com"))
      assert finding.reason =~ "upload.example.com:443"
    end

    assert findings(run("echo $(printf x) # curl https://comment.example.com")) == []
    assert findings(run("(echo x)# curl https://comment.example.com")) == []
  end

  test "an unknown allowed port is not evidence that every port is allowed" do
    steps = """
    - uses: step-security/harden-runner@v2
      with:
        egress-policy: block
        allowed-endpoints: api.example.com:${{ vars.PORT }}
    - run: echo ${{ secrets.TOKEN }}
    """

    assert [finding] = findings(steps, ["api.example.com"])
    assert finding.reason =~ ~s|does not allow declared "api.example.com"|
    assert findings(String.replace(steps, ":${{ vars.PORT }}", ""), ["api.example.com"]) == []
  end

  test "hashes in environment values are data, including inherited values" do
    for options <- [
          [workflow_env: "UPLOAD: '# production https://upload.example.com'"],
          [job_env: "UPLOAD: '# production https://upload.example.com'"]
        ] do
      assert [finding] = findings(run("curl $UPLOAD"), [], options)
      assert finding.reason =~ "upload.example.com:443"
    end

    steps = """
    - run: curl "$UPLOAD"
      env:
        K: ${{ secrets.TOKEN }}
        UPLOAD: '# production https://upload.example.com'
    """

    assert [finding] = findings(steps)
    assert finding.reason =~ "upload.example.com:443"
  end

  test "hashes in action input values are data, including an input called run" do
    steps = """
    - uses: owner/action@v1
      with:
        run: '# production https://upload.example.com'
        token: ${{ secrets.TOKEN }}
    """

    assert [finding] = findings(steps)
    assert finding.reason =~ "upload.example.com:443"
    assert findings(steps, ["upload.example.com:443"]) == []
  end
end

# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.CicdRules.RescriptNpmJsTest do
  @moduledoc """
  Tests for the Layer-1 path_allow_prefixes additions on
  `:rescript_detected`, `:rescript_interface_detected`, `:nodejs_detected`,
  and the new `:javascript_detected` / `:javascript_jsx_detected` rules.

  Mirrors `cicd_rules_typescript_test.exs` for the TS-analog batch that
  hypatia#375 + #378 shipped. Filed as the Layer-1 batch for
  hyperpolymath/standards#260 (RS STEP 1) + #261 (npm STEP 1) +
  #263 (JS STEP 1).
  """

  use ExUnit.Case, async: true

  alias Hypatia.Rules.CicdRules

  # ------------------------------------------------------------------- ReScript

  describe "rescript_detected rule" do
    test "flags ReScript source files outside the allowlist" do
      files = [
        "idaptik/src/vm.res",
        "panll/lib/handler.res",
        "developer-ecosystem/some-pkg/src/main.res"
      ]

      results = CicdRules.check_commit_blocks(files)
      rs = Enum.find(results, &(&1.rule == :rescript_detected))

      assert rs, "expected :rescript_detected finding for non-exempt .res files"
      assert length(rs.files) == 3
      assert rs.reason =~ "ReScript banned"
      assert rs.reason =~ "AffineScript"
      assert rs.reason =~ "standards#252"
    end

    test "exempts bsconfig.json tooling configs" do
      files = ["some-pkg/bsconfig.json"]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :rescript_detected)) == nil
    end

    test "exempts upstream forks" do
      files = [
        "rescript/lib/Belt.res",
        "servers/some-server/main.res",
        "repos-monorepo/sub/handler.res",
        "linguist/samples/ReScript/example.res"
      ]

      results = CicdRules.check_commit_blocks(files)

      assert Enum.find(results, &(&1.rule == :rescript_detected)) == nil,
             "upstream-fork ReScript is not estate-authored"
    end

    test "exempts archived repos" do
      files = ["hyperpolymath-archive/legacy-rs-pkg/src/main.res"]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :rescript_detected)) == nil
    end

    test "exempts vendored deps" do
      files = [
        "some-repo/deps/phoenix_live_view/lib/x.res",
        "another/node_modules/some-pkg/dist.res"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :rescript_detected)) == nil
    end

    test "exempts vscode editor-host extensions" do
      files = ["some-pkg/editors/vscode/src/extension.res"]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :rescript_detected)) == nil
    end

    test "exempts compiled output dirs (lib/js, lib/es6, lib/bs)" do
      files = [
        "some-pkg/lib/js/src/x.res",
        "some-pkg/lib/es6/src/y.res",
        "some-pkg/lib/bs/src/z.res"
      ]

      results = CicdRules.check_commit_blocks(files)

      assert Enum.find(results, &(&1.rule == :rescript_detected)) == nil,
             "bsc compilation output is not source"
    end

    test "exempts bootstrap shims" do
      files = [
        "affinescript-deno-test/src/x.res",
        "affinescript-cli/lib/y.res"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :rescript_detected)) == nil
    end
  end

  describe "rescript_interface_detected rule" do
    test "flags .resi outside the allowlist with same carve-outs as .res" do
      flagged_files = ["idaptik/src/vm.resi"]

      exempt_files = [
        "rescript/lib/Belt.resi",
        "some-pkg/lib/js/x.resi",
        "hyperpolymath-archive/legacy/main.resi",
        "some-pkg/editors/vscode/src/extension.resi"
      ]

      flagged = CicdRules.check_commit_blocks(flagged_files)
      exempt = CicdRules.check_commit_blocks(exempt_files)

      assert Enum.find(flagged, &(&1.rule == :rescript_interface_detected))
      assert Enum.find(exempt, &(&1.rule == :rescript_interface_detected)) == nil
    end
  end

  # ------------------------------------------------------------------ Node/npm

  describe "nodejs_detected rule" do
    test "flags package-lock.json outside the allowlist" do
      files = [
        "developer-ecosystem/some-pkg/package-lock.json",
        "ssg-collection/sub/package-lock.json"
      ]

      results = CicdRules.check_commit_blocks(files)
      nj = Enum.find(results, &(&1.rule == :nodejs_detected))

      assert nj, "expected :nodejs_detected finding for non-exempt package-lock.json"
      assert length(nj.files) == 2
      assert nj.reason =~ "Node.js banned"
      assert nj.reason =~ "Deno"
      assert nj.reason =~ "standards#253"
    end

    test "exempts VSCode extension host-required lockfiles" do
      files = ["some-pkg/editors/vscode/package-lock.json"]

      results = CicdRules.check_commit_blocks(files)

      assert Enum.find(results, &(&1.rule == :nodejs_detected)) == nil,
             "VSCode extension toolchain runs on Node — lockfile is contractual"
    end

    test "exempts bootstrap shims" do
      files = [
        "affinescript-deno-test/package-lock.json",
        "affinescript-cli/package-lock.json"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :nodejs_detected)) == nil
    end

    test "exempts upstream forks + archived + vendored" do
      files = [
        "rescript/package-lock.json",
        "servers/some/package-lock.json",
        "hyperpolymath-archive/legacy/package-lock.json",
        "some/deps/x/package-lock.json",
        "some/node_modules/x/package-lock.json"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :nodejs_detected)) == nil
    end

    test "exempts example/test fixtures" do
      files = [
        "some-pkg/example/package-lock.json",
        "some-pkg/examples/sub/package-lock.json",
        "some-pkg/test-fixtures/sub/package-lock.json",
        "some-pkg/fixtures/sub/package-lock.json"
      ]

      results = CicdRules.check_commit_blocks(files)

      assert Enum.find(results, &(&1.rule == :nodejs_detected)) == nil,
             "example/fixture lockfiles demonstrate npm consumer — not own toolchain"
    end
  end

  # ---------------------------------------------------------- Unnecessarily-JS

  describe "javascript_detected rule" do
    test "flags .js outside the allowlist" do
      files = [
        "panll/src/handler.js",
        "stapeln/lib/main.js",
        "developer-ecosystem/some-pkg/src/x.js"
      ]

      results = CicdRules.check_commit_blocks(files)
      js = Enum.find(results, &(&1.rule == :javascript_detected))

      assert js, "expected :javascript_detected finding for non-exempt .js files"
      assert length(js.files) == 3
      assert js.reason =~ "Unnecessarily-JavaScript banned"
      assert js.reason =~ "AffineScript"
      assert js.reason =~ "standards#254"
    end

    test "exempts host-required JS (MCP servers, plugin entry points)" do
      files = [
        "boj-server/mcp-bridge/lib/dispatcher.js",
        "some-pkg/plugins/foo/index.js"
      ]

      results = CicdRules.check_commit_blocks(files)

      assert Enum.find(results, &(&1.rule == :javascript_detected)) == nil,
             "host-required JS — JS is the host contract for these surfaces"
    end

    test "exempts tooling configs (.config.{js,cjs,mjs})" do
      files = [
        "some-pkg/vite.config.js",
        "some-pkg/jest.config.cjs",
        "some-pkg/babel.config.mjs"
      ]

      results = CicdRules.check_commit_blocks(files)

      assert Enum.find(results, &(&1.rule == :javascript_detected)) == nil,
             "build-orchestration configs are tooling"
    end

    test "exempts compiled output (out/, lib/js/, .deno/)" do
      files = [
        "some-pkg/out/dist.js",
        "some-pkg/lib/js/x.js",
        "some-pkg/.deno/cache/y.js"
      ]

      results = CicdRules.check_commit_blocks(files)

      assert Enum.find(results, &(&1.rule == :javascript_detected)) == nil,
             "AS / RS compile output is not source"
    end

    test "exempts vscode editor-host extensions" do
      files = [
        "some-pkg/editors/vscode/dist/extension.js",
        "some-pkg/extensions/vscode/out/extension.js"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :javascript_detected)) == nil
    end

    test "exempts upstream forks + archived + vendored" do
      files = [
        "rescript/lib/x.js",
        "servers/some/main.js",
        "linguist/samples/JavaScript/example.js",
        "hyperpolymath-archive/legacy/main.js",
        "some/deps/x/y.js",
        "some/node_modules/x/y.js"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :javascript_detected)) == nil
    end

    test "exempts bootstrap shims" do
      files = [
        "affinescript-deno-test/src/x.js",
        "affinescript-cli/lib/y.js"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :javascript_detected)) == nil
    end

    test "flags ordinary .js even when name resembles a carve-out" do
      # `boj-server/src/something.js` is NOT under mcp-bridge/ or plugins/
      files = ["boj-server/src/handler.js"]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :javascript_detected))
    end
  end

  describe "javascript_jsx_detected rule" do
    test "flags .jsx outside the allowlist with same carve-outs as .js" do
      flagged = CicdRules.check_commit_blocks(["panll/components/Button.jsx"])

      exempt =
        CicdRules.check_commit_blocks([
          "rescript/example.jsx",
          "some-pkg/editors/vscode/dist/extension.jsx",
          "hyperpolymath-archive/legacy/Component.jsx",
          "some-pkg/plugins/foo/index.jsx"
        ])

      assert Enum.find(flagged, &(&1.rule == :javascript_jsx_detected))
      assert Enum.find(exempt, &(&1.rule == :javascript_jsx_detected)) == nil
    end
  end
end

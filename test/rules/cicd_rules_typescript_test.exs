# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.CicdRules.TypescriptTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CicdRules

  describe "typescript_detected rule" do
    test "flags TS source files outside the allowlist" do
      files = [
        "idaptik/idaptik/idaptik-dlc-iky/core/vm.ts",
        "wordpress-tools/praxis/SymbolicEngine/swarm/src/executor.ts",
        "lcb-website/src/index.ts"
      ]

      results = CicdRules.check_commit_blocks(files)
      ts = Enum.find(results, &(&1.rule == :typescript_detected))

      assert ts, "expected :typescript_detected finding for non-exempt .ts files"
      assert length(ts.files) == 3
      assert ts.reason =~ "TypeScript banned in NEW code"
      assert ts.reason =~ "AffineScript"
    end

    test "exempts .d.ts declaration files (FFI/library headers)" do
      files = [
        "rrecord-verity/WebExtensions.d.ts",
        "tma-mark2/types/global.d.ts",
        "developer-ecosystem/zerotier-k8s-link/index.d.ts"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :typescript_detected)) == nil,
             ".d.ts declaration files are exempt (headers, not implementation)"
    end

    test "exempts */bindings/{deno,typescript,ts}/ interop targets" do
      files = [
        "proven/bindings/deno/mod.ts",
        "proven/bindings/deno/src/safe_unit.ts",
        "some-lib/bindings/typescript/index.ts",
        "another-lib/bindings/ts/client.ts"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :typescript_detected)) == nil,
             "consumer-facing TS bindings are exempt (parallel to v-bindings/v-adapter)"
    end

    test "exempts avow-protocol/telegram-bot/avow-telegram-bot/ (Telegraf PERMANENT)" do
      files = [
        "avow-protocol/telegram-bot/avow-telegram-bot/src/bot.ts",
        "avow-protocol/telegram-bot/avow-telegram-bot/test-mock.ts"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :typescript_detected)) == nil
    end

    test "exempts tooling configs (vite/vitest/tsup/tsconfig)" do
      files = [
        "hyperpolymath-archive/zotero-nesy/vite.config.ts",
        "some-repo/vitest.config.ts",
        "another-repo/tsup.config.ts"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :typescript_detected)) == nil,
             "build orchestration is exempt (not application code)"
    end

    test "exempts affinescript-deno-test and affinescript-cli bootstrap shims" do
      files = [
        "affinescript-deno-test/runner.ts",
        "affinescript-cli/bin/cli.ts"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :typescript_detected)) == nil
    end

    test "exempts upstream-fork repos (rescript/servers/repos-monorepo)" do
      files = [
        "rescript/jscomp/test/test.ts",
        "servers/src/everything/index.ts",
        "repos-monorepo/some/path/file.ts"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :typescript_detected)) == nil,
             "upstream forks are not estate-authored — vendored as-is"
    end

    test "exempts hyperpolymath-archive/** (archived repos)" do
      files = [
        "hyperpolymath-archive/avow-telegram-bot/test-mock.ts",
        "hyperpolymath-archive/some-old-project/src/main.ts"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :typescript_detected)) == nil
    end

    test "flags new TS even with carve-out-like names but outside carve-out paths" do
      # `proven/src/something.ts` is NOT in `proven/bindings/deno/`,
      # so it MUST still flag.
      files = ["proven/src/handler.ts"]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :typescript_detected))
    end
  end
end

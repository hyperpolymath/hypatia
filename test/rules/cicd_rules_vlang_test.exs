# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.CicdRules.VlangTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CicdRules

  describe "vlang_detected rule" do
    test "flags V-lang source files outside the allowlist" do
      files = [
        "laminar/api/v/laminar.v",
        "quandledb/src/api/semantic_api.v",
        "proven-servers/connectors/proven-nesy-solver-api/v/server.v"
      ]

      results = CicdRules.check_commit_blocks(files)
      vlang = Enum.find(results, &(&1.rule == :vlang_detected))

      assert vlang, "expected :vlang_detected finding for non-exempt .v files"
      assert length(vlang.files) == 3
      assert vlang.reason =~ "V-lang banned"
      assert vlang.reason =~ "Zig"
    end

    test "exempts the v-ecosystem R&D carve-out" do
      files = [
        "developer-ecosystem/v-ecosystem/v-rest/src/rest.v",
        "developer-ecosystem/v-ecosystem/v-grpc/src/grpc.v"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :vlang_detected)) == nil
    end

    test "exempts asdf-vlang toolchain installer plugins" do
      files = [
        "asdf-augmenters/asdf-plugin-collection/plugins/vlang/recoverer-setup.v",
        "hyperpolymath-archive/asdf-vlang-plugin/src/install.v"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :vlang_detected)) == nil
    end

    test "exempts Coq proof scripts (.v is also Coq's extension)" do
      files = [
        "ephapax/formal/Semantics_L1.v",
        "absolute-zero/proofs/coq/quantum/QuantumMechanicsExact.v",
        "007/proofs/canonical-proof-suite/M2_lagrange.v",
        "my-lang/proofs/verification/coq/Typing.v",
        "phronesis/academic/formal-verification/coq/Phronesis.v",
        "wokelang/docs/proofs/verification/WokeLang.v",
        "echidna/examples/simple_proof.v",
        "echidna/tests/live_goals/identity.v",
        "hypatia/test/soundness/fixtures/code_safety/admitted.v"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :vlang_detected)) == nil,
             ".v files in Coq proof directories must never be flagged as V-lang"
    end

    test "exempts Verilog and linguist samples" do
      files = [
        "HOL/examples/PSL/1.01/executable-semantics/test_c1.v",
        "linguist/samples/Verilog/encrypted_module.v",
        "linguist/samples/V/hello.v"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :vlang_detected)) == nil
    end

    test "flags non-exempt .v files even with v-ecosystem-like names" do
      files = ["my-repo/api/v/v-rest-client.v"]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :vlang_detected))
    end
  end

  describe "vmod_detected rule" do
    test "flags v.mod manifest outside the allowlist" do
      files = [
        "stapeln/api/v/v.mod",
        "ambientops/recovery/emergency-room/v.mod"
      ]

      results = CicdRules.check_commit_blocks(files)
      vmod = Enum.find(results, &(&1.rule == :vmod_detected))

      assert vmod
      assert length(vmod.files) == 2
      assert vmod.reason =~ "build.zig.zon"
    end

    test "exempts v.mod inside v-ecosystem and asdf-vlang plugins" do
      files = [
        "developer-ecosystem/v-ecosystem/v-rest/v.mod",
        "asdf-augmenters/asdf-plugin-collection/plugins/vlang/v.mod",
        "hyperpolymath-archive/asdf-vlang-plugin/v.mod"
      ]

      results = CicdRules.check_commit_blocks(files)
      assert Enum.find(results, &(&1.rule == :vmod_detected)) == nil
    end
  end
end

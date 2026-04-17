# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.CodeSafetyTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CodeSafety

  describe "patterns_for_language/1" do
    test "returns Rust patterns for rust" do
      patterns = CodeSafety.patterns_for_language("rust")
      assert is_list(patterns)
      assert length(patterns) > 0
      ids = Enum.map(patterns, & &1.id)
      assert :unwrap_without_check in ids
      assert :unsafe_block in ids
    end

    test "returns ReScript patterns for rescript" do
      patterns = CodeSafety.patterns_for_language("rescript")
      ids = Enum.map(patterns, & &1.id)
      assert :getexn_on_external in ids
      assert :obj_magic in ids
    end

    test "returns Idris2 banned patterns" do
      patterns = CodeSafety.patterns_for_language("idris2")
      ids = Enum.map(patterns, & &1.id)
      assert :believe_me in ids
      assert :assert_total in ids
    end

    test "returns Haskell banned patterns" do
      patterns = CodeSafety.patterns_for_language("haskell")
      ids = Enum.map(patterns, & &1.id)
      assert :unsafe_coerce in ids
    end

    test "returns OCaml banned patterns" do
      patterns = CodeSafety.patterns_for_language("ocaml")
      ids = Enum.map(patterns, & &1.id)
      assert :obj_magic_ocaml in ids
    end

    test "returns Coq banned patterns" do
      patterns = CodeSafety.patterns_for_language("coq")
      ids = Enum.map(patterns, & &1.id)
      assert :admitted in ids
    end

    test "returns Lean banned patterns" do
      patterns = CodeSafety.patterns_for_language("lean")
      ids = Enum.map(patterns, & &1.id)
      assert :sorry in ids
    end

    test "returns Nickel patterns" do
      patterns = CodeSafety.patterns_for_language("nickel")
      assert length(patterns) > 0
      ids = Enum.map(patterns, & &1.id)
      assert :ncl_hardcoded_secret in ids
    end

    test "returns empty list for unknown language" do
      assert [] = CodeSafety.patterns_for_language("brainfuck")
    end
  end

  describe "scan_content/2" do
    test "detects unwrap() in Rust code" do
      code = """
      fn main() {
        let val = some_option.unwrap();
      }
      """

      findings = CodeSafety.scan_content(code, "rust")
      assert Enum.any?(findings, &(&1.rule == :unwrap_without_check))
    end

    test "detects believe_me in Idris2 code" do
      code = """
      total myProof : (a : Type) -> a
      myProof = believe_me ()
      """

      findings = CodeSafety.scan_content(code, "idris2")
      assert Enum.any?(findings, &(&1.rule == :believe_me))
    end

    test "detects assert_total in Idris2 code" do
      code = "myFunc = assert_total (loop xs)"

      findings = CodeSafety.scan_content(code, "idris2")
      assert Enum.any?(findings, &(&1.rule == :assert_total))
    end

    test "detects sorry in Lean code" do
      code = """
      theorem my_theorem : True := by
        sorry
      """

      findings = CodeSafety.scan_content(code, "lean")
      assert Enum.any?(findings, &(&1.rule == :sorry))
    end

    test "detects Admitted in Coq code" do
      code = """
      Theorem my_theorem : forall n, n = n.
      Proof. intros. Admitted.
      """

      findings = CodeSafety.scan_content(code, "coq")
      assert Enum.any?(findings, &(&1.rule == :admitted))
    end

    test "detects getExn in ReScript code" do
      code = "let value = response->Option.getExn"

      findings = CodeSafety.scan_content(code, "rescript")
      assert Enum.any?(findings, &(&1.rule == :getexn_on_external))
    end

    test "detects hardcoded secrets in Nickel config" do
      code = ~s(password = "hunter2")

      findings = CodeSafety.scan_content(code, "nickel")
      assert Enum.any?(findings, &(&1.rule == :ncl_hardcoded_secret))
    end

    test "returns empty for clean code" do
      code = """
      fn main() {
        let val = some_option.unwrap_or_else(|| default());
      }
      """

      findings = CodeSafety.scan_content(code, "rust")
      # unwrap_or_else is fine -- only unwrap() and unwrap_or(0) are flagged
      refute Enum.any?(findings, &(&1.rule == :unwrap_without_check))
    end

    test "counts occurrences" do
      code = """
      let a = x.unwrap();
      let b = y.unwrap();
      let c = z.unwrap();
      """

      [finding] = CodeSafety.scan_content(code, "rust")
                   |> Enum.filter(&(&1.rule == :unwrap_without_check))

      assert finding.occurrences == 3
    end

    test "includes CWE identifiers" do
      code = "unsafe { ptr::read(addr) }"

      findings = CodeSafety.scan_content(code, "rust")
      unsafe_finding = Enum.find(findings, &(&1.rule == :unsafe_block))

      assert unsafe_finding.cwe == "CWE-676"
    end
  end

  describe "scan_container_code/1" do
    test "detects wildcard CORS" do
      code = ~s(Access-Control-Allow-Origin: *)

      findings = CodeSafety.scan_container_code(code)
      assert Enum.any?(findings, &(&1.rule == :wildcard_cors))
    end

    test "detects root UID" do
      code = "uid: 0"

      findings = CodeSafety.scan_container_code(code)
      assert Enum.any?(findings, &(&1.rule == :default_to_root))
    end

    test "returns empty for safe container code" do
      code = """
      USER nonroot
      EXPOSE 8080
      ENTRYPOINT ["/app/server"]
      """

      findings = CodeSafety.scan_container_code(code)
      assert findings == []
    end
  end

  describe "check_rust_safety/2" do
    test "flags lib.rs missing forbid(unsafe_code)" do
      findings = CodeSafety.check_rust_safety(
        ["src/lib.rs"],
        %{"src/lib.rs" => "pub fn hello() {}"}
      )

      assert Enum.any?(findings, &(&1.rule == :missing_forbid_unsafe))
    end

    test "passes lib.rs with forbid(unsafe_code)" do
      findings = CodeSafety.check_rust_safety(
        ["src/lib.rs"],
        %{"src/lib.rs" => "#![forbid(unsafe_code)]\npub fn hello() {}"}
      )

      assert findings == []
    end

    test "ignores non-entry-point files" do
      findings = CodeSafety.check_rust_safety(
        ["src/utils.rs"],
        %{"src/utils.rs" => "pub fn helper() {}"}
      )

      assert findings == []
    end
  end

  describe "check_scm_locations/1" do
    test "flags SCM files outside .machine_readable/" do
      findings = CodeSafety.check_scm_locations([
        "repo/STATE.a2ml",
        "repo/.machine_readable/STATE.a2ml"
      ])

      # Only the root one should be flagged
      assert length(findings) == 1
      assert hd(findings).rule == :scm_wrong_location
    end

    test "passes when SCM files are in correct location" do
      findings = CodeSafety.check_scm_locations([
        "repo/.machine_readable/STATE.a2ml"
      ])

      assert findings == []
    end
  end

  describe "check_dockerfile_naming/1" do
    test "flags Dockerfile" do
      findings = CodeSafety.check_dockerfile_naming(["repo/Dockerfile"])

      assert length(findings) == 1
      assert hd(findings).rule == :dockerfile_not_containerfile
    end

    test "flags Dockerfile.dev" do
      findings = CodeSafety.check_dockerfile_naming(["repo/Dockerfile.dev"])

      assert length(findings) == 1
    end

    test "passes Containerfile" do
      findings = CodeSafety.check_dockerfile_naming(["repo/Containerfile"])

      assert findings == []
    end
  end
end

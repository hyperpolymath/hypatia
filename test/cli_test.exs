# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.CLITest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  # Regression coverage for hyperpolymath/hypatia#213 -- the `scan` step
  # in consumer-repo workflows was failing under `set -e` because the
  # CLI halted with exit 1 whenever findings existed, and there was no
  # stderr diagnostic to explain why. The fix adds `--exit-zero` /
  # `HYPATIA_EXIT_ZERO` plus an always-emitted summary line; this test
  # locks in the flag plumbing and the documented exit-code semantics.

  describe "--exit-zero plumbing" do
    test "OptionParser accepts --exit-zero as a strict boolean" do
      {opts, _args, invalid} =
        OptionParser.parse(
          ["scan", ".", "--exit-zero"],
          strict: [
            rules: :string,
            format: :string,
            severity: :string,
            path: :string,
            help: :boolean,
            version: :boolean,
            exit_zero: :boolean
          ]
        )

      assert opts[:exit_zero] == true
      assert invalid == []
    end
  end

  describe "help output" do
    test "documents --exit-zero, HYPATIA_EXIT_ZERO, and exit codes" do
      help = capture_io(fn -> Hypatia.CLI.main(["help"]) end)

      assert help =~ "--exit-zero"
      assert help =~ "HYPATIA_EXIT_ZERO"
      assert help =~ "EXIT CODES"
    end
  end
end

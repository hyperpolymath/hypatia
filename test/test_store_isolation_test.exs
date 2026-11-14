# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.TestStoreIsolationTest do
  use ExUnit.Case, async: true

  # The suite must not write outside the hypatia checkout. The fleet outcome
  # file (shared-context/learning/fix-outcomes.jsonl) belongs to the sibling
  # gitbot-fleet repository, so its path must be inside _build/test.
  test "the fleet path of the test environment is inside _build/test" do
    fleet = Path.expand(Hypatia.Paths.fleet())
    test_build = Path.expand("_build/test", File.cwd!())

    assert String.starts_with?(fleet, test_build <> "/"),
           "fleet path #{fleet} is outside #{test_build}"
  end
end

# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# The qualification gate is only as strong as the list of callers that may stamp
# "verified". Elixir has no module-private constructors, so that list is
# enforced here, as a fitness test over the source tree:
#
#   * `OutcomeTracker.record_fleet_outcome/7` is the trusted entry for a fleet
#     row that the scheduler has already qualified. It takes the verification
#     as given. Only `LearningScheduler` may call it.
#   * `OutcomeTracker.record_outcome/5` is the public entry. It rewrites a
#     caller's "verified" (E0-04), so any module may call it.
#
# A new caller of the trusted entry is not forbidden for ever. It must be added
# to the allowlist below in a reviewed change, with the reason, so that the
# decision is visible.
defmodule Hypatia.TrustedCallersTest do
  use ExUnit.Case, async: true

  # Modules that may call the trusted entry, and why.
  @trusted_entry_callers %{
    "lib/learning_scheduler.ex" =>
      "qualifies each fleet row (classify_fleet_record/1) before it stores it",
    "lib/outcome_tracker.ex" => "defines it"
  }

  defp code_lines(path) do
    path
    |> File.read!()
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.reject(fn {line, _} -> String.starts_with?(String.trim_leading(line), "#") end)
  end

  defp callers_of(pattern) do
    for path <- Path.wildcard("lib/**/*.ex"),
        {line, number} <- code_lines(path),
        Regex.match?(pattern, line),
        do: {path, number}
  end

  test "only the allowlisted modules call the trusted fleet entry" do
    # A call, not a mention in a doc string: the name followed by an open paren.
    calls = callers_of(~r/record_fleet_outcome\(/)
    files = calls |> Enum.map(&elem(&1, 0)) |> Enum.uniq() |> Enum.sort()

    unexpected = files -- Map.keys(@trusted_entry_callers)

    assert unexpected == [],
           "these modules call OutcomeTracker.record_fleet_outcome, the entry that does not " <>
             "guard \"verified\": #{inspect(unexpected)}. Use record_outcome/5, or add the " <>
             "module to @trusted_entry_callers in test/trusted_callers_test.exs with the reason."

    # The scheduler really does call it (the allowlist is not stale).
    assert "lib/learning_scheduler.ex" in files
  end

  test "persist_outcome/6, which writes a row without the guard, is private to OutcomeTracker" do
    files = callers_of(~r/persist_outcome\(/) |> Enum.map(&elem(&1, 0)) |> Enum.uniq()
    assert files == ["lib/outcome_tracker.ex"]

    refute function_exported?(Hypatia.OutcomeTracker, :persist_outcome, 6)
  end

  test "the public entry rewrites verified, so it is safe for any caller" do
    root = Path.join(System.tmp_dir!(), "trusted_callers_#{System.unique_integer([:positive])}")
    on_exit(fn -> File.rm_rf!(root) end)

    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    original = for key <- keys, do: {key, Application.fetch_env(:hypatia, key)}

    on_exit(fn ->
      for {key, value} <- original do
        case value do
          {:ok, previous} -> Application.put_env(:hypatia, key, previous)
          :error -> Application.delete_env(:hypatia, key)
        end
      end
    end)

    Application.put_env(:hypatia, :verisimdb_data_path, Path.join(root, "data"))
    Application.put_env(:hypatia, :annealing_state_path, Path.join(root, "annealing"))
    Application.put_env(:hypatia, :fleet_path, Path.join(root, "fleet"))

    assert {:ok, record} =
             Hypatia.OutcomeTracker.record_outcome(
               "recipe-trusted-callers",
               "o/r",
               "f",
               :success,
               %{
                 verification: "verified"
               }
             )

    refute record["verification"] == "verified"
  end
end

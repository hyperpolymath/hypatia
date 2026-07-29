# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.HexadecaContractTest do
  @moduledoc false
  use ExUnit.Case, async: true

  # Single-oracle drift guard for the hexadeca-connector wire contract.
  #
  # `ffi/connectors.json` is the golden source-of-truth for the 16-connector
  # surface. The Zig enum, the Idris2 ABI, and the Rust client each mirror it;
  # this test reads the golden plus all three source files and fails if any
  # mirror drifts in name or order. Wire ordering is load-bearing
  # (see ffi/zig/src/hexadeca.zig).

  @root Path.expand("..", __DIR__)
  @golden Path.join(@root, "ffi/connectors.json")

  @sources [
    {"Zig (hexadeca.zig)", Path.join(@root, "ffi/zig/src/hexadeca.zig"),
     ~r/\.\w+\s*=>\s*"([a-z0-9-]+)"/},
    {"Idris2 (Types.idr)", Path.join(@root, "src/abi/Types.idr"),
     ~r/connectorName\s+\w+\s*=\s*"([a-z0-9-]+)"/},
    {"Rust (connector.rs)", Path.join(@root, "clients/rust/hypatia-client/src/connector.rs"),
     ~r/Connector::\w+\s*=>\s*"([a-z0-9-]+)"/}
  ]

  defp golden_connectors do
    @golden |> File.read!() |> Jason.decode!() |> Map.fetch!("connectors")
  end

  defp names_in(path, regex) do
    regex |> Regex.scan(File.read!(path)) |> Enum.map(fn [_, name] -> name end)
  end

  test "golden fixture lists the 16 connectors with sequential wire ids" do
    conns = golden_connectors()
    assert length(conns) == 16
    assert Enum.map(conns, & &1["id"]) == Enum.to_list(0..15)
  end

  test "every language mirror matches the golden in name and order" do
    golden_names = Enum.map(golden_connectors(), & &1["name"])

    for {label, path, regex} <- @sources do
      names = names_in(path, regex)

      assert length(names) == 16,
             "#{label}: expected 16 connector names, found #{length(names)}"

      assert names == golden_names,
             "#{label} drifted from ffi/connectors.json\n  golden: #{inspect(golden_names)}\n  found:  #{inspect(names)}"
    end
  end
end

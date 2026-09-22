# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.UnifiedApiAdapterContractTest do
  @moduledoc false
  use ExUnit.Case, async: true

  # Drift guard for the unified-api-adapter wire contract.
  #
  # The golden source is the **normative Idris2 ABI**, `src/Hypatia/ABI/Types.idr`
  # -- the module that `src/abi/hypatia-abi.ipkg` actually compiles (its
  # `sourcedir = ".."` resolves to `src/`). This guard previously read
  # `src/abi/Types.idr`, a byte-identical copy that is compiled by nothing: the
  # live ABI could drift and the guard would have stayed green.
  #
  # The three sources below are emitted from that golden by `just abi-gen`, so
  # in normal operation they cannot disagree. This test is still worth running
  # because it checks generated output against the source by a **different
  # mechanism** -- regex scraping here, evaluation in the generator. Four
  # generated files agreeing with each other would be vacuous; generated-vs-
  # source by an independent method catches a bug in the generator itself.

  @root Path.expand("..", __DIR__)

  @golden_path Path.join(@root, "src/Hypatia/ABI/Types.idr")
  @golden_regex ~r/connectorName\s+\w+\s*=\s*"([a-z0-9-]+)"/

  # Names alone do not pin the wire contract. Swapping two ids in
  # `connectorWireId` leaves every name and their order untouched, so a
  # name-only guard stays green through a C ABI break -- measured, not
  # supposed: that exact mutant passed this file before these two lines
  # existed. Scrape the constructor -> id and constructor -> name maps and
  # compare the pair.
  @golden_id_regex ~r/connectorWireId\s+(\w+)\s*=\s*(\d+)/
  @golden_ctor_name_regex ~r/connectorName\s+(\w+)\s*=\s*"([a-z0-9-]+)"/

  @sources [
    {"Zig (connector_generated.zig)", Path.join(@root, "ffi/zig/src/connector_generated.zig"),
     ~r/\.\w+\s*=>\s*"([a-z0-9-]+)"/},
    {"Rust (connector_generated.rs)",
     Path.join(@root, "clients/rust/hypatia-client/src/connector_generated.rs"),
     ~r/Self::\w+\s*=>\s*"([a-z0-9-]+)"/},
    {"JSON (ffi/connectors.json)", Path.join(@root, "ffi/connectors.json"),
     ~r/"name":\s*"([a-z0-9-]+)"/}
  ]

  defp names_in(path, regex) do
    regex |> Regex.scan(File.read!(path)) |> Enum.map(fn [_, name] -> name end)
  end

  defp golden_names, do: names_in(@golden_path, @golden_regex)

  # name => wire id, derived from the golden by joining the two Idris2
  # functions on their shared constructor.
  defp golden_ids do
    src = File.read!(@golden_path)

    ctor_to_name =
      @golden_ctor_name_regex
      |> Regex.scan(src)
      |> Map.new(fn [_, ctor, name] -> {ctor, name} end)

    @golden_id_regex
    |> Regex.scan(src)
    |> Enum.flat_map(fn [_, ctor, id] ->
      case Map.fetch(ctor_to_name, ctor) do
        {:ok, name} -> [{name, String.to_integer(id)}]
        :error -> []
      end
    end)
    |> Map.new()
  end

  test "the golden ABI yields a non-empty connector list" do
    # The denominator. A guard that cannot state how much it compared is not
    # evidence: if this regex ever stops matching, every comparison below
    # succeeds vacuously against an empty list.
    names = golden_names()
    assert names != [], "no connector names scraped from #{@golden_path}"
    assert length(names) == length(Enum.uniq(names)), "duplicate connector names in the ABI"
  end

  test "every generated mirror matches the golden ABI in name and order" do
    golden = golden_names()

    for {label, path, regex} <- @sources do
      assert File.exists?(path), "#{label}: generated file missing at #{path}"
      names = names_in(path, regex)

      assert names == golden,
             "#{label} drifted from src/Hypatia/ABI/Types.idr -- run `just abi-gen`" <>
               "\n  golden: #{inspect(golden)}\n  found:  #{inspect(names)}"
    end
  end

  test "the JSON manifest numbers connectors sequentially from zero" do
    conns =
      Path.join(@root, "ffi/connectors.json") |> File.read!() |> Jason.decode!()
      |> Map.fetch!("connectors")

    # Derived from the golden, never a hand-written 16: the count is pinned by
    # `connectorCount = Refl`, the Zig comptime assertion and Rust's
    # `count_is_sixteen`. A sixth hand-written literal would be drift surface.
    n = length(golden_names())
    assert length(conns) == n
    assert Enum.map(conns, & &1["id"]) == Enum.to_list(0..(n - 1))
  end

  test "the JSON manifest wire ids match the golden ABI connectorWireId" do
    ids = golden_ids()

    # Denominator first: a join that silently produced an empty map would
    # make every assertion below vacuous.
    assert map_size(ids) == length(golden_names()),
           "scraped #{map_size(ids)} wire ids but #{length(golden_names())} names from #{@golden_path}"

    conns =
      Path.join(@root, "ffi/connectors.json") |> File.read!() |> Jason.decode!()
      |> Map.fetch!("connectors")

    for %{"id" => id, "name" => name} <- conns do
      assert Map.fetch!(ids, name) == id,
             "ffi/connectors.json gives #{name} id #{id}, the ABI gives #{Map.fetch!(ids, name)}"
    end
  end
end

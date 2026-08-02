# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.LeaseValidatorTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.LeaseValidator

  @now ~U[2026-06-14 12:00:00Z]

  defp lease(o) do
    Map.merge(
      %{
        "lease_id" => "L",
        "holder" => "hypatia",
        "repo" => "hyperpolymath/a",
        "territory" => %{"branch" => "b", "paths" => [], "is_meta" => false},
        "status" => "held",
        "acquired_at" => "2026-06-14T11:00:00Z",
        "expires_at" => "2026-06-14T13:00:00Z",
        "owner_authorized" => false
      },
      o
    )
  end

  test "a clean store has no violations" do
    assert LeaseValidator.validate([lease(%{})], @now) == []
  end

  test "LE1: a held lease without a TTL is flagged" do
    assert {:le1_no_ttl, "x"} in LeaseValidator.validate(
             [lease(%{"lease_id" => "x", "expires_at" => nil})],
             @now
           )
  end

  test "LE2: a held meta lease without owner authorization is flagged; an authorized one is clean" do
    meta = lease(%{"lease_id" => "m", "territory" => %{"is_meta" => true}})
    assert {:le2_meta_unauthorized, "m"} in LeaseValidator.validate([meta], @now)

    ok =
      lease(%{"lease_id" => "m2", "territory" => %{"is_meta" => true}, "owner_authorized" => true})

    assert LeaseValidator.validate([ok], @now) == []
  end

  test "stale: a held lease past its TTL is flagged" do
    stale = lease(%{"lease_id" => "s", "expires_at" => "2026-06-14T11:30:00Z"})
    assert {:stale, "s"} in LeaseValidator.validate([stale], @now)
  end

  test "overlap: two live held leases on one repo are flagged" do
    v =
      LeaseValidator.validate([lease(%{"lease_id" => "a1"}), lease(%{"lease_id" => "a2"})], @now)

    assert [{:overlap, "hyperpolymath/a", ids}] = v
    assert Enum.sort(ids) == ["a1", "a2"]
  end

  test "different repos, and released/expired leases, do not overlap" do
    leases = [
      lease(%{"lease_id" => "a1", "repo" => "hyperpolymath/a"}),
      lease(%{"lease_id" => "b1", "repo" => "hyperpolymath/b"}),
      lease(%{"lease_id" => "r", "status" => "released"})
    ]

    assert LeaseValidator.validate(leases, @now) == []
  end

  test "validate_store reads the lease dir and validates (injected decode)" do
    dir = Path.join(System.tmp_dir!(), "lv-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    enc = fn t -> t |> :erlang.term_to_binary() |> Base.encode64() end
    dec = fn b -> b |> Base.decode64!() |> :erlang.binary_to_term() end
    File.write!(Path.join(dir, "a.json"), enc.(lease(%{"lease_id" => "a1"})))
    # same repo as a1 → an overlap the live gate can't see across files
    File.write!(Path.join(dir, "b.json"), enc.(lease(%{"lease_id" => "a2"})))

    assert [{:overlap, "hyperpolymath/a", _}] =
             LeaseValidator.validate_store(dir, decode: dec, now: @now)

    File.rm_rf!(dir)
  end
end

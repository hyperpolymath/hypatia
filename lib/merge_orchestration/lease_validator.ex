# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.LeaseValidator do
  @moduledoc """
  The lease-store auditor — the "k9 lease validator" (a5 follow-on).

  `KinGate.decide_acquire/3` prevents *bad acquires* at mint time. This validates
  the *persisted* lease store after the fact, catching drift a live gate can't:
  the a5 schema invariants plus the two coordination hazards the spec names.

  `validate/2` is pure over a list of a5 lease records and returns a list of
  violations (empty ⇒ clean):

    * `{:le1_no_ttl, lease_id}`            — a `held` lease with no `expires_at` (LE1)
    * `{:le2_meta_unauthorized, lease_id}` — a `held` meta-territory lease without
                                             `owner_authorized` (LE2)
    * `{:stale, lease_id}`                 — a `held` lease past its TTL (should have
                                             released/expired; coordination drift)
    * `{:overlap, repo, [lease_id]}`       — two or more *live* `held` leases on the
                                             same repo (the "one bot per repo" rule
                                             broken in the store)

  `validate_store/2` reads the lease dir via `KinGate.FileStore` and validates it;
  the `mix hypatia.validate_leases` task is the runnable CI gate over it.
  """

  alias Hypatia.MergeOrchestration.KinGate
  alias Hypatia.MergeOrchestration.KinGate.FileStore

  @doc "Validate a list of a5 lease records at `now`. Returns violations (empty ⇒ clean)."
  def validate(leases, now \\ DateTime.utc_now()) when is_list(leases) do
    held = Enum.filter(leases, &(&1["status"] == "held"))
    le1(held) ++ le2(held) ++ stale(held, now) ++ overlap(held, now)
  end

  @doc "Read the lease store dir (`KinGate.FileStore`) and validate it. `opts`: `:decode`, `:now`."
  def validate_store(dir, opts \\ []) do
    decode = Keyword.get(opts, :decode, &FileStore.json_decode!/1)
    now = Keyword.get(opts, :now, DateTime.utc_now())
    dir |> FileStore.list(decode: decode) |> validate(now)
  end

  # LE1 — a held lease must carry a TTL
  defp le1(held), do: for(l <- held, blank?(l["expires_at"]), do: {:le1_no_ttl, id(l)})

  # LE2 — a held meta-territory claim must be owner-authorized
  defp le2(held),
    do: for(l <- held, meta?(l), not owner_authorized?(l), do: {:le2_meta_unauthorized, id(l)})

  # stale — a held lease (with a TTL) that is past it; LE1 already covers no-TTL
  defp stale(held, now),
    do: for(l <- held, not blank?(l["expires_at"]), KinGate.expired?(l, now), do: {:stale, id(l)})

  # overlap — two or more live held leases on one repo (the per-repo rule, broken in the store)
  defp overlap(held, now) do
    held
    |> Enum.reject(&KinGate.expired?(&1, now))
    |> Enum.group_by(& &1["repo"])
    |> Enum.filter(fn {_repo, ls} -> length(ls) > 1 end)
    |> Enum.map(fn {repo, ls} -> {:overlap, repo, Enum.map(ls, &id/1)} end)
  end

  defp blank?(nil), do: true
  defp blank?(""), do: true
  defp blank?(_), do: false
  defp meta?(l), do: get_in(l, ["territory", "is_meta"]) == true
  defp owner_authorized?(l), do: l["owner_authorized"] == true
  defp id(l), do: l["lease_id"]
end

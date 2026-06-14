# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.KinGate.FileStore do
  @moduledoc """
  Filesystem persistence for `KinGate` leases — the central store made concrete.

  One JSON file per repo (`<owner__name>.json`) *is* the per-repo lock. The
  contended first-acquire goes through an `O_EXCL` create (`File.open(_,
  [:write, :exclusive])`) so two processes racing an empty repo cannot both
  win — the loser re-evaluates against the winner's record and gets a
  `:conflict`. Subsequent acquires are decision-gated by `KinGate.decide_acquire/3`
  (renew / takeover-expired / conflict).

  `opts[:encode]` / `opts[:decode]` default to Jason (a real hypatia dep) so the
  on-disk record is the a5 `CoordinationLease` shape every tool reads; both are
  injectable so the logic tests run dependency-free. `opts[:now]` overrides the
  clock for deterministic tests.

  Single-node atomicity is real (the `O_EXCL` create). Cross-machine exclusion
  over a git-backed store is the push-conflict layer above this — out of scope
  for the store itself.
  """

  alias Hypatia.MergeOrchestration.KinGate

  @doc "Acquire (mint/renew/takeover) the lease for `request.repo`. See `KinGate.decide_acquire/3`."
  def acquire(dir, request, opts \\ []) do
    now = Keyword.get(opts, :now, DateTime.utc_now())
    encode = Keyword.get(opts, :encode, &json_encode!/1)
    decode = Keyword.get(opts, :decode, &json_decode!/1)
    File.mkdir_p!(dir)
    path = repo_path(dir, request.repo)

    case File.read(path) do
      {:ok, body} ->
        case KinGate.decide_acquire([decode.(body)], request, now) do
          {:ok, lease} ->
            File.write!(path, encode.(lease))
            {:ok, lease}

          other ->
            other
        end

      {:error, :enoent} ->
        case KinGate.decide_acquire([], request, now) do
          {:ok, lease} -> create_exclusive(dir, request, lease, opts, encode)
          other -> other
        end
    end
  end

  @doc "Release the repo's lease — frees the per-repo lock. Returns `{:ok, released_lease}`."
  def release(dir, repo, opts \\ []) do
    decode = Keyword.get(opts, :decode, &json_decode!/1)
    path = repo_path(dir, repo)

    case File.read(path) do
      {:ok, body} ->
        released = KinGate.release(decode.(body))
        File.rm(path)
        {:ok, released}

      _ ->
        {:error, :not_held}
    end
  end

  @doc "Every lease record currently in the store."
  def list(dir, opts \\ []) do
    decode = Keyword.get(opts, :decode, &json_decode!/1)

    case File.ls(dir) do
      {:ok, names} ->
        names
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.map(&decode.(File.read!(Path.join(dir, &1))))

      _ ->
        []
    end
  end

  defp create_exclusive(dir, request, lease, opts, encode) do
    path = repo_path(dir, request.repo)

    case File.open(path, [:write, :exclusive]) do
      {:ok, io} ->
        IO.binwrite(io, encode.(lease))
        File.close(io)
        {:ok, lease}

      {:error, :eexist} ->
        # Lost the create race — re-evaluate against whoever won.
        acquire(dir, request, opts)
    end
  end

  defp repo_path(dir, repo), do: Path.join(dir, String.replace(repo, "/", "__") <> ".json")

  @doc false
  def json_encode!(term), do: apply(Jason, :encode!, [term])
  @doc false
  def json_decode!(body), do: apply(Jason, :decode!, [body])
end

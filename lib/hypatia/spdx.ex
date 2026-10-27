# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.SPDX do
  @moduledoc """
  SPDX licence identifiers, from a vendored copy of the SPDX License List.

  The list lives in `priv/spdx/license-ids.txt` and is read at compile time,
  so the escript carries it without needing `priv/` at run time. Refresh it
  with `scripts/refresh-spdx-license-ids.sh <tag>` when SPDX publishes a new
  release; the first line of the file names the version in use.
  """

  @list_path Path.expand("../../priv/spdx/license-ids.txt", __DIR__)
  @external_resource @list_path

  {header, ids} =
    @list_path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.split_with(&String.starts_with?(&1, "#"))

  if length(ids) < 500 do
    raise "#{@list_path} holds only #{length(ids)} identifiers; regenerate it"
  end

  @ids MapSet.new(ids)
  @version_line hd(header)

  @doc """
  Whether `id` is an SPDX licence identifier on the vendored list, or a
  REUSE custom licence (`LicenseRef-` followed by letters, digits, `.` or
  `-`). Matching is exact, as REUSE requires for `LICENSES/` file names.
  Deprecated identifiers count: they still name a licence.
  """
  def licence_id?(id) when is_binary(id) do
    MapSet.member?(@ids, id) or Regex.match?(~r/\ALicenseRef-[A-Za-z0-9.\-]+\z/, id)
  end

  @doc "The header line naming the SPDX License List version in use."
  def list_version, do: @version_line
end

# SPDX-License-Identifier: MPL-2.0
# Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.Rules.StandardsRepo do
  @moduledoc """
  The names of the estate standards repository.

  `hyperpolymath/standards` moved to `metadatastician/standards` on
  2026-10-25. GitHub redirects git and the REST API for the old name, but not
  a reusable workflow's `uses:`: a caller still naming the old repository ends
  in `startup_failure` ("workflow was not found"). Callers, lock records and
  merge events under the old name stay in the estate until each repository is
  re-pointed, so every rule that keys on the standards repository accepts
  both names. The current name comes first.
  """

  @standards_move Enum.find(Hypatia.Rules.MovedRepositories.entries(), fn entry ->
                    entry.repo_id == 1_116_521_501
                  end)
  @current @standards_move.current
  @former [@standards_move.former]

  @doc "The repository's current name."
  def current, do: @current

  @doc "The names the repository was known by before, most recent first."
  def former, do: @former

  @doc "Every name the repository is known by, current first."
  def names, do: [@current | @former]

  @doc "True when `repo` (`owner/name`, any case) is the standards repository."
  def name?(repo) when is_binary(repo) do
    downcased = String.downcase(repo)
    Enum.any?(names(), &(&1 == downcased))
  end

  def name?(_), do: false

  @doc """
  Regex source for `path` (`owner/repo/rest`) that matches it under any name
  of the standards repository, the `owner/repo` part case-insensitively (as
  GitHub resolves it). A path outside the standards repository is escaped
  as it stands.
  """
  def path_regex_source(path) when is_binary(path) do
    case String.split(path, "/", parts: 3) do
      [owner, repo, rest] ->
        if name?(owner <> "/" <> repo) do
          alternation = Enum.map_join(names(), "|", &Regex.escape/1)
          "(?i:(?:#{alternation}))/#{Regex.escape(rest)}"
        else
          Regex.escape(path)
        end

      _ ->
        Regex.escape(path)
    end
  end
end

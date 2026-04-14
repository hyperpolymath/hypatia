# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.ForgeAdapters do
  @moduledoc """
  Primary Elixir forge adapter rules (migrated from legacy Logtalk engine).
  """
  Provides validated operations against multiple forges (GitHub, GitLab, Bitbucket).
  Enforces input validation to prevent shell injection.
  """

  @supported_forges [:github, :gitlab, :bitbucket, :codeberg, :sourcehut, :gitea, :radicle]

  @dangerous_chars ~c[;|&$`(){}\\<>\'"]

  def supported_forges, do: @supported_forges

  def validate_name(name) when is_binary(name) do
    cond do
      String.length(name) == 0 ->
        {:error, :empty_name}
      Enum.any?(@dangerous_chars, &String.contains?(name, <<&1>>)) ->
        {:error, :dangerous_characters}
      not Regex.match?(~r/^[a-zA-Z0-9_\-\.]+$/, name) ->
        {:error, :invalid_characters}
      true ->
        :ok
    end
  end

  def get_repos_command(:github, org) do
    with :ok <- validate_name(org) do
      {:ok, "gh repo list #{org} --limit 500 --json name,visibility,defaultBranchRef"}
    end
  end

  def get_repos_command(:gitlab, org) do
    with :ok <- validate_name(org) do
      {:ok, "glab repo list -g #{org}"}
    end
  end

  def get_repos_command(:bitbucket, org) do
    with :ok <- validate_name(org) do
      {:ok, "curl -s 'https://api.bitbucket.org/2.0/repositories/#{org}?pagelen=100'"}
    end
  end

  def get_repos_command(forge, _org), do: {:error, {:unsupported_forge, forge}}

  def get_alerts_command(:github, org, repo) do
    with :ok <- validate_name(org),
         :ok <- validate_name(repo) do
      {:ok, "gh api repos/#{org}/#{repo}/code-scanning/alerts --paginate"}
    end
  end

  def get_alerts_command(:gitlab, org, repo) do
    with :ok <- validate_name(org),
         :ok <- validate_name(repo) do
      {:ok, "glab api projects/#{org}%2F#{repo}/vulnerability_findings"}
    end
  end

  def get_alerts_command(forge, _org, _repo), do: {:error, {:unsupported_forge, forge}}

  def enable_branch_protection_command(:github, repo) do
    with :ok <- validate_name(repo) do
      {:ok, """
      gh api repos/hyperpolymath/#{repo}/branches/main/protection \
        -X PUT \
        -F required_pull_request_reviews='{"required_approving_review_count":1}' \
        -F enforce_admins=false \
        -F restrictions=null \
        -F required_status_checks=null
      """}
    end
  end

  def enable_branch_protection_command(forge, _repo), do: {:error, {:unsupported_forge, forge}}
end

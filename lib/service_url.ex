# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.ServiceUrl do
  @moduledoc """
  The base URLs of the external services hypatia calls, read from the
  environment.

  There are no built-in defaults. A service whose variable is unset, empty or
  whitespace-only is **not configured**: `from_env/1` returns
  `{:error, :not_configured}`, and every caller treats that as "this feature
  is off". It makes no network call, and returns the same empty or failure
  value it returns when the service is unreachable. Nothing fails at boot.

  | Variable | Service | Meaning |
  |---|---|---|
  | `HYPATIA_VERISIM_URL` | verisim-api | base URL; callers append `/api/v1/...` |
  | `HYPATIA_ECHIDNABOT_URL` | echidnabot | base URL; callers append `/graphql` |
  """

  @verisim_env "HYPATIA_VERISIM_URL"
  @echidnabot_env "HYPATIA_ECHIDNABOT_URL"

  @doc """
  Return the verisim-api base URL.

  A non-blank `:base_url` option wins. Otherwise the URL comes from
  `HYPATIA_VERISIM_URL`. Returns `{:error, :not_configured}` when neither
  holds a value.
  """
  @spec verisim(keyword()) :: {:ok, String.t()} | {:error, :not_configured}
  def verisim(opts \\ []) do
    case present(Keyword.get(opts, :base_url)) do
      {:ok, url} -> {:ok, url}
      {:error, :not_configured} -> from_env(@verisim_env)
    end
  end

  @doc """
  Return echidnabot's base URL from `HYPATIA_ECHIDNABOT_URL`, or
  `{:error, :not_configured}` when it is unset or blank.
  """
  @spec echidnabot() :: {:ok, String.t()} | {:error, :not_configured}
  def echidnabot, do: from_env(@echidnabot_env)

  @doc """
  Read a service URL from the environment variable `name`.

  Returns `{:ok, url}`, with surrounding whitespace removed, or
  `{:error, :not_configured}` when the variable is unset, empty or
  whitespace-only.
  """
  @spec from_env(String.t()) :: {:ok, String.t()} | {:error, :not_configured}
  def from_env(name) when is_binary(name), do: present(System.get_env(name))

  # Normalise one candidate URL: a non-blank binary is configured, anything
  # else is not.
  defp present(value) when is_binary(value) do
    case String.trim(value) do
      "" -> {:error, :not_configured}
      url -> {:ok, url}
    end
  end

  defp present(_), do: {:error, :not_configured}
end

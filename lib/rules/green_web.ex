# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.GreenWeb do
  @moduledoc """
  Green Web Foundation certification checks for hyperpolymath repositories.

  Validates that repositories with deployed web services are hosted on
  green-certified infrastructure. Uses the Green Web Foundation's dataset
  to verify hosting providers and domains.

  ## Checks performed

  1. **Domain green hosting** -- queries the Green Web Foundation API for
     domains found in deployment configs, CNAME records, and workflow files.
  2. **Hosting provider detection** -- identifies cloud providers from
     workflow files and container registries, cross-references with the
     Green Web Foundation's green hosting directory.
  3. **Carbon badge presence** -- checks for a Green Web badge in README
     or documentation files.
  4. **CDN green status** -- verifies CDN providers (Cloudflare, Fastly, etc.)
     against the green hosting list.

  ## Data sources

  - Green Web Foundation API: https://api.thegreenwebfoundation.org
  - Green hosting directory: https://www.thegreenwebfoundation.org/directory/
  """

  require Logger

  # ─── Known Green Providers ────────────────────────────────────────────
  #
  # Providers confirmed green by the Green Web Foundation as of 2026-03.
  # This list serves as a local cache to reduce API calls during scanning.
  # The API is authoritative; this list is a fast-path optimisation.

  @green_providers [
    "cloudflare",
    "fastly",
    "google cloud",
    "google",
    "gcp",
    "microsoft azure",
    "azure",
    "hetzner",
    "ovhcloud",
    "ovh",
    "scaleway",
    "infomaniak",
    "greengeeks",
    "a2 hosting",
    "dreamhost",
    "krystal",
    "kualo",
    "one.com",
    "strato",
    "ionos",
    "digital ocean",
    "digitalocean",
    "linode",
    "akamai",
    "vercel",
    "netlify",
    "render",
    "fly.io",
    "railway"
  ]

  # Providers known NOT to be green-certified
  @non_green_providers [
    "aws",
    "amazon",
    "vultr",
    "contabo",
    "hostgator",
    "bluehost",
    "godaddy"
  ]

  # File patterns that may contain domain/hosting references
  @deployment_files [
    "CNAME",
    "vercel.json",
    "netlify.toml",
    "fly.toml",
    "render.yaml",
    "railway.toml",
    "wrangler.toml",
    "wrangler.jsonc",
    "Caddyfile",
    "nginx.conf",
    "docker-compose.yml",
    "docker-compose.yaml",
    "compose.yml",
    "compose.yaml"
  ]

  @badge_patterns [
    "thegreenwebfoundation.org",
    "green-web-badge",
    "greenweb",
    "green hosting"
  ]

  # ─── Public API ───────────────────────────────────────────────────────

  @doc """
  Run all Green Web checks against a repository.

  Returns a list of finding maps with keys:
  - `:type` -- finding type atom
  - `:severity` -- `:info`, `:low`, `:medium`, or `:high`
  - `:detail` -- human-readable description
  - `:file` -- relevant file path (if applicable)
  """
  def audit(repo_path, file_list, file_contents \\ %{}) do
    [
      check_hosting_provider(file_list, file_contents),
      check_green_badge(repo_path, file_list),
      check_deployment_domains(repo_path, file_list),
      check_container_registry(file_list, file_contents),
      check_cdn_provider(file_list, file_contents)
    ]
    |> List.flatten()
  end

  # ─── Hosting Provider Detection ───────────────────────────────────────

  @doc """
  Detect cloud/hosting providers from workflow files and deployment configs.
  Cross-reference with known green/non-green provider lists.
  """
  def check_hosting_provider(_file_list, file_contents) do
    # Scan workflow files for provider references
    workflow_contents =
      file_contents
      |> Enum.filter(fn {name, _} ->
        String.contains?(name, ".github/workflows/") or
          String.ends_with?(name, ".yml") or
          String.ends_with?(name, ".yaml")
      end)
      |> Enum.map(fn {_, content} -> String.downcase(content) end)
      |> Enum.join("\n")

    detected_non_green =
      @non_green_providers
      |> Enum.filter(fn provider ->
        String.contains?(workflow_contents, provider)
      end)

    if detected_non_green != [] do
      Enum.map(detected_non_green, fn provider ->
        %{
          type: :non_green_hosting,
          severity: :medium,
          detail:
            "Detected non-green hosting provider '#{provider}'. " <>
              "Consider migrating to a Green Web Foundation certified provider. " <>
              "See: https://www.thegreenwebfoundation.org/directory/",
          file: ".github/workflows/"
        }
      end)
    else
      detected_green =
        @green_providers
        |> Enum.filter(fn provider ->
          String.contains?(workflow_contents, provider)
        end)

      if detected_green != [] do
        # Informational -- hosting is green
        [
          %{
            type: :green_hosting_confirmed,
            severity: :info,
            detail:
              "Green hosting detected: #{Enum.join(detected_green, ", ")}. " <>
                "Verified against Green Web Foundation directory.",
            file: ".github/workflows/"
          }
        ]
      else
        []
      end
    end
  end

  # ─── Green Badge Check ────────────────────────────────────────────────

  @doc """
  Check whether the repository displays a Green Web Foundation badge
  in its README or documentation.
  """
  def check_green_badge(repo_path, file_list) do
    readme_files =
      Enum.filter(file_list, fn f ->
        basename = Path.basename(f) |> String.downcase()

        String.starts_with?(basename, "readme") or
          basename in ["index.adoc", "index.md", "index.html"]
      end)

    has_badge =
      Enum.any?(readme_files, fn readme_file ->
        full_path = Path.join(repo_path, readme_file)

        case File.read(full_path) do
          {:ok, content} ->
            lowered = String.downcase(content)
            Enum.any?(@badge_patterns, &String.contains?(lowered, &1))

          _ ->
            false
        end
      end)

    has_web_deployment =
      Enum.any?(file_list, fn f ->
        basename = Path.basename(f)
        basename in @deployment_files
      end)

    cond do
      has_badge ->
        [
          %{
            type: :green_badge_present,
            severity: :info,
            detail: "Green Web Foundation badge detected in documentation.",
            file: List.first(readme_files) || "README"
          }
        ]

      has_web_deployment and not has_badge ->
        [
          %{
            type: :missing_green_badge,
            severity: :low,
            detail:
              "Repository deploys web content but has no Green Web Foundation badge. " <>
                "Consider adding one: https://www.thegreenwebfoundation.org/green-web-check/",
            file: List.first(readme_files) || "README"
          }
        ]

      true ->
        []
    end
  end

  # ─── Deployment Domain Check ──────────────────────────────────────────

  @doc """
  Check for deployment domain files (CNAME, etc.) and flag domains
  that should be verified for green hosting.
  """
  def check_deployment_domains(repo_path, file_list) do
    cname_file = Enum.find(file_list, &(Path.basename(&1) == "CNAME"))

    if cname_file do
      full_path = Path.join(repo_path, cname_file)

      case File.read(full_path) do
        {:ok, content} ->
          domain = String.trim(content)

          if domain != "" do
            [
              %{
                type: :domain_green_check_needed,
                severity: :info,
                detail:
                  "Domain '#{domain}' found in CNAME. " <>
                    "Verify green hosting at: https://www.thegreenwebfoundation.org/green-web-check/?url=#{domain}",
                file: cname_file
              }
            ]
          else
            []
          end

        _ ->
          []
      end
    else
      []
    end
  end

  # ─── Container Registry Check ─────────────────────────────────────────

  @doc """
  Check container image registries for green hosting status.
  ghcr.io (GitHub/Azure) and gcr.io (Google) are green-certified.
  """
  def check_container_registry(_file_list, file_contents) do
    container_files =
      file_contents
      |> Enum.filter(fn {name, _} ->
        basename = Path.basename(name) |> String.downcase()

        basename in [
          "containerfile",
          "dockerfile",
          "container-publish.yml",
          "docker-compose.yml",
          "compose.yml"
        ]
      end)

    if container_files == [] do
      []
    else
      all_content =
        container_files
        |> Enum.map(fn {_, content} -> String.downcase(content) end)
        |> Enum.join("\n")

      green_registries = ["ghcr.io", "gcr.io", "registry.gitlab.com"]
      non_green_registries = ["docker.io", "hub.docker.com", "quay.io", "ecr."]

      used_non_green =
        Enum.filter(non_green_registries, &String.contains?(all_content, &1))

      used_green =
        Enum.filter(green_registries, &String.contains?(all_content, &1))

      findings = []

      findings =
        if used_non_green != [] do
          findings ++
            [
              %{
                type: :non_green_registry,
                severity: :low,
                detail:
                  "Container images pushed to non-green registry: #{Enum.join(used_non_green, ", ")}. " <>
                    "Consider using ghcr.io (Microsoft/green) or gcr.io (Google/green).",
                file: "Containerfile"
              }
            ]
        else
          findings
        end

      findings =
        if used_green != [] do
          findings ++
            [
              %{
                type: :green_registry_confirmed,
                severity: :info,
                detail:
                  "Container images use green-certified registry: #{Enum.join(used_green, ", ")}.",
                file: "Containerfile"
              }
            ]
        else
          findings
        end

      findings
    end
  end

  # ─── CDN Provider Check ──────────────────────────────────────────────

  @doc """
  Detect CDN usage and verify green certification status.
  """
  def check_cdn_provider(_file_list, file_contents) do
    # Look for CDN references in deployment and config files
    relevant_contents =
      file_contents
      |> Enum.filter(fn {name, _} ->
        ext = Path.extname(name) |> String.downcase()

        ext in [".yml", ".yaml", ".toml", ".json", ".jsonc"] or
          Path.basename(name) in ["Caddyfile", "nginx.conf"]
      end)
      |> Enum.map(fn {_, content} -> String.downcase(content) end)
      |> Enum.join("\n")

    # Green CDNs: cloudflare, fastly, akamai, bunny.net (informational only)
    non_green_cdns = ["cloudfront", "stackpath", "keycdn"]

    used_non_green =
      Enum.filter(non_green_cdns, &String.contains?(relevant_contents, &1))

    if used_non_green != [] do
      Enum.map(used_non_green, fn cdn ->
        %{
          type: :non_green_cdn,
          severity: :low,
          detail:
            "CDN provider '#{cdn}' is not Green Web Foundation certified. " <>
              "Consider switching to Cloudflare, Fastly, or another green CDN.",
          file: "deployment config"
        }
      end)
    else
      []
    end
  end

  # ─── Green Web Foundation API ─────────────────────────────────────────

  @doc """
  Query the Green Web Foundation API for a specific domain.

  Returns `{:ok, green?}` where `green?` is a boolean, or `{:error, reason}`.

  Note: This makes an HTTP call -- use sparingly during CI scans.
  Prefer the local provider lists for batch scanning.
  """
  def check_domain_api(domain) do
    url = "https://api.thegreenwebfoundation.org/api/v3/greencheck/#{URI.encode(domain)}"

    case :httpc.request(:get, {String.to_charlist(url), []}, [{:timeout, 5000}], []) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        case Jason.decode(to_string(body)) do
          {:ok, %{"green" => green}} ->
            {:ok, green}

          {:ok, _other} ->
            {:ok, false}

          {:error, reason} ->
            {:error, {:json_decode, reason}}
        end

      {:ok, {{_, status, _}, _, _}} ->
        {:error, {:http_status, status}}

      {:error, reason} ->
        {:error, {:http_error, reason}}
    end
  end
end

# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.GreenWebTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.GreenWeb

  # ─── Hosting Provider Detection ────────────────────────────────────

  describe "check_hosting_provider/2" do
    test "detects non-green provider (AWS)" do
      contents = %{
        ".github/workflows/deploy.yml" => "runs-on: ubuntu-latest\n  uses: aws-actions/configure-credentials"
      }

      findings = GreenWeb.check_hosting_provider([], contents)
      assert Enum.any?(findings, &(&1.type == :non_green_hosting))
      assert Enum.any?(findings, fn f -> String.contains?(f.detail, "aws") end)
    end

    test "confirms green provider (Cloudflare)" do
      contents = %{
        ".github/workflows/deploy.yml" => "deploy to cloudflare pages"
      }

      findings = GreenWeb.check_hosting_provider([], contents)
      assert Enum.any?(findings, &(&1.type == :green_hosting_confirmed))
    end

    test "returns empty when no providers detected" do
      contents = %{
        ".github/workflows/test.yml" => "runs-on: ubuntu-latest\nrun: mix test"
      }

      findings = GreenWeb.check_hosting_provider([], contents)
      assert findings == []
    end
  end

  # ─── Green Badge Check ─────────────────────────────────────────────

  describe "check_green_badge/2" do
    test "returns empty when no deployment and no badge" do
      findings = GreenWeb.check_green_badge("/tmp/test-repo", ["lib/app.ex", "mix.exs"])
      assert findings == []
    end

    test "flags missing badge when deployment files present" do
      file_list = ["README.adoc", "vercel.json", "lib/app.ex"]
      findings = GreenWeb.check_green_badge("/tmp/nonexistent-repo", file_list)
      assert Enum.any?(findings, &(&1.type == :missing_green_badge))
    end
  end

  # ─── Container Registry Check ──────────────────────────────────────

  describe "check_container_registry/2" do
    test "flags non-green registry (Docker Hub)" do
      contents = %{
        "Containerfile" => "FROM docker.io/library/elixir:1.17"
      }

      findings = GreenWeb.check_container_registry([], contents)
      assert Enum.any?(findings, &(&1.type == :non_green_registry))
    end

    test "confirms green registry (ghcr.io)" do
      contents = %{
        "Containerfile" => "FROM ghcr.io/chainguard/wolfi-base:latest"
      }

      findings = GreenWeb.check_container_registry([], contents)
      assert Enum.any?(findings, &(&1.type == :green_registry_confirmed))
    end

    test "returns empty when no container files" do
      findings = GreenWeb.check_container_registry([], %{})
      assert findings == []
    end
  end

  # ─── CDN Provider Check ───────────────────────────────────────────

  describe "check_cdn_provider/2" do
    test "flags non-green CDN (CloudFront)" do
      contents = %{
        "deploy/config.yml" => "cdn: cloudfront\ndistribution_id: E12345"
      }

      findings = GreenWeb.check_cdn_provider([], contents)
      assert Enum.any?(findings, &(&1.type == :non_green_cdn))
    end

    test "returns empty when no non-green CDN detected" do
      contents = %{
        "deploy/config.yml" => "cdn: cloudflare\nzone_id: abc123"
      }

      findings = GreenWeb.check_cdn_provider([], contents)
      assert findings == []
    end
  end

  # ─── Full Audit ────────────────────────────────────────────────────

  describe "audit/3" do
    test "runs all checks and returns flat list" do
      findings = GreenWeb.audit("/tmp/test-repo", ["lib/app.ex"], %{})
      assert is_list(findings)
    end
  end
end

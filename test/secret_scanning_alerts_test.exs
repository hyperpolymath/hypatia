# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for Secret Scanning alert querying rules (SSA001-SSA004).
# Exercise the logic without hitting the GitHub API by relying on
# token-absent behaviour and direct helper calls.

defmodule Hypatia.Rules.SecretScanningAlertsTest do
  use ExUnit.Case, async: false

  alias Hypatia.Rules.SecretScanningAlerts

  @token_vars ["GITHUB_TOKEN", "HYPATIA_SECRET_ALERTS_TOKEN"]

  setup do
    saved = Map.new(@token_vars, &{&1, System.get_env(&1)})
    Enum.each(@token_vars, &System.delete_env/1)

    on_exit(fn ->
      Enum.each(saved, fn
        {name, nil} -> System.delete_env(name)
        {name, value} -> System.put_env(name, value)
      end)
    end)

    :ok
  end

  describe "ssa001_open_alerts/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      assert SecretScanningAlerts.ssa001_open_alerts("hyperpolymath", "test-nonexistent") == []
    end
  end

  describe "ssa002_severity_summary/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      assert SecretScanningAlerts.ssa002_severity_summary("hyperpolymath", "test-nonexistent") ==
               []
    end
  end

  describe "ssa003_stale_alerts/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      assert SecretScanningAlerts.ssa003_stale_alerts("hyperpolymath", "test-nonexistent") == []
    end
  end

  describe "ssa004_dismissed_without_fix/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      assert SecretScanningAlerts.ssa004_dismissed_without_fix(
               "hyperpolymath",
               "test-nonexistent"
             ) ==
               []
    end
  end

  describe "scan/2" do
    test "returns error tuple when GITHUB_TOKEN is not set" do
      assert {:error, msg} = SecretScanningAlerts.scan("hyperpolymath", "test-nonexistent")
      assert msg =~ "GITHUB_TOKEN not set"
    end
  end

  describe "resolve_token/0" do
    test "prefers the dedicated HYPATIA_SECRET_ALERTS_TOKEN" do
      System.put_env("GITHUB_TOKEN", "test-general")
      System.put_env("HYPATIA_SECRET_ALERTS_TOKEN", "test-dedicated")
      assert SecretScanningAlerts.resolve_token() == "test-dedicated"
    end

    test "falls back to GITHUB_TOKEN, treating an empty dedicated token as unset" do
      System.put_env("GITHUB_TOKEN", "test-general")
      System.put_env("HYPATIA_SECRET_ALERTS_TOKEN", "")
      assert SecretScanningAlerts.resolve_token() == "test-general"
    end

    test "is nil when neither is set" do
      assert SecretScanningAlerts.resolve_token() == nil
    end
  end

  describe "sweep/2" do
    test "is an error, not an empty result, when no token is set" do
      assert {:error, msg} = SecretScanningAlerts.sweep("hyperpolymath", "test-nonexistent")
      assert msg =~ "HYPATIA_SECRET_ALERTS_TOKEN"
    end
  end

  describe "parse_response/1" do
    test "a 200 with a JSON list is the alert list" do
      assert {:ok, [%{"number" => 1}]} =
               SecretScanningAlerts.parse_response(~s([{"number":1}]) <> "\n200")
    end

    test "an error status carries GitHub's message" do
      body = ~s({"message":"Secret scanning is disabled on this repository."})

      assert {:error, "GitHub API 404: Secret scanning is disabled on this repository."} =
               SecretScanningAlerts.parse_response(body <> "\n404")

      assert {:error, "GitHub API 403: Resource not accessible by integration"} =
               SecretScanningAlerts.parse_response(
                 ~s({"message":"Resource not accessible by integration"}) <> "\n403"
               )
    end

    test "a non-JSON error body still reports the status" do
      assert {:error, "GitHub API returned HTTP 502"} =
               SecretScanningAlerts.parse_response("<html>bad gateway</html>\n502")
    end

    test "a 200 that is not a list is an error, never an empty result" do
      assert {:error, "Invalid JSON response from GitHub API"} =
               SecretScanningAlerts.parse_response("not json\n200")
    end
  end

  describe "parse_page/1" do
    test "reads the alerts and the next page from curl -i output" do
      output =
        "HTTP/2 200\r\n" <>
          "content-type: application/json\r\n" <>
          ~s(link: <https://api.github.com/repositories/1/secret-scanning/alerts?page=2>; rel="next", ) <>
          ~s(<https://api.github.com/repositories/1/secret-scanning/alerts?page=3>; rel="last"\r\n) <>
          "\r\n" <> ~s([{"number":1}]) <> "\n200"

      assert {:ok, [%{"number" => 1}],
              "https://api.github.com/repositories/1/secret-scanning/alerts?page=2"} =
               SecretScanningAlerts.parse_page(output)
    end

    test "the last page has no next link" do
      output = "HTTP/2 200\r\ncontent-type: application/json\r\n\r\n[]\n200"
      assert {:ok, [], nil} = SecretScanningAlerts.parse_page(output)
    end

    test "skips an HTTPS proxy's own header block" do
      output =
        "HTTP/1.1 200 Connection established\r\n\r\n" <>
          "HTTP/2 200\r\ncontent-type: application/json\r\n\r\n" <>
          ~s([{"number":7}]) <> "\n200"

      assert {:ok, [%{"number" => 7}], nil} = SecretScanningAlerts.parse_page(output)
    end

    test "an error page is an error" do
      output =
        "HTTP/2 404\r\ncontent-type: application/json\r\n\r\n" <>
          ~s({"message":"Secret scanning is disabled on this repository."}) <> "\n404"

      assert {:error, "GitHub API 404: Secret scanning is disabled on this repository."} =
               SecretScanningAlerts.parse_page(output)
    end
  end

  describe "collect_pages/2" do
    @base "https://api.github.com/repos/acme/demo/secret-scanning/alerts?per_page=100"

    defp page(alerts, next) do
      link = if next, do: ~s(link: <#{next}>; rel="next"\r\n), else: ""
      "HTTP/2 200\r\n" <> link <> "\r\n" <> alerts <> "\n200"
    end

    test "follows next links and returns every page's alerts" do
      second = "https://api.github.com/repositories/1/secret-scanning/alerts?page=2"

      request = fn
        @base -> {:ok, page(~s([{"number":1},{"number":2}]), second)}
        ^second -> {:ok, page(~s([{"number":3}]), nil)}
      end

      assert {:ok, alerts} = SecretScanningAlerts.collect_pages(@base, request)
      assert Enum.map(alerts, & &1["number"]) == [1, 2, 3]
    end

    test "never sends the token to a next link off the API host" do
      request = fn @base -> {:ok, page("[]", "https://evil.example.com/steal")} end

      assert {:error, "refusing to follow a pagination link off https://api.github.com"} =
               SecretScanningAlerts.collect_pages(@base, request)
    end

    test "a failed later page fails the whole fetch" do
      second = "https://api.github.com/repositories/1/secret-scanning/alerts?page=2"

      request = fn
        @base -> {:ok, page(~s([{"number":1}]), second)}
        ^second -> {:error, "GitHub API request timed out after 30s"}
      end

      assert {:error, "GitHub API request timed out after 30s"} =
               SecretScanningAlerts.collect_pages(@base, request)
    end

    test "past the page cap it is an error, never a truncated list" do
      request = fn _ -> flunk("must not request past the cap") end

      assert {:error, "more than 5000 alerts" <> _} =
               SecretScanningAlerts.collect_pages(@base, request, [], 51)
    end

    test "stops after exactly 50 requests when every page has a next link" do
      {:ok, counter} = Agent.start_link(fn -> 0 end)
      next = "https://api.github.com/repositories/1/secret-scanning/alerts?page=n"

      request = fn _url ->
        Agent.update(counter, &(&1 + 1))
        {:ok, page("[]", next)}
      end

      assert {:error, "more than 5000 alerts" <> _} =
               SecretScanningAlerts.collect_pages(@base, request)

      assert Agent.get(counter, & &1) == 50
    end
  end

  describe "findings_from_alerts/3" do
    defp days_ago(n), do: DateTime.utc_now() |> DateTime.add(-n * 86_400) |> DateTime.to_iso8601()

    test "builds SSA001-SSA004 from fetched alerts" do
      alerts = [
        %{
          "number" => 1,
          "state" => "open",
          "secret_type" => "github_personal_access_token",
          "created_at" => days_ago(30)
        },
        %{
          "number" => 2,
          "state" => "open",
          "secret_type" => "aws_access_key_id",
          "created_at" => days_ago(1)
        },
        %{"number" => 3, "state" => "resolved", "resolution" => "wont_fix"},
        %{"number" => 4, "state" => "resolved", "resolution" => "revoked"}
      ]

      findings = SecretScanningAlerts.findings_from_alerts("acme", "demo", alerts)
      by_rule = Enum.group_by(findings, & &1.rule)

      assert length(by_rule["SSA001"]) == 2
      assert [%{file: "acme/demo", detail: %{total: 2}}] = by_rule["SSA002"]
      # Only the 30-day-old alert is past the 7-day threshold.
      assert [%{detail: %{alert_number: 1}}] = by_rule["SSA003"]
      # `wont_fix` is not an accepted resolution; `revoked` is.
      assert [%{detail: %{alert_number: 3}, severity: :high}] = by_rule["SSA004"]
    end

    test "no alerts means no findings" do
      assert SecretScanningAlerts.findings_from_alerts("acme", "demo", []) == []
    end
  end

  describe "scan_from_path/1" do
    test "returns error when remote is not a github URL" do
      tmp = Path.join(System.tmp_dir!(), "ssa-test-#{System.unique_integer([:positive])}")
      File.mkdir_p!(tmp)
      System.cmd("git", ["init", "-q"], cd: tmp)

      System.cmd("git", ["remote", "add", "origin", "http://gitea.example.com/foo/bar.git"],
        cd: tmp
      )

      assert {:error, msg} = SecretScanningAlerts.scan_from_path(tmp)
      assert msg =~ "Remote URL is not a GitHub URL"

      File.rm_rf!(tmp)
    end
  end
end

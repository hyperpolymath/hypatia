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
      assert [%{file: ".", detail: %{total: 2, repo: "acme/demo"}}] = by_rule["SSA002"]
      # Only the 30-day-old alert is past the 7-day threshold.
      assert [%{detail: %{alert_number: 1}}] = by_rule["SSA003"]
      # `wont_fix` is not an accepted resolution; `revoked` is.
      assert [%{detail: %{alert_number: 3}, severity: :high}] = by_rule["SSA004"]
    end

    test "no alerts means no findings" do
      assert SecretScanningAlerts.findings_from_alerts("acme", "demo", []) == []
    end
  end

  describe "alert locations" do
    @locations "https://api.github.com/repos/acme/demo/secret-scanning/alerts/1/locations"

    defp body(json), do: "HTTP/2 200\r\n\r\n" <> json <> "\n200"

    test "the first location with a path gives the file and line" do
      locations = [
        %{"type" => "issue_body", "details" => %{"issue_body_url" => "https://x"}},
        %{"type" => "commit", "details" => %{"path" => "config/prod.env", "start_line" => 12}},
        %{"type" => "commit", "details" => %{"path" => "later.env", "start_line" => 3}}
      ]

      assert SecretScanningAlerts.location_from(locations) ==
               %{"path" => "config/prod.env", "start_line" => 12}
    end

    test "no location with a path is an error, not a guessed file" do
      assert %{"error" => "no location names a file" <> _} =
               SecretScanningAlerts.location_from([
                 %{"type" => "pull_request_comment", "details" => %{}}
               ])

      assert %{"error" => _} = SecretScanningAlerts.location_from([])
    end

    test "reads one page of locations from the API host" do
      request = fn url ->
        assert url == @locations <> "?per_page=100"

        {:ok, body(~s([{"type":"commit","details":{"path":"a/b.ex","start_line":4}}]))}
      end

      assert SecretScanningAlerts.fetch_location(@locations, request) ==
               %{"path" => "a/b.ex", "start_line" => 4}
    end

    test "never sends the token to a locations URL off the API host" do
      request = fn _ -> flunk("must not request off the API host") end

      assert %{"error" => "refusing to follow a locations URL off https://api.github.com"} =
               SecretScanningAlerts.fetch_location("https://evil.example.com/x", request)
    end

    test "a failed lookup and a missing URL are recorded as errors" do
      request = fn _ -> {:error, "GitHub API request timed out after 30s"} end

      assert SecretScanningAlerts.fetch_location(@locations, request) ==
               %{"error" => "GitHub API request timed out after 30s"}

      assert %{"error" => "the alert has no locations_url"} =
               SecretScanningAlerts.fetch_location(nil, request)
    end

    test "only alerts that become file-scoped findings are looked up" do
      {:ok, asked} = Agent.start_link(fn -> [] end)

      request = fn url ->
        Agent.update(asked, &[url | &1])
        {:ok, body(~s([{"details":{"path":"f.txt","start_line":1}}]))}
      end

      alerts =
        for {n, state, resolution} <- [
              {1, "open", nil},
              {2, "resolved", "wont_fix"},
              {3, "resolved", "revoked"}
            ] do
          %{
            "number" => n,
            "state" => state,
            "resolution" => resolution,
            "locations_url" => "https://api.github.com/repos/acme/demo/l/#{n}"
          }
        end

      located = SecretScanningAlerts.attach_locations(alerts, request)

      assert Agent.get(asked, &Enum.sort/1) == [
               "https://api.github.com/repos/acme/demo/l/1?per_page=100",
               "https://api.github.com/repos/acme/demo/l/2?per_page=100"
             ]

      assert [%{"hypatia_location" => _}, %{"hypatia_location" => _}, third] = located
      refute Map.has_key?(third, "hypatia_location")
    end

    test "findings carry the location, or the repository level with the reason" do
      created = DateTime.utc_now() |> DateTime.add(-30 * 86_400) |> DateTime.to_iso8601()

      alerts = [
        %{
          "number" => 1,
          "state" => "open",
          "secret_type" => "github_personal_access_token",
          "created_at" => created,
          "hypatia_location" => %{"path" => "config/prod.env", "start_line" => 12}
        },
        %{
          "number" => 2,
          "state" => "open",
          "secret_type" => "aws_access_key_id",
          "created_at" => created,
          "hypatia_location" => %{"error" => "no location names a file in the repository"}
        },
        %{
          "number" => 3,
          "state" => "resolved",
          "resolution" => "wont_fix",
          "secret_type" => "slack_api_token"
        }
      ]

      by_key =
        "acme"
        |> SecretScanningAlerts.findings_from_alerts("demo", alerts)
        |> Map.new(fn f -> {{f.rule, f.detail[:alert_number]}, f} end)

      for rule <- ["SSA001", "SSA003"] do
        located = by_key[{rule, 1}]
        assert located.file == "config/prod.env"
        assert located.line == 12
        assert located.detail.location.status == "found"
        assert located.detail.secret_type == "github_personal_access_token"
      end

      unavailable = by_key[{"SSA001", 2}]
      assert unavailable.file == "."
      refute Map.has_key?(unavailable, :line)

      assert unavailable.detail.location ==
               %{status: "unavailable", reason: "no location names a file in the repository"}

      not_fetched = by_key[{"SSA004", 3}]
      assert not_fetched.file == "."
      assert not_fetched.detail.location == %{status: "not_fetched"}
    end

    test "the verdict span points at the located file and line" do
      [finding] =
        SecretScanningAlerts.open_alert_findings([
          %{
            "number" => 1,
            "state" => "open",
            "secret_type" => "github_personal_access_token",
            "hypatia_location" => %{"path" => "config/prod.env", "start_line" => 12}
          }
        ])

      token =
        finding
        |> Map.put(:rule_module, "secret_scanning_alerts")
        |> Map.put(:type, finding.rule)
        |> Hypatia.Verdict.token()

      assert token.span == %{file: "config/prod.env", start_line: 12, end_line: 12}
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

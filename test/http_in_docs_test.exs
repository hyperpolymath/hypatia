# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.HttpInDocsTest do
  use ExUnit.Case, async: true

  alias Hypatia.CLI

  # Every negative case below is a line hypatia reported on echidna
  # (echidna#314): none names a public host that could be moved to https.

  setup do
    dir = Path.join(System.tmp_dir!(), "hyp-http-test-#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  defp findings(dir, rel, line) do
    path = Path.join(dir, rel)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, line <> "\n")

    dir
    |> CLI.collect_findings([:content_patterns])
    |> Enum.filter(&(&1.type == "http_in_docs"))
  end

  test "a public http link is reported", %{dir: dir} do
    assert [_] = findings(dir, "docs/x.adoc", "See http://mizar.org/system/index.html[Mizar].")
  end

  test "a public http link after a reserved one on the same line is still reported", %{dir: dir} do
    assert [_] = findings(dir, "README.md", "http://julia-ml:9000 then http://pvs.csl.sri.com/")
  end

  for line <- [
        ~s|curl -m 5 http://<SERVER_IP>:8081/api/health|,
        "export URL=http://julia-ml:9000",
        "VERISIM=http://verisim.staging.example:9090",
        "http://api.example.org/v1 and http://example.com",
        "http://svc.internal/health and http://printer.local/",
        "|http://+ |fragment|",
        "http://localhost:4000"
      ] do
    test "non-public host is not reported: #{line}", %{dir: dir} do
      assert [] = findings(dir, "docs/x.adoc", unquote(line))
    end
  end

  test "verbatim licence text under LICENSES/ is not reported", %{dir: dir} do
    assert [] =
             findings(dir, "LICENSES/MPL-2.0.txt", "one at http://mozilla.org/MPL/2.0/.")
  end
end

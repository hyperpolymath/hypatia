# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.ProofSourceSecretTest do
  use ExUnit.Case, async: true

  alias Hypatia.CLI

  setup do
    dir = Path.join(System.tmp_dir!(), "hyp-proof-secret-#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  defp secret_findings(dir, name, body) do
    File.write!(Path.join(dir, name), body)

    dir
    |> CLI.collect_findings([:code_safety])
    |> Enum.filter(&(&1.type == "secret_detected"))
  end

  test "a named proof fact is not reported as a secret", %{dir: dir} do
    assert [] = secret_findings(dir, "OND.thy", ~s|lemma secret: "x = y"\n|)
  end

  # CodeRabbit on #883: the suppression must not hide a real credential
  # just because it sits in a proof-language file.
  test "an assigned credential in a proof source is still reported", %{dir: dir} do
    assert [_ | _] = secret_findings(dir, "A.lean", ~s|password = "hunter2"\n|)
  end
end

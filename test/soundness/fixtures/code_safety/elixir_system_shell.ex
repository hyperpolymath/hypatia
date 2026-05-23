# SPDX-License-Identifier: MPL-2.0
# SOUNDNESS FIXTURE — known-bad sample for code_safety/elixir_system_shell.
# This is THE pattern #278's stale-escript audit found being silently
# dropped. DO NOT FIX.

defmodule Soundness.ElixirSystemShell do
  def bad(user) do
    System.shell("echo #{user}")
  end
end

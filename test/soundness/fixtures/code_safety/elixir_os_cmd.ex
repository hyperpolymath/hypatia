# SPDX-License-Identifier: MPL-2.0
# SOUNDNESS FIXTURE — known-bad sample for code_safety/elixir_os_cmd.
# DO NOT FIX.

defmodule Soundness.ElixirOsCmd do
  def bad(user) do
    :os.cmd(~c"echo #{user}")
  end
end

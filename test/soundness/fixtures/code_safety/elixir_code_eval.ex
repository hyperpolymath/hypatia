# SPDX-License-Identifier: MPL-2.0
# SOUNDNESS FIXTURE — known-bad sample for code_safety/elixir_code_eval.
# DO NOT FIX.

defmodule Soundness.ElixirCodeEval do
  def bad(input) do
    Code.eval_string(input)
  end
end

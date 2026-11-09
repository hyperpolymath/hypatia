# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.KyamlSpecMutationTest do
  use ExUnit.Case, async: true
  alias Hypatia.Kyaml

  # A normative oracle, independent of kyq: YAML 1.2 §5.1 c-printable.
  # Exhaustive over excluded C0/C1 classes, not random reference agreement.
  @excluded Enum.to_list(0..8) ++
              [11, 12] ++
              Enum.to_list(14..31) ++
              [127] ++
              Enum.to_list(128..132) ++ Enum.to_list(134..159)
  test "every excluded printable-set member is rejected in each lexical context" do
    for point <- @excluded,
        template <- [
          "a: before~after\n",
          "a: 'before~after'\n",
          "a: |\n  before~after\n",
          "a: >\n  before~after\n",
          "# before~after\na: 1\n",
          "a~b: 1\n",
          "a: &before~after 1\n"
        ] do
      input = String.replace(template, "~", <<point::utf8>>)

      assert {:error, %{code: "CONTROL_CHARACTER"}} = Kyaml.parse(input),
             "accepted U+#{Integer.to_string(point, 16)} in #{inspect(template)}"
    end
  end

  test "printable boundary characters and escaped controls remain valid" do
    for point <- [32, 126, 160, 255, 0xD7FF, 0xE000, 0xFFFD, 0x10000, 0x10FFFF] do
      # I-JSON additionally excludes noncharacters, including U+10FFFF.
      if point != 0x10FFFF,
        do: assert({:ok, [_]} = Kyaml.parse("a: 'before#{<<point::utf8>>}after'\n"))
    end

    assert {:ok, [_]} = Kyaml.parse("a: \"escaped\\u0001\"\n")
    assert {:ok, [_]} = Kyaml.parse("a: before\tafter\n")
  end
end

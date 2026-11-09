# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.StructuredFormatTest do
  use ExUnit.Case, async: true
  alias Hypatia.Rules.StructuredFormat

  test "workflow structure regexes have located advisory findings" do
    source = ~S"""
    defmodule Reader do
      def jobs(line), do: Regex.match?(~r/^jobs:\s*$/, line)
    end
    """

    assert [%{type: "SF001", line: 2, advisory: true, severity: "medium"}] =
             StructuredFormat.scan_content(source)
  end

  test "comments, documentation strings and canonical parser calls are not regex parsing" do
    assert [] =
             StructuredFormat.scan_content(~S"""
             defmodule Reader do
               @moduledoc "Example: ~r/^jobs:/"
               # Regex.match?(~r/^steps:/, line)
               def jobs(bytes), do: Hypatia.Kyaml.parse(bytes)
               def words(text), do: Regex.scan(~r/hello/, text)
             end
             """)
  end
end

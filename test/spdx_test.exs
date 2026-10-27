# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.SPDXTest do
  use ExUnit.Case, async: true

  alias Hypatia.SPDX

  test "identifiers on the SPDX License List are licence ids" do
    for id <- ["MPL-2.0", "MIT", "Apache-2.0", "CC-BY-SA-4.0", "0BSD"] do
      assert SPDX.licence_id?(id), id
    end
  end

  test "lowercase identifiers on the list count, so no case heuristic is needed" do
    assert SPDX.licence_id?("curl")
    assert SPDX.licence_id?("blessing")
  end

  test "deprecated identifiers still name a licence" do
    assert SPDX.licence_id?("GPL-3.0")
  end

  test "REUSE custom licences are LicenseRef- ids" do
    assert SPDX.licence_id?("LicenseRef-Estate-1.0")
    refute SPDX.licence_id?("LicenseRef-")
    refute SPDX.licence_id?("LicenseRef-has space")
  end

  test "other names are not, and matching is exact" do
    for id <- ["notes", "README", "LICENSE", "mpl-2.0", "MPL-2", ""] do
      refute SPDX.licence_id?(id), id
    end
  end

  test "the list version is recorded" do
    assert SPDX.list_version() =~ ~r/\A# SPDX License List \d+\.\d+/
  end
end

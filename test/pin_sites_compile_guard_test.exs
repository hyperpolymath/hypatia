# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.PinSitesCompileGuardTest do
  @moduledoc """
  OTP 28 (seen on 28.3 and 28.5.0.8) miscompiles `PinSites.walk/4` and the VM
  segfaults in garbage collection: estate scans and the RSR oracle exited 139.
  The module turns the faulty pass off. A segfault cannot be asserted from
  inside the VM it kills, so this guards the switch itself.
  """
  use ExUnit.Case, async: true

  test "PinSites is compiled without the destructive-update pass" do
    options = Hypatia.Rules.PinSites.module_info(:compile)[:options]
    assert :no_ssa_opt_destructive_update in options
  end
end

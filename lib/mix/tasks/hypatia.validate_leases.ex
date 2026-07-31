# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Mix.Tasks.Hypatia.ValidateLeases do
  @shortdoc "Validate the Kin.Gate lease store (LE1/LE2/overlap/stale) — the k9 lease validator"
  @moduledoc """
  Audits the persisted Kin.Gate lease store for the a5 schema invariants (LE1:
  held ⇒ TTL; LE2: meta ⇒ owner-authorized) plus the two coordination hazards the
  pool/lease spec names (overlapping and stale leases). Exits non-zero on any
  violation, so it runs as a CI gate / the k9 lease validator the a5 standards
  adoption points at.

      mix hypatia.validate_leases [--store PATH]

  Store defaults to `$MERGE_ORCH_STORE` then `data/verisim`; the leases live under
  `<store>/leases`.
  """
  use Mix.Task

  alias Hypatia.MergeOrchestration.LeaseValidator

  @impl Mix.Task
  def run(argv) do
    {opts, _, _} = OptionParser.parse(argv, strict: [store: :string])
    store = opts[:store] || System.get_env("MERGE_ORCH_STORE") || "data/verisim"
    dir = Path.join(store, "leases")

    case LeaseValidator.validate_store(dir) do
      [] ->
        Mix.shell().info("[lease-validator] clean: no LE1/LE2/overlap/stale violations in #{dir}")

      violations ->
        Mix.shell().error("[lease-validator] #{length(violations)} violation(s) in #{dir}:")
        Enum.each(violations, fn v -> Mix.shell().error("  - #{inspect(v)}") end)
        exit({:shutdown, 1})
    end
  end
end

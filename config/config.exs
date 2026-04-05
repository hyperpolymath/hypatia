# SPDX-License-Identifier: PMPL-1.0-or-later
import Config

# verisim-data: the canonical git-backed flat-file store (300 scans, 23 recipes)
# Lives inside the nextgen-databases monorepo
config :hypatia,
  verisimdb_data_path: Path.expand("~/Documents/hyperpolymath-repos/nextgen-databases/verisim/verisim-data"),
  fleet_path: Path.expand("~/Documents/hyperpolymath-repos/gitbot-fleet"),
  http_port: 9090

# Import environment specific config (config/test.exs, config/dev.exs, etc.)
import_config "#{config_env()}.exs"

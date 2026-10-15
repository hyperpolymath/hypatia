# SPDX-License-Identifier: MPL-2.0
import Config

# verisim-data: the canonical git-backed flat-file store (300 scans, 23 recipes).
# No machine-specific defaults (same migration as #918): the in-repo store ships
# as `data/verisim` and deployments point elsewhere via HYPATIA_VERISIM_DATA_PATH
# / HYPATIA_FLEET_PATH. The paths are expanded relative to the hypatia repo
# root, so run the app from there (the documented `just`/`mix` workflow does).
config :hypatia,
  verisimdb_data_path: System.get_env("HYPATIA_VERISIM_DATA_PATH", "data/verisim"),
  fleet_path: System.get_env("HYPATIA_FLEET_PATH", "../gitbot-fleet"),
  http_port: 9090

# Import environment specific config (config/test.exs, config/dev.exs, etc.)
import_config "#{config_env()}.exs"

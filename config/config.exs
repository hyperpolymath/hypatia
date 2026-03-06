# SPDX-License-Identifier: PMPL-1.0-or-later
import Config

# Hypatia's own local verisimdb data store
# This is NOT the central verisimdb-data repo — hypatia maintains its own instance
config :hypatia,
  verisimdb_data_path: Path.expand("data/verisimdb", __DIR__ |> Path.dirname()),
  fleet_path: Path.expand("~/Documents/hyperpolymath-repos/gitbot-fleet")

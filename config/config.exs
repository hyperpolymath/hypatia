# SPDX-License-Identifier: PMPL-1.0-or-later
import Config

# verisimdb-data: the canonical git-backed flat-file store (300 scans, 23 recipes)
# Lives inside the nextgen-databases monorepo
config :hypatia,
  verisimdb_data_path: Path.expand("~/Documents/hyperpolymath-repos/nextgen-databases/verisimdb/verisimdb-data"),
  fleet_path: Path.expand("~/Documents/hyperpolymath-repos/gitbot-fleet")

# SPDX-License-Identifier: PMPL-1.0-or-later
import Config

# Use a different port for tests to avoid collisions with the running
# dev/production server on port 9090.
config :hypatia,
  http_port: 9099

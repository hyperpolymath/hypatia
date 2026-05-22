# SPDX-License-Identifier: MPL-2.0
import Config

# Use a different port for tests to avoid collisions with the running
# dev/production server on port 9090.
config :hypatia,
  http_port: 9099

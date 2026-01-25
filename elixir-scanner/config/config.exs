# SPDX-License-Identifier: PMPL-1.0-or-later
import Config

# Configure logger to write to stderr only
config :logger, :console,
  device: :standard_error,
  format: "$time $metadata[$level] $message\n"

# In production, reduce log level
if config_env() == :prod do
  config :logger, level: :warning
end

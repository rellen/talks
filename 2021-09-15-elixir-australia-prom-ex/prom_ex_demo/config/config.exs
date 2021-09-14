# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :prom_ex_demo,
  ecto_repos: [PromExDemo.Repo]

# Configures the endpoint
config :prom_ex_demo, PromExDemoWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "tIUdks78vTrnQ3swRFw4zxUIC+1wBK39eIkoymOMMTpL2wEeEzdI7VkOMNf/+1qh",
  render_errors: [view: PromExDemoWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: PromExDemo.PubSub,
  live_view: [signing_salt: "Zpf8EPxB"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :prom_ex_demo, PromExDemo.PromEx,
  disabled: false,
  manual_metrics_start_delay: :no_delay,
  drop_metrics_groups: [],
  metrics_server: [
    port: "9568",
    path: "/metrics"
  ],
  grafana: [
    host: System.get_env("GRAFANA_CLOUD_URL"),
    auth_token: System.get_env("GRAFANA_CLOUD_AUTH_TOKEN"),
    # This is an optional setting and will default to `true`
    folder_name: "prom_ex_demo",
    upload_dashboards_on_start: true
  ]

config :prom_ex_demo,
  grafana_datasource_id: System.get_env("GRAFANA_DATASOURCE_ID")

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"

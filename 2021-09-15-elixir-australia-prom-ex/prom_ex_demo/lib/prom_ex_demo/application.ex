defmodule PromExDemo.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      PromExDemo.PromEx,
      # Start the Ecto repository
      PromExDemo.Repo,
      # Start the Telemetry supervisor
      PromExDemoWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: PromExDemo.PubSub},
      # Start the Endpoint (http/https)
      PromExDemoWeb.Endpoint
      # Start a worker by calling: PromExDemo.Worker.start_link(arg)
      # {PromExDemo.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: PromExDemo.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    PromExDemoWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end

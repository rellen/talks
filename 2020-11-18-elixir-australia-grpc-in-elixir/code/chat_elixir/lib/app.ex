defmodule ChatElixir.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: ChatElixir.RoomRegistry},
      {ChatElixir.RoomSupervisor, []},
      {GRPC.Server.Supervisor, {ChatElixir.Endpoint, 50051}}
    ]

    opts = [strategy: :one_for_one, name: ChatElixir]
    Supervisor.start_link(children, opts)
  end
end

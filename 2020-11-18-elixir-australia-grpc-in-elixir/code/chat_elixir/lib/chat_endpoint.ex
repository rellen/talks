defmodule ChatElixir.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Logger.Server)
  run(ChatElixir.ChatServer)
end

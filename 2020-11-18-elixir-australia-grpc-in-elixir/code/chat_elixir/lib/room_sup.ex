defmodule ChatElixir.RoomSupervisor do
  use DynamicSupervisor

  alias ChatElixir.Room

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def start_child(name, description) do
    spec = {Room, {name, description}} |> IO.inspect()

    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(
      strategy: :one_for_one,
      extra_arguments: [init_arg]
    )
  end
end

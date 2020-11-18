defmodule ChatElixir.Room do
  use GenServer

  alias ChatElixir.RoomRegistry, as: Reg

  def start_link(_, {name, description} = args) do
    via_name = {:via, Registry, {Reg, name, description}}
    GenServer.start_link(__MODULE__, args, name: via_name)
  end

  def get_state(name) do
    case Registry.lookup(Reg, name) do
      [pid, _] ->
        GenServer.call(pid, :get_state)

      _ ->
        {:reply, {:error, :could_not_find_room}}
    end
  end

  def join_room(name, user_handle, stream) do
    GenServer.call({:via, Registry, {Reg, name}}, {:join_room, user_handle, stream})
  end

  def send_message(name, user_handle, message) do
    GenServer.cast({:via, Registry, {Reg, name}}, {:send_message, user_handle, message})
  end

  @impl true
  def init({name, description}) do
    {:ok, %{name: name, description: description, members: %{}, messages: []}}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:join_room, user_handle, server}, _from, state) do
    newstate = update_in(state, [:members], &Map.put(&1, user_handle, server))
    {:reply, :ok, newstate}
  end

  @impl true
  def handle_cast({:send_message, user_handle, message}, state) do
    data =
      Chat.RoomData.new(
        room_name: state.name,
        payload:
          {:update,
           Chat.Update.new(
             posts: [
               Chat.Post.new(
                 timestamp_utc: 0,
                 post: {:user_post, Chat.UserPost.new(user_handle: user_handle, content: message)}
               )
             ]
           )}
      )

    state.members
    |> Enum.each(fn {_user_handle, server} ->
      Process.send(server, {:message, data}, [])
    end)

    newstate =
      update_in(state, [:messages], fn messages -> [{0, user_handle, message} | messages] end)

    {:noreply, newstate}
  end
end

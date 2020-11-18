defmodule ChatElixir.Client do
  def get_channel() do
    GRPC.Stub.connect("localhost:50051")
  end

  def create_room(channel, name, description) do
    request = Chat.CreateRoomRequest.new(name: name, description: description)
    {:ok, _response} = channel |> Chat.Chatter.Stub.create_room(request)
  end

  def join_room(channel, room, user) do
    spawn_link(fn -> receive(channel, room, user, self()) end)
  end

  defp receive(channel, room, user, _parent) do
    request = Chat.JoinRoomRequest.new(room_name: room, user_handle: user)

    {:ok, stream} = channel |> Chat.Chatter.Stub.join_room(request)

    Enum.each(stream, fn msg -> IO.inspect(msg) end)
  end

  def send_message(channel, room, user, message) do
    request = Chat.SendMessageRequest.new(room_name: room, user_handle: user, content: message)
    channel |> Chat.Chatter.Stub.send_message(request)
  end
end

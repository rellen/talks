defmodule ChatElixir.ChatServer do
  use GRPC.Server, service: Chat.Chatter.Service

  alias ChatElixir.RoomRegistry

  @spec create_room(Chat.CreateRoomRequest.t(), GRPC.Server.Stream.t()) ::
          Chat.CreateRoomResponse.t()
  def create_room(request, _stream) do
    response =
      case ChatElixir.RoomSupervisor.start_child(request.name, request.description) do
        {:ok, _pid} ->
          {:success, Chat.CreateRoomSuccess.new()}

        {:error, {:already_started, _pid}} ->
          {:error,
           Chat.Error.new(
             code: Chat.ErrorCode.value(:ROOM_NAME_TAKEN),
             reason: "#{request.name} already created"
           )}
      end

    Chat.CreateRoomResponse.new(response: response)
  end

  @spec list_rooms(Chat.ListRoomsRequest.t(), GRPC.Server.Stream.t()) ::
          Chat.ListRoomsResponse.t()
  def list_rooms(_request, _stream) do
    rooms =
      Registry.select(RoomRegistry, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}])

    roomsData =
      rooms
      |> Enum.map(fn {name, _pid, description} ->
        Chat.Room.new(name: name, description: description)
      end)

    Chat.ListRoomsResponse.new(rooms: roomsData)
  end

  @spec join_room(Chat.JoinRoomRequest.t(), GRPC.Server.Stream.t()) :: any
  def join_room(request, stream) do
    ChatElixir.Room.join_room(request.room_name, request.user_handle, self())
    loop(stream)
  end

  @spec send_message(Chat.SendMessageRequest.t(), GRPC.Server.Stream.t()) ::
          Chat.SendMessageResponse.t()
  def send_message(request, _stream) do
    ChatElixir.Room.send_message(request.room_name, request.user_handle, request.content)
    Chat.SendMessageResponse.new()
  end

  defp loop(stream) do
    receive do
      {:message, msg} ->
        GRPC.Server.send_reply(stream, msg)
        loop(stream)

      _ ->
        loop(stream)
    end
  end
end

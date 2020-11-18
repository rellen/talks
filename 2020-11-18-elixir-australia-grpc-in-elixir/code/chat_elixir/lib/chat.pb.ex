defmodule Chat.ErrorCode do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3
  @type t :: integer | :INVALID_ROOM | :ROOM_NAME_TAKEN | :USER_HANDLE_TAKEN

  field :INVALID_ROOM, 0

  field :ROOM_NAME_TAKEN, 1

  field :USER_HANDLE_TAKEN, 2
end

defmodule Chat.ListRoomsRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3
  @type t :: %__MODULE__{}

  defstruct []
end

defmodule Chat.ListRoomsResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          rooms: [Chat.Room.t()]
        }

  defstruct [:rooms]

  field :rooms, 1, repeated: true, type: Chat.Room
end

defmodule Chat.Room do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          name: String.t(),
          description: String.t()
        }

  defstruct [:name, :description]

  field :name, 1, type: :string
  field :description, 2, type: :string
end

defmodule Chat.CreateRoomRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          name: String.t(),
          description: String.t()
        }

  defstruct [:name, :description]

  field :name, 1, type: :string
  field :description, 2, type: :string
end

defmodule Chat.CreateRoomResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          response: {atom, any}
        }

  defstruct [:response]

  oneof :response, 0
  field :success, 1, type: Chat.CreateRoomSuccess, oneof: 0
  field :error, 2, type: Chat.Error, oneof: 0
end

defmodule Chat.CreateRoomSuccess do
  @moduledoc false
  use Protobuf, syntax: :proto3
  @type t :: %__MODULE__{}

  defstruct []
end

defmodule Chat.JoinRoomRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          room_name: String.t(),
          user_handle: String.t()
        }

  defstruct [:room_name, :user_handle]

  field :room_name, 1, type: :string
  field :user_handle, 2, type: :string
end

defmodule Chat.RoomData do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          payload: {atom, any},
          room_name: String.t()
        }

  defstruct [:payload, :room_name]

  oneof :payload, 0
  field :room_name, 1, type: :string
  field :error, 2, type: Chat.Error, oneof: 0
  field :update, 3, type: Chat.Update, oneof: 0
end

defmodule Chat.Error do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          code: Chat.ErrorCode.t(),
          reason: String.t()
        }

  defstruct [:code, :reason]

  field :code, 1, type: Chat.ErrorCode, enum: true
  field :reason, 2, type: :string
end

defmodule Chat.Update do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          posts: [Chat.Post.t()],
          members: [Chat.Member.t()]
        }

  defstruct [:posts, :members]

  field :posts, 1, repeated: true, type: Chat.Post
  field :members, 2, repeated: true, type: Chat.Member
end

defmodule Chat.Post do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          post: {atom, any},
          timestamp_utc: non_neg_integer
        }

  defstruct [:post, :timestamp_utc]

  oneof :post, 0
  field :timestamp_utc, 1, type: :uint64
  field :user_post, 2, type: Chat.UserPost, oneof: 0
  field :room_post, 3, type: Chat.RoomPost, oneof: 0
end

defmodule Chat.UserPost do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          user_handle: String.t(),
          content: String.t()
        }

  defstruct [:user_handle, :content]

  field :user_handle, 1, type: :string
  field :content, 2, type: :string
end

defmodule Chat.RoomPost do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          content: String.t()
        }

  defstruct [:content]

  field :content, 1, type: :string
end

defmodule Chat.Member do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          handle: String.t()
        }

  defstruct [:handle]

  field :handle, 1, type: :string
end

defmodule Chat.LeaveRoomRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          room_name: String.t(),
          user_handle: String.t()
        }

  defstruct [:room_name, :user_handle]

  field :room_name, 1, type: :string
  field :user_handle, 2, type: :string
end

defmodule Chat.LeaveRoomResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3
  @type t :: %__MODULE__{}

  defstruct []
end

defmodule Chat.SendMessageRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          room_name: String.t(),
          user_handle: String.t(),
          content: String.t()
        }

  defstruct [:room_name, :user_handle, :content]

  field :room_name, 1, type: :string
  field :user_handle, 2, type: :string
  field :content, 3, type: :string
end

defmodule Chat.SendMessageResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3
  @type t :: %__MODULE__{}

  defstruct []
end

defmodule Chat.Chatter.Service do
  @moduledoc false
  use GRPC.Service, name: "chat.Chatter"

  rpc :listRooms, Chat.ListRoomsRequest, Chat.ListRoomsResponse

  rpc :createRoom, Chat.CreateRoomRequest, Chat.CreateRoomResponse

  rpc :joinRoom, Chat.JoinRoomRequest, stream(Chat.RoomData)

  rpc :leaveRoom, Chat.LeaveRoomRequest, Chat.LeaveRoomResponse

  rpc :sendMessage, Chat.SendMessageRequest, Chat.SendMessageResponse
end

defmodule Chat.Chatter.Stub do
  @moduledoc false
  use GRPC.Stub, service: Chat.Chatter.Service
end

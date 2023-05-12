defmodule GradientExample.StructExample do
  defstruct id: 1, name: "Alice"

  @type t() :: %__MODULE__{}

  @spec init() :: t()
  def init() do
    %__MODULE__{id: 2, name: "Bob"}
  end

  # no error found
  @spec bad_id() :: t()
  def bad_id() do
    %__MODULE__{id: :bob, name: "Bob"}
  end

  # error found
  @spec bad_type() :: t()
  def bad_type() do
    %{id: 2, name: "Bob"}
  end
end

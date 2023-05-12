defmodule TaskEither do
  import Algae

  defsum do
    defdata(Left :: any())
    defdata(Right :: any())
  end
end

alias TaskEither.{Left, Right}
import TypeClass
use Witchcraft

#############
# Generator #
#############

defimpl TypeClass.Property.Generator, for: TaskEither.Left do
  def generate(_) do
    [1, 1.1, "", []]
    |> Enum.random()
    |> TypeClass.Property.Generator.generate()
    |> TaskEither.Left.new()
  end
end

defimpl TypeClass.Property.Generator, for: TaskEither.Right do
  def generate(_) do
    [1, 1.1, "", []]
    |> Enum.random()
    |> TypeClass.Property.Generator.generate()
    |> TaskEither.Right.new()
  end
end

definst Witchcraft.Functor, for: TaskEither.Left do
  def map(left, _), do: left
end

definst Witchcraft.Functor, for: TaskEither.Right do
  def map(%Right{right: data}, fun), do: data |> fun.() |> Right.new()
end

#########
# Apply #
#########

definst Witchcraft.Apply, for: TaskEither.Left do
  def convey(left, _), do: left
end

definst Witchcraft.Apply, for: TaskEither.Right do
  def convey(_, left = %Left{}), do: left
  def convey(data, %Right{right: fun}), do: map(data, fun)
end

###############
# Applicative #
###############

definst Witchcraft.Applicative, for: TaskEither.Left do
  @force_type_instance true

  def of(_, data), do: Right.new(data)
end

definst Witchcraft.Applicative, for: TaskEither.Right do
  def of(_, data), do: Right.new(data)
end

# #########
# # Chain #
# #########

definst Witchcraft.Chain, for: TaskEither.Left do
  def chain(left, _), do: left
end

definst Witchcraft.Chain, for: TaskEither.Right do
  def chain(%Right{right: data}, link), do: link.(data)
end

#########
# Monad #
#########

definst Witchcraft.Monad, for: TaskEither.Left do
  @force_type_instance true
end

definst Witchcraft.Monad, for: TaskEither.Right do
  @force_type_instance true
end

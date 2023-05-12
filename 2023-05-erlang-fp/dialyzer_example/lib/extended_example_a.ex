defmodule DialyzerExample.ExtendedExample do
  @spec extra_return(integer()) :: :even | :odd | :zero
  def extra_return(a) do
    if rem(a, 2) == 0, do: :even, else: :odd
  end

  @spec missing_return(integer()) :: :even | :odd
  def missing_return(a) do
    cond do
      a == 0 -> :zero
      rem(a, 2) == 0 -> :even
      rem(a, 2) != 0 -> :odd
    end
  end
end

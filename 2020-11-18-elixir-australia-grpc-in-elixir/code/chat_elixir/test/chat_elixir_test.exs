defmodule ChatElixirTest do
  use ExUnit.Case
  doctest ChatElixir

  test "greets the world" do
    assert ChatElixir.hello() == :world
  end
end

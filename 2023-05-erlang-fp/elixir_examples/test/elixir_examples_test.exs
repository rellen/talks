defmodule ElixirExamplesTest do
  use ExUnit.Case
  doctest ElixirExamples

  test "greets the world" do
    assert ElixirExamples.hello() == :world
  end
end

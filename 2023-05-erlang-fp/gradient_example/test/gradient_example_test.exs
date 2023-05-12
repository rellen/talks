defmodule GradientExampleTest do
  use ExUnit.Case
  doctest GradientExample

  test "greets the world" do
    assert GradientExample.hello() == :world
  end
end

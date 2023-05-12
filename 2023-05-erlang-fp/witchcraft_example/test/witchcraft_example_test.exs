defmodule WitchcraftExampleTest do
  use ExUnit.Case
  doctest WitchcraftExample

  test "greets the world" do
    assert WitchcraftExample.hello() == :world
  end
end

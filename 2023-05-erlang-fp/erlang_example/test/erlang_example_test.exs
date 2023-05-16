defmodule ErlangExampleTest do
  use ExUnit.Case
  doctest ErlangExample

  test "greets the world" do
    assert ErlangExample.hello() == :world
  end
end

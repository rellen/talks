# a comment
defmodule Foo do
  def bar(x) do
    string = "hello"

    fc = fn x, y -> x + y end
    atom = fc.(:amatom)
  end
end

defmodule DialyzerExample.MatchExampleB do
  @type tag :: :foo | :bar
  @type tagged_type :: {tag(), term()}

  @spec handle(tagged_type()) :: term()
  def handle(tagged_data)

  def handle({:foo, data}),
    do: IO.inspect(data, label: "got foo")

  # Let's actually call handle/1
  def run do
    handle({:foo, "foo"})
    handle({:bar, "bar"})
  end
end

defmodule DialyzerExample.MatchExampleA do
  @type tag :: :foo | :bar
  @type tagged_type :: {tag(), term()}

  @spec handle(tagged_type()) :: term()
  def handle(tagged_data)

  def handle({:foo, data}) do
    IO.inspect(data, label: "got foo")
  end

  # where do we handle :bar??
end

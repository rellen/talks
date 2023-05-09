defmodule DialyzerExample.CaseExample do
  @type tag :: :foo | :bar
  @type tagged_type :: {tag(), term()}

  @spec handle(tagged_type()) :: term()
  def handle(tagged_data)

  def handle({tag, data}) do
    case tag do
      :foo -> IO.inspect(data, label: "got foo")
    end
  end

  def run do
    handle({:baz, "baz"})
  end
end

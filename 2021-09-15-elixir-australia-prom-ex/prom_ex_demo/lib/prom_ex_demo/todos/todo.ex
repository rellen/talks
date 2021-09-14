defmodule PromExDemo.Todos.Todo do
  use Ecto.Schema
  import Ecto.Changeset

  schema "todo" do
    field :done, :boolean, default: false
    field :title, :string

    timestamps()
  end

  @doc false
  def changeset(todo, attrs) do
    todo
    |> cast(attrs, [:title, :done])
    |> validate_required([:title, :done])
  end
end

defmodule PromExDemo.Todo.Todu do
  use Ecto.Schema
  import Ecto.Changeset

  schema "todu" do
    field :done, :boolean, default: false
    field :title, :string

    timestamps()
  end

  @doc false
  def changeset(todu, attrs) do
    todu
    |> cast(attrs, [:title, :done])
    |> validate_required([:title, :done])
  end
end

defmodule PromExDemo.Repo.Migrations.CreateTodo do
  use Ecto.Migration

  def change do
    create table(:todo) do
      add :title, :string
      add :done, :boolean, default: false, null: false

      timestamps()
    end

  end
end

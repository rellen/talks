defmodule PromExDemo.Repo.Migrations.CreateTodu do
  use Ecto.Migration

  def change do
    create table(:todu) do
      add :title, :string
      add :done, :boolean, default: false, null: false

      timestamps()
    end

  end
end

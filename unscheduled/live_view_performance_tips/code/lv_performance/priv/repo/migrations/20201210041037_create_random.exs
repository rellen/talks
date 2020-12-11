defmodule LvPerformance.Repo.Migrations.CreateRandom do
  use Ecto.Migration

  def change do
    create table(:random) do

      timestamps()
    end

  end
end

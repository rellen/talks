defmodule LvPerformance.PerformanceTest do
  use LvPerformance.DataCase

  alias LvPerformance.Performance

  describe "random" do
    alias LvPerformance.Performance.Random

    @valid_attrs %{}
    @update_attrs %{}
    @invalid_attrs %{}

    def random_fixture(attrs \\ %{}) do
      {:ok, random} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Performance.create_random()

      random
    end

    test "list_random/0 returns all random" do
      random = random_fixture()
      assert Performance.list_random() == [random]
    end

    test "get_random!/1 returns the random with given id" do
      random = random_fixture()
      assert Performance.get_random!(random.id) == random
    end

    test "create_random/1 with valid data creates a random" do
      assert {:ok, %Random{} = random} = Performance.create_random(@valid_attrs)
    end

    test "create_random/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Performance.create_random(@invalid_attrs)
    end

    test "update_random/2 with valid data updates the random" do
      random = random_fixture()
      assert {:ok, %Random{} = random} = Performance.update_random(random, @update_attrs)
    end

    test "update_random/2 with invalid data returns error changeset" do
      random = random_fixture()
      assert {:error, %Ecto.Changeset{}} = Performance.update_random(random, @invalid_attrs)
      assert random == Performance.get_random!(random.id)
    end

    test "delete_random/1 deletes the random" do
      random = random_fixture()
      assert {:ok, %Random{}} = Performance.delete_random(random)
      assert_raise Ecto.NoResultsError, fn -> Performance.get_random!(random.id) end
    end

    test "change_random/1 returns a random changeset" do
      random = random_fixture()
      assert %Ecto.Changeset{} = Performance.change_random(random)
    end
  end
end

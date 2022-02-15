defmodule PromExDemo.TodoTest do
  use PromExDemo.DataCase

  alias PromExDemo.Todo

  describe "todu" do
    alias PromExDemo.Todo.Todu

    @valid_attrs %{done: true, title: "some title"}
    @update_attrs %{done: false, title: "some updated title"}
    @invalid_attrs %{done: nil, title: nil}

    def todu_fixture(attrs \\ %{}) do
      {:ok, todu} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Todo.create_todu()

      todu
    end

    test "list_todu/0 returns all todu" do
      todu = todu_fixture()
      assert Todo.list_todu() == [todu]
    end

    test "get_todu!/1 returns the todu with given id" do
      todu = todu_fixture()
      assert Todo.get_todu!(todu.id) == todu
    end

    test "create_todu/1 with valid data creates a todu" do
      assert {:ok, %Todu{} = todu} = Todo.create_todu(@valid_attrs)
      assert todu.done == true
      assert todu.title == "some title"
    end

    test "create_todu/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Todo.create_todu(@invalid_attrs)
    end

    test "update_todu/2 with valid data updates the todu" do
      todu = todu_fixture()
      assert {:ok, %Todu{} = todu} = Todo.update_todu(todu, @update_attrs)
      assert todu.done == false
      assert todu.title == "some updated title"
    end

    test "update_todu/2 with invalid data returns error changeset" do
      todu = todu_fixture()
      assert {:error, %Ecto.Changeset{}} = Todo.update_todu(todu, @invalid_attrs)
      assert todu == Todo.get_todu!(todu.id)
    end

    test "delete_todu/1 deletes the todu" do
      todu = todu_fixture()
      assert {:ok, %Todu{}} = Todo.delete_todu(todu)
      assert_raise Ecto.NoResultsError, fn -> Todo.get_todu!(todu.id) end
    end

    test "change_todu/1 returns a todu changeset" do
      todu = todu_fixture()
      assert %Ecto.Changeset{} = Todo.change_todu(todu)
    end
  end
end

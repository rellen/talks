defmodule PromExDemoWeb.ToduControllerTest do
  use PromExDemoWeb.ConnCase

  alias PromExDemo.Todo
  alias PromExDemo.Todo.Todu

  @create_attrs %{
    done: true,
    title: "some title"
  }
  @update_attrs %{
    done: false,
    title: "some updated title"
  }
  @invalid_attrs %{done: nil, title: nil}

  def fixture(:todu) do
    {:ok, todu} = Todo.create_todu(@create_attrs)
    todu
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all todu", %{conn: conn} do
      conn = get(conn, Routes.todu_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create todu" do
    test "renders todu when data is valid", %{conn: conn} do
      conn = post(conn, Routes.todu_path(conn, :create), todu: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.todu_path(conn, :show, id))

      assert %{
               "id" => id,
               "done" => true,
               "title" => "some title"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.todu_path(conn, :create), todu: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update todu" do
    setup [:create_todu]

    test "renders todu when data is valid", %{conn: conn, todu: %Todu{id: id} = todu} do
      conn = put(conn, Routes.todu_path(conn, :update, todu), todu: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.todu_path(conn, :show, id))

      assert %{
               "id" => id,
               "done" => false,
               "title" => "some updated title"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, todu: todu} do
      conn = put(conn, Routes.todu_path(conn, :update, todu), todu: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete todu" do
    setup [:create_todu]

    test "deletes chosen todu", %{conn: conn, todu: todu} do
      conn = delete(conn, Routes.todu_path(conn, :delete, todu))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.todu_path(conn, :show, todu))
      end
    end
  end

  defp create_todu(_) do
    todu = fixture(:todu)
    %{todu: todu}
  end
end

defmodule PromExDemoWeb.ToduController do
  use PromExDemoWeb, :controller

  alias PromExDemo.Todo
  alias PromExDemo.Todo.Todu

  action_fallback PromExDemoWeb.FallbackController

  def index(conn, _params) do
    todu = Todo.list_todu()

    conn
    |> put_status(400)
    |> render("index.json", todu: todu)
  end

  def create(conn, %{"todu" => todu_params}) do
    with {:ok, %Todu{} = todu} <- Todo.create_todu(todu_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.todu_path(conn, :show, todu))
      |> render("show.json", todu: todu)
    end
  end

  def show(conn, %{"id" => id}) do
    todu = Todo.get_todu!(id)
    render(conn, "show.json", todu: todu)
  end

  def update(conn, %{"id" => id, "todu" => todu_params}) do
    todu = Todo.get_todu!(id)

    with {:ok, %Todu{} = todu} <- Todo.update_todu(todu, todu_params) do
      render(conn, "show.json", todu: todu)
    end
  end

  def delete(conn, %{"id" => id}) do
    todu = Todo.get_todu!(id)

    with {:ok, %Todu{}} <- Todo.delete_todu(todu) do
      send_resp(conn, :no_content, "")
    end
  end
end

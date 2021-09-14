defmodule PromExDemoWeb.TodoView do
  use PromExDemoWeb, :view
  alias PromExDemoWeb.TodoView

  def render("index.json", %{todo: todo}) do
    %{data: render_many(todo, TodoView, "todo.json")}
  end

  def render("show.json", %{todo: todo}) do
    %{data: render_one(todo, TodoView, "todo.json")}
  end

  def render("todo.json", %{todo: todo}) do
    %{id: todo.id,
      title: todo.title,
      done: todo.done}
  end
end

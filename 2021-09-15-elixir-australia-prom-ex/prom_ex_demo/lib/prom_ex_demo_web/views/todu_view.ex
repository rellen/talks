defmodule PromExDemoWeb.ToduView do
  use PromExDemoWeb, :view
  alias PromExDemoWeb.ToduView

  def render("index.json", %{todu: todu}) do
    %{data: render_many(todu, ToduView, "todu.json")}
  end

  def render("show.json", %{todu: todu}) do
    %{data: render_one(todu, ToduView, "todu.json")}
  end

  def render("todu.json", %{todu: todu}) do
    %{id: todu.id,
      title: todu.title,
      done: todu.done}
  end
end

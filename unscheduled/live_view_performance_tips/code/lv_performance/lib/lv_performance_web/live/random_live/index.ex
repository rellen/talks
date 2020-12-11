defmodule LvPerformanceWeb.RandomLive.Index do
  use LvPerformanceWeb, :live_view
  require Logger

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :random_collection, create_random())}
  end

  @impl true
  def handle_params(params, _url, socket) do
    Process.send_after(self(), :update, 5000)
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Listing Random")
    |> assign(:random, nil)
  end

  @impl true
  def handle_info(:update, socket) do
    Process.send_after(self(), :update, 5000)

    {:noreply,
     socket |> assign(:random_collection, socket.assigns.random_collection |> update_random())}
  end

  defp create_random() do
    1..100
    |> Enum.map(fn _ -> {random_string(64), random_string(4)} end)
  end

  defp update_random(collection) do
    collection |> Enum.map(fn {key, _} -> {key, random_string(4)} end)
  end

  defp random_string(length) do
    :crypto.strong_rand_bytes(length) |> Base.url_encode64() |> binary_part(0, length)
  end
end

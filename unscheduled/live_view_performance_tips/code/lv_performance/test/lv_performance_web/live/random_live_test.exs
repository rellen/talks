defmodule LvPerformanceWeb.RandomLiveTest do
  use LvPerformanceWeb.ConnCase

  import Phoenix.LiveViewTest

  alias LvPerformance.Performance

  @create_attrs %{}
  @update_attrs %{}
  @invalid_attrs %{}

  defp fixture(:random) do
    {:ok, random} = Performance.create_random(@create_attrs)
    random
  end

  defp create_random(_) do
    random = fixture(:random)
    %{random: random}
  end

  describe "Index" do
    setup [:create_random]

    test "lists all random", %{conn: conn, random: random} do
      {:ok, _index_live, html} = live(conn, Routes.random_index_path(conn, :index))

      assert html =~ "Listing Random"
    end

    test "saves new random", %{conn: conn} do
      {:ok, index_live, _html} = live(conn, Routes.random_index_path(conn, :index))

      assert index_live |> element("a", "New Random") |> render_click() =~
               "New Random"

      assert_patch(index_live, Routes.random_index_path(conn, :new))

      assert index_live
             |> form("#random-form", random: @invalid_attrs)
             |> render_change() =~ "can&apos;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#random-form", random: @create_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.random_index_path(conn, :index))

      assert html =~ "Random created successfully"
    end

    test "updates random in listing", %{conn: conn, random: random} do
      {:ok, index_live, _html} = live(conn, Routes.random_index_path(conn, :index))

      assert index_live |> element("#random-#{random.id} a", "Edit") |> render_click() =~
               "Edit Random"

      assert_patch(index_live, Routes.random_index_path(conn, :edit, random))

      assert index_live
             |> form("#random-form", random: @invalid_attrs)
             |> render_change() =~ "can&apos;t be blank"

      {:ok, _, html} =
        index_live
        |> form("#random-form", random: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.random_index_path(conn, :index))

      assert html =~ "Random updated successfully"
    end

    test "deletes random in listing", %{conn: conn, random: random} do
      {:ok, index_live, _html} = live(conn, Routes.random_index_path(conn, :index))

      assert index_live |> element("#random-#{random.id} a", "Delete") |> render_click()
      refute has_element?(index_live, "#random-#{random.id}")
    end
  end

  describe "Show" do
    setup [:create_random]

    test "displays random", %{conn: conn, random: random} do
      {:ok, _show_live, html} = live(conn, Routes.random_show_path(conn, :show, random))

      assert html =~ "Show Random"
    end

    test "updates random within modal", %{conn: conn, random: random} do
      {:ok, show_live, _html} = live(conn, Routes.random_show_path(conn, :show, random))

      assert show_live |> element("a", "Edit") |> render_click() =~
               "Edit Random"

      assert_patch(show_live, Routes.random_show_path(conn, :edit, random))

      assert show_live
             |> form("#random-form", random: @invalid_attrs)
             |> render_change() =~ "can&apos;t be blank"

      {:ok, _, html} =
        show_live
        |> form("#random-form", random: @update_attrs)
        |> render_submit()
        |> follow_redirect(conn, Routes.random_show_path(conn, :show, random))

      assert html =~ "Random updated successfully"
    end
  end
end

defmodule PromExDemo.Todo do
  @moduledoc """
  The Todo context.
  """

  import Ecto.Query, warn: false
  alias PromExDemo.Repo

  alias PromExDemo.Todo.Todu

  @doc """
  Returns the list of todu.

  ## Examples

      iex> list_todu()
      [%Todu{}, ...]

  """
  def list_todu do
    Repo.all(Todu)
  end

  @doc """
  Gets a single todu.

  Raises `Ecto.NoResultsError` if the Todu does not exist.

  ## Examples

      iex> get_todu!(123)
      %Todu{}

      iex> get_todu!(456)
      ** (Ecto.NoResultsError)

  """
  def get_todu!(id), do: Repo.get!(Todu, id)

  @doc """
  Creates a todu.

  ## Examples

      iex> create_todu(%{field: value})
      {:ok, %Todu{}}

      iex> create_todu(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_todu(attrs \\ %{}) do
    %Todu{}
    |> Todu.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a todu.

  ## Examples

      iex> update_todu(todu, %{field: new_value})
      {:ok, %Todu{}}

      iex> update_todu(todu, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_todu(%Todu{} = todu, attrs) do
    todu
    |> Todu.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a todu.

  ## Examples

      iex> delete_todu(todu)
      {:ok, %Todu{}}

      iex> delete_todu(todu)
      {:error, %Ecto.Changeset{}}

  """
  def delete_todu(%Todu{} = todu) do
    Repo.delete(todu)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking todu changes.

  ## Examples

      iex> change_todu(todu)
      %Ecto.Changeset{data: %Todu{}}

  """
  def change_todu(%Todu{} = todu, attrs \\ %{}) do
    Todu.changeset(todu, attrs)
  end
end

defmodule ElixirExamples do
  @moduledoc """
  Documentation for `ElixirExamples`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> ElixirExamples.hello()
      :world

  """
  def hello do
    :world
  end

  @type foobar :: :foo | {:bar, term()}
  @type my_map :: %{
          :a => number(),
          :b => binary(),
          :c => foobar(),
          optional(String.t()) => %SomeStruct{}
        }

  # def with_example do
  #   case get_client(creds) do
  #     {:ok, client} ->
  #       case get_api_data(client) do
  #         {:ok, data} -> write_to_db(data)
  #         {:error, :api_error} -> ...
  #       end

  #     {:error, :no_client} ->
  #       ...
  #   end

  #   with {:ok, client} <- get_client(creds),
  #        {:ok, data} <- get_api_data(client) do
  #     write_to_db(data)
  #   else
  #     {:error, :no_client} -> ...
  #     {:error, :api_error} -> ...
  #   end
  # end

  # defp get_client(_) do
  #   {:ok, :client}
  # end

  # defp get_api_data(_) do
  #   {:ok, :data}
  # end

  # defp write_to_db(_) do
  #   :ok
  # end
end

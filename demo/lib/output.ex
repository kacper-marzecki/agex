defmodule Demo do
  @moduledoc """
  Documentation for `Demo`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Demo.hello()
      :world

  """
  def hello(str) do
    App.Example.parse(str)
    |> IO.inspect()
  end
end

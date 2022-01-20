defmodule App.Example do
  alias App.Example.Types

  alias App.ErrorChecker

  def parse(input) do
    error_fn = ErrorChecker.unfortunate_error_checker("13")

    lucky_string = "42"

    a = %{1 => 2, 3 => 4}

    lucky_number = 42

    case input do
      ^lucky_string -> {:ok, lucky_number}
      other -> {:error, error_fn.(input)}
    end
  end

  def match_map() do
    map = %{:value => 1, :nested_map => %{1 => "one", "string_key" => {:tuple_elem, 2}}}

    case map do
      %{
        :nested_map => %{
          "string_key" => {_, x}
        }
      } ->
        x

      other ->
        1
    end
  end
end

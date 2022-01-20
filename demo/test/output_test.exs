defmodule DemoTest do
  use ExUnit.Case
  doctest Output

  test "greets the world" do
    assert Output.hello() == :world
  end
end

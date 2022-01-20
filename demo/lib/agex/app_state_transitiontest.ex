defmodule App.State.TransitionTest do
  alias App.State.Transition

  def test() do
    Transition.transition(:new, {:start, "asd"})
  end
end

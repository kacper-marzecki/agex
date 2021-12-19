defmodule App.State.Transition do
  alias App.Example.Types

  def transition(state, command) do
    case {state, command} do
      {:new, {:start, data}} -> {:ok, {:in_progress, data}}
      {{:in_progress, _}, :cancel} -> {:ok, :cancelled}
      {{:in_progress, _}, {:finish, result}} -> {:ok, {:finished, result}}
      {state, command} -> {:error, {:invalid_transition, state, command}}
    end
  end
end

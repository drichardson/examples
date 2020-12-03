defmodule Stack do
  use GenServer

  # Callbacks

  def handle_call(:pop, _from, [h|t]) do
    {:reply, h, t}
  end

  def handle_call({:push, item}, _from, state) do
    {:reply, :ok, [item | state]}
  end

  def handle_cast({:push, item}, state) do
    {:noreply, [item | state]}
  end
end


defmodule OurGenServer do
  # public api - inter process
  def print_state(pid) do
    send(pid, :print_state)
  end

  def add_to_state(pid, x) do
    send(pid, {:add_to_state, x})
  end

  # intra process
  def handle_info(:print_state, state) do
    IO.inspect(state)
    {:noreply, state}
  end

  def handle_info({:add_to_state, x}, state) do
    {:noreply, [x | state]}
  end

  def run(state \\ []) do
    receive do
      x ->
        case handle_info(x, state) do
          {:noreply, state} -> run(state)
        end
    end
  end
end

defmodule KV.Bucket do
  @doc "Starts a new bucket"
  def start_link, do: Agent.start_link(fn -> %{} end)

  @doc """
  Gets a value from the `bucket` by `key`.
  """
  def get(bucket, key), do: Agent.get(bucket, &Map.get(&1, key))

  @doc """
  Puts the `value` for the given `key` in the `bucket`.
  """
  def put(bucket, key, value) do
    Agent.update(bucket, &Map.put(&1, key, value))
    #Agent.update(bucket, fn state -> Map.put(state, key, value) end)
  end

  @doc "Deletes `key` from `bucket`"
  def delete(bucket, key), do: Agent.get_and_update(bucket, &Map.pop(&1, key))

end

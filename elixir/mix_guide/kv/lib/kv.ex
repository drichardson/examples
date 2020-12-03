defmodule KV do
  @moduledoc """
  Documentation for KV.
  """

  use Application

  def start(_type, _args) do
    KV.Supervisor.start_link
  end
end

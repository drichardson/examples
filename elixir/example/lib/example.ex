defmodule Example do
  @moduledoc """
  Documentation for Example.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Example.hello
      :world

  """
  def hello do
    :world
  end

  def main(args) do
    args |> parse_args |> process
  end

  defp parse_args(args) do
    {options, _, _} = OptionParser.parse(args, switches: [name: :string])
    options
  end

  defp process([]) do
    IO.puts "No arguments given."
  end

  defp process(options) do
    IO.puts "Hello #{options[:name]}"
  end
end

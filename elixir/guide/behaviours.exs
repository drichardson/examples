defmodule Doug.Parser do
  @callback parse(String.t) :: any
  @callback extensions() :: [String.t]
end

defmodule Doug.JSONParse do
  @behaviour Doug.Parser

  def parse(str) do
    %{ parser: "json", str: str }
  end

  def extensions(), do: ["json"]
end

defmodule Doug.YAMLParse do
  @behaviour Doug.Parser

  def parse(str), do: %{ parser: "yaml", str: str }
  def extensions(), do: ["yml"]
end

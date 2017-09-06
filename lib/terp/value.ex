defmodule Terp.Value do
  alias __MODULE__
  defstruct [:constructor, :args]

  def constructor_fn([c | xs], env) do
    args = Enum.map(xs, fn x -> x.node end)
    f = constructor_helper(args, c.node, env)
    Terp.Environment.let([c, RoseTree.new(f)], env)
  end
  defp constructor_helper([], c, _env), do: fn -> to_value(c, []) end
  defp constructor_helper([_argument | []], c, _env) do
    fn arg -> to_value(c, arg) end
  end
  defp constructor_helper([argument | arguments], c, env) do
    fn arg ->
      constructor_helper(arguments, c, fn y -> if argument == y, do: arg, else: env.(y) end)
    end
  end

  def to_value(c, xs) do
    %Value{constructor: c, args: List.wrap(xs)}
  end

  defimpl String.Chars do
    def to_string(%Value{constructor: c, args: xs}) do
      [c | xs]
      |> Enum.join(" ")
    end
  end
end

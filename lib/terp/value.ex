defmodule Terp.Value do
  alias __MODULE__
  defstruct [:constructor, :args]

  def constructor_fn([c | xs], env) do
    args = Enum.map(xs, fn x -> x.node end)
    f = constructor_helper(args, c.node, [])
    Terp.Environment.let([c, RoseTree.new(f)], env)
  end
  defp constructor_helper([], c, _env) do
    fn -> to_value(c, []) end
  end
  defp constructor_helper([_argument | []], c, env) do
    fn arg ->
      to_value(c, Enum.reverse([arg | env]))
    end
  end
  defp constructor_helper([_argument | arguments], c, env) do
    fn arg ->
      constructor_helper(arguments, c, [arg | env])
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

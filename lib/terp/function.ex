defmodule Terp.Function do
  @moduledoc """
  Function/anonymous function definition
  """

  @doc """
  Defines an anonymous function.
  """
  def lambda([%RoseTree{node: x} | [body | []]], env) do
    fn arg ->
      Terp.eval_tree(body, fn y -> if x == y, do: arg, else: env.(y) end)
    end
  end

end

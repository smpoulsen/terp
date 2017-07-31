defmodule Terp.Function do
  @moduledoc """
  Function/anonymous function definition
  """

  @doc """
  Defines an anonymous function.
  """
  def lambda([%RoseTree{node: :__quote, children: arguments} | [body | []]], env) do
    xs = Enum.map(arguments, fn x -> x.node end)
    lambda_helper(xs, body, env)
  end
  defp lambda_helper([argument | []], body, env) do
    fn arg ->
      Terp.eval_tree(body, fn y -> if argument == y, do: arg, else: env.(y) end)
    end
  end
  defp lambda_helper([argument | arguments], body, env) do
    fn arg ->
      lambda_helper(arguments, body, fn y -> if argument == y, do: arg, else: env.(y) end)
    end
  end

  @doc """
  Apply a lambda function:
  """
  def apply_lambda(func, [arg | []], env), do: func.(arg)
  def apply_lambda(func, [arg | args], env) do
    apply_lambda(func.(arg), args, env)
  end
end

defmodule Terp.Function do
  @moduledoc """
  Function/anonymous function definition and application.

  Functions in `terp` are defined with the `lambda` keyword.

  The list of arguments must be quoted; multiple arguments can be specified.

  Terp functions are curried out-of-the-box.

  ## Examples

      iex> "((lambda '(:x) (* :x :x)) 5)"
      ...> |> Terp.eval()
      25

      iex> "((lambda '(:x :y) (* :x :y)) 5 9)"
      ...> |> Terp.eval()
      45
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
      Terp.eval_expr(body, fn y -> if argument == y, do: arg, else: env.(y) end)
    end
  end
  defp lambda_helper([argument | arguments], body, env) do
    fn arg ->
      lambda_helper(arguments, body, fn y -> if argument == y, do: arg, else: env.(y) end)
    end
  end

  @doc """
  Apply a list of arguments to a lambda function one at a time.
  """
  def apply_lambda(func, [arg | []], _env), do: func.(arg)
  def apply_lambda(func, [arg | args], env) do
    apply_lambda(func.(arg), args, env)
  end
end

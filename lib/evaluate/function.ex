defmodule Terp.Evaluate.Function do
  @moduledoc """
  Function/anonymous function definition and application.

  Functions in `terp` are defined with the `lambda` keyword.

  The list of arguments must be quoted; multiple arguments can be specified.

  Terp functions are curried out-of-the-box.
  """
  alias Terp.Evaluate

  @doc """
  Defines an anonymous function.
  """
  def lambda([%RoseTree{children: arguments} | [body | []]], env) do
    xs = Enum.map(arguments, fn x -> x.node end)
    lambda_helper(xs, body, env)
  end
  defp lambda_helper([argument | []], body, env) do
    fn arg ->
      Evaluate.eval_expr(body, fn y -> if argument == y, do: arg, else: env.(y) end)
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
  def apply_lambda(func, [], _env), do: func.()
  def apply_lambda(func, [arg | []], _env), do: func.(arg)
  def apply_lambda(func, [arg | args], env) do
    apply_lambda(func.(arg), args, env)
  end

  @doc """
  Y = λf.(λx.f (x x))(λx.f (x x))

  ## Examples

      iex> Terp.Evaluate.Function.y(fn f -> fn 0 -> 1; x -> x * f.(x - 1) end end).(5)
      120
  """
  def y(f) do
    f.(fn x ->
      y(f).(x)
    end)
  end

  def letrec([name | [bound | []]], env) do
    # Make a new function wrapping bound, replacing the recursive call with a bound variable, :z
    recursive_fn = :__apply
    |> RoseTree.new([
      RoseTree.new(:__lambda),
      RoseTree.new(:__quote, [:z]),
      RoseTree.update_node(bound, name.node, :z)
    ])
    |> Evaluate.eval_expr(env)
    |> y()

    Evaluate.eval_expr(name,
      fn y ->
        fn arg ->
          if arg == y, do: recursive_fn, else: env.(arg)
        end
      end
    )
  end
end

defmodule Terp.Boolean do
  @moduledoc """
  Boolean values and conditional evaluation.
  """

  @doc """
  true

  ## Examples

      iex> Terp.Boolean.t
      true
  """
  def t(), do: true

  @doc """
  false

  ## Examples

      iex> Terp.Boolean.f
      false
  """
  def f(), do: false

  @doc """
  If - then - else conditional logic.

  ## Examples

      iex> [RoseTree.new(true), RoseTree.new(5), RoseTree.new(3)]
      ...> |> Terp.Boolean.conditional(fn x -> x end)
      5

      iex> [RoseTree.new(false), RoseTree.new(5), RoseTree.new(3)]
      ...> |> Terp.Boolean.conditional(fn x -> x end)
      3
  """
  def conditional([test | [consequent | [alternative | []]]], env) do
    if Terp.eval_expr(test, env) do
      Terp.eval_expr(consequent, env)
    else
      Terp.eval_expr(alternative, env)
    end
  end

  @doc """
  `cond/2` evaluates a list of conditions one by one until
  a true condition is found; when one is true, the body is evaluated.

  ## Examples

      iex> "(cond [(equal? 1 5) (9)] [#t 5])"
      ...> |> Terp.eval()
      5

      iex> "(cond [(equal? 1 5) (9)] [#f 5])"
      ...> |> Terp.eval()
      {:error, {:terp, :no_true_condition}}

      iex> "(cond [(equal? (+ 2 3) 5) 9] [#f 5])"
      ...> |> Terp.eval()
      9
  """
  def cond([], _env), do: {:error, {:cond, "no true condition"}}
  def cond([%{node: [condition | consequent]} | conditions], env) do
    if Terp.eval_expr(condition, env) do
      # An artifact of the parser; pulls in consequent as a list.
      consequent
      |> Enum.map(&Terp.eval_expr(&1, env))
      |> List.first
    else
      cond(conditions, env)
    end
  end

  @doc """
  Test whether two values are equal.

  ## Examples

      iex> "(equal? 5 3)" |> Terp.eval()
      false

      iex> "(equal? 5 5)" |> Terp.eval()
      true
  """
  def equal?(operands, environment) do
    case Enum.map(operands, &Terp.eval_expr(&1, environment)) do
      [x | [y | []]] ->
        x == y
      _ ->
        {:error, {:equal?, "invalid number of arguments"}}
    end
  end
end

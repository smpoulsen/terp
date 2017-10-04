defmodule Terp.Evaluate.Boolean do
  @moduledoc """
  Boolean values and conditional evaluation.
  """
  alias Terp.Evaluate

  @doc """
  true

  ## Examples

      iex> Terp.Evaluate.Boolean.t
      true
  """
  def t(), do: true

  @doc """
  false

  ## Examples

      iex> Terp.Evaluate.Boolean.f
      false
  """
  def f(), do: false

  @doc """
  If - then - else conditional logic.

  ## Examples

      iex> [RoseTree.new(true), RoseTree.new(5), RoseTree.new(3)]
      ...> |> Terp.Evaluate.Boolean.conditional(fn x -> x end)
      5

      iex> [RoseTree.new(false), RoseTree.new(5), RoseTree.new(3)]
      ...> |> Terp.Evaluate.Boolean.conditional(fn x -> x end)
      3
  """
  def conditional([test | [consequent | [alternative | []]]], env) do
    if Evaluate.eval_expr(test, env) do
      Evaluate.eval_expr(consequent, env)
    else
      Evaluate.eval_expr(alternative, env)
    end
  end

  @doc """
  `cond/2` evaluates a list of conditions one by one until
  a true condition is found; when one is true, the body is evaluated.
  """
  def cond([], _env), do: {:error, {:cond, "no true condition"}}
  def cond([%{node: [condition | consequent]} | conditions], env) do
    if Evaluate.eval_expr(condition, env) do
      # An artifact of the parser; pulls in consequent as a list.
      consequent
      |> Enum.map(&Evaluate.eval_expr(&1, env))
      |> List.first
    else
      cond(conditions, env)
    end
  end

  @doc """
  Test whether two values are equal.
  """
  def equal?(operands, environment) do
    case Enum.map(operands, &Evaluate.eval_expr(&1, environment)) do
      [x | [y | []]] ->
        x == y
      _ ->
        {:error, {:equal?, "invalid number of arguments"}}
    end
  end
end

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
end

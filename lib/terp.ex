defmodule Terp do
  @moduledoc """
  A toy interpreter.
  """
  alias Terp.AST
  alias Terp.Evaluate

  @doc """
  Evaluates a terp expression.

  Only returns the result of evaluating the code.
  """
  def eval(str) do
    {result, _environment} = eval_source(str)
    result
  end

  @doc """
  Evaluates a chunk of source by converting it to an AST and
  running it through the evaluator.
  """
  def eval_source(src, env \\ fn (z) -> {:error, {:unbound, z}} end) do
    src
    |> AST.from_src()
    |> eval_ast(env)
  end

  @doc """
  Filters comments out of the syntax tree and evaluates the remaining expressions.
  """
  def eval_ast(trees, env) when is_list(trees) do
    trees
    |> Evaluate.eval_trees(env)
  end
end

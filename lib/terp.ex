defmodule Terp do
  @moduledoc """
  A toy interpreter.
  """
  alias Terp.Parser
  alias Terp.Arithmetic
  alias Terp.Boolean
  alias Terp.Function

  @doc """
  Evaluate a terp expression.

  ## Example

      iex> "(+ 5 3)"
      ...> |> Terp.eval()
      8

      iex> "(* 2 4 5)"
      ...> |> Terp.eval()
      40

      iex> "(* 2 4 (+ 4 1))"
      ...> |> Terp.eval()
      40

      iex> "(if #t (* 5 5) (+ 4 1))"
      ...> |> Terp.eval()
      25

      iex> "(if #f (* 5 5) 5)"
      ...> |> Terp.eval()
      5
  """
  def eval(str) do
    str
    |> Parser.parse()
    |> Parser.to_tree()
    |> eval_tree()
  end

  @doc """
  Evaluate an expression represented by a parse tree.

  ## Example

      # (+ 5 3)
      iex> Terp.eval_tree(RoseTree.new("+", [5, 3]))
      8

      # (* 2 4 5)
      iex> Terp.eval_tree(RoseTree.new("*", [2, 4, 5]))
      40

      # (* 2 4 (+ 4 1))
      iex> Terp.eval_tree(RoseTree.new("*", [2, 4, RoseTree.new("+", [4, 1])]))
      40
  """
  def eval_tree(%RoseTree{node: node, children: children}, env \\ fn (_y) -> {:error, :unbound} end) do
    case node do
      x when is_number(x) -> x
      x when is_atom(x) -> env.(x)
      "#t" -> Boolean.t()
      "#f" -> Boolean.f()
      "+" -> Arithmetic.add(Enum.map(children, &(eval_tree(&1, env))))
      "*" -> Arithmetic.multiply(Enum.map(children, &(eval_tree(&1, env))))
      "-" -> Arithmetic.subtract(Enum.map(children, &(eval_tree(&1, env))))
      "/" -> Arithmetic.divide(Enum.map(children, &(eval_tree(&1, env))))
      "if" -> Boolean.conditional(Enum.map(children, &(eval_tree(&1, env))))
      "lambda" -> Function.lambda(children, env)
      x = %RoseTree{} -> # This is the case for lambda application; should be able to neaten up
          eval_tree(%RoseTree{node: eval_tree(x, env), children: children}, env)
      x when is_function(x) -> apply(x, Enum.map(children, &(eval_tree(&1, env))))
    end
  end
end

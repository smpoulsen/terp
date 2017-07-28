defmodule Terp do
  @moduledoc """
  A toy interpreter.
  """
  alias Terp.Parser
  alias Terp.Arithmetic

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
  def eval_tree(%RoseTree{node: node, children: children}) do
    children = Enum.map(children, &eval_tree/1)
    case node do
      x when is_number(x) -> x
      "+" -> Arithmetic.add(children)
      "*" -> Arithmetic.multiply(children)
      "-" -> Arithmetic.subtract(children)
      "/" -> Arithmetic.divide(children)
    end
  end
end

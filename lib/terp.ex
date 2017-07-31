defmodule Terp do
  @moduledoc """
  A toy interpreter.
  """
  alias Terp.Parser
  alias Terp.Arithmetic
  alias Terp.Boolean
  alias Terp.Function
  alias RoseTree.Zipper

  @debug false

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
    |> Enum.map(&eval_tree/1)
    |> List.first() # TODO
  end

  @doc """
  Evaluate an expression represented by a parse tree.

  ## Example

      # (+ 5 3)
      iex> "(+ 5 3)"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.Parser.to_tree()
      ...> |> Enum.map(&Terp.eval_tree/1)
      ...> |> List.first() # TODO
      8

      # (* 2 4 5)
      iex> "(* 2 4 5)"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.Parser.to_tree()
      ...> |> Enum.map(&Terp.eval_tree/1)
      ...> |> List.first() # TODO
      40

      # (* 2 4 (+ 4 1))
      iex> "(* 2 4 (+ 4 1))"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.Parser.to_tree()
      ...> |> Enum.map(&Terp.eval_tree/1)
      ...> |> List.first() # TODO
      40
  """
  def eval_tree(%RoseTree{node: node, children: children} = tree, env \\ fn (y) -> {:error, {:unbound, y}} end) do
    if @debug do
      IO.inspect({"TREE", tree})
    end
    case node do
      :__lambda ->
        Function.lambda(children, env)
      :__quote ->
        children
      :__apply ->
        [operator | operands] = Enum.map(children, &eval_tree(&1, env))
        if @debug do
          IO.inspect({"OPERATOR", operator})
          IO.inspect({"OPERANDS", operands})
        end

        case operator do
          :+ -> Arithmetic.add(operands)
          :* -> Arithmetic.multiply(operands)
          :- -> Arithmetic.subtract(operands)
          :/ -> Arithmetic.divide(operands)
          :__if -> Boolean.conditional(operands)
          true -> true
          false -> false
          x when is_function(x) -> Function.apply_lambda(operator, operands, env)
          x when is_number(x) -> x
          _  -> eval(operator, operands, env)
        end
      x when is_number(x) -> x
      #TODO Remove all of this duplication between here and apply
      "#t" -> Boolean.t()
      "#f" -> Boolean.f()
      :+ -> :+
      :* -> :*
      :- -> :-
      :/ -> :/
      "if" -> :__if
      x when is_atom(x) -> env.(x)
    end
  end

  @doc """
  Apply a list of arguments to a curried function.
  """
  def eval(operator, [h | []], env), do: operator.(env.(h))
  def eval(operator, [h | t], env), do: eval(operator.(env.(h)), t, env)
end

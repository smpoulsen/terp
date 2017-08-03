defmodule Terp do
  @moduledoc """
  A toy interpreter.
  """
  alias Terp.Parser
  alias Terp.Arithmetic
  alias Terp.Boolean
  alias Terp.Function

  @debug false

  @doc """
  Evaluate a terp expression.

  ### Notes:
  + When defining functions, e.g. `(lambda '(:x) :x)`, the arguments have to be quoted.

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
    trees = str
    |> Parser.parse()
    |> Enum.flat_map(&Parser.to_tree/1)

    trees
    |> filter_comments()
    |> eval_trees()
  end

  # Given a list of trees and an environment, evaluates the trees in
  # the context of the environment.
  defp eval_trees(_, env \\ fn (z) -> {:error, {:unbound, z}} end)
  defp eval_trees([tree | []], env), do: eval_expr(tree, env)
  defp eval_trees([tree | trees], env) do
    res = eval_expr(tree, env)
    if is_function(res) do
      eval_trees(trees, res)
    else
      eval_trees(trees, env)
    end
  end

  # Filters comments out of the AST.
  defp filter_comments(trees) do
    Enum.reject(trees, fn %RoseTree{node: node} -> node == :__comment end)
  end

  @doc """
  Evaluate an expression represented by a parse tree.

  ## Example

      # (+ 5 3)
      iex> "(+ 5 3)"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.Parser.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.eval_expr(tree, fn (z) -> {:error, {:unbound, z}} end) end)
      [8]

      # (* 2 4 5)
      iex> "(* 2 4 5)"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.Parser.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.eval_expr(tree, fn (z) -> {:error, {:unbound, z}} end) end)
      [40]

      # (* 2 4 (+ 4 1))
      iex> "(* 2 4 (+ 4 1))"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.Parser.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.eval_expr(tree, fn (z) -> {:error, {:unbound, z}} end) end)
      [40]
  """
  def eval_expr(%RoseTree{node: node, children: children} = tree, env \\ fn (y) -> {:error, {:unbound, y}} end) do
    if @debug do
      IO.inspect({"TREE", tree})
    end
    case node do
      :__lambda ->
        Function.lambda(children, env)
      :__quote ->
        children
      :__letrec ->
        Function.letrec(tree, env)
      :__let ->
        [name | [bound | []]] = children
        eval_expr(name,
          fn y ->
            fn name ->
              if y == name do
                eval_expr(bound, env)
              else
                env.(name)
              end
            end
          end)
      :__apply ->
        [operator | operands] = children
        operator = eval_expr(operator, env)
        if operator == :__if do
          Boolean.conditional(operands, env)
        else
          operands = Enum.map(operands, &eval_expr(&1, env))

          if @debug do
            IO.inspect({"OPERATOR", operator})
            IO.inspect({"OPERANDS", operands})
          end

          case operator do
            :+ -> Arithmetic.add(operands)
            :* -> Arithmetic.multiply(operands)
            :- -> Arithmetic.subtract(operands)
            :/ -> Arithmetic.divide(operands)
            :eq -> (fn [x | [y | []]] -> x == y end).(operands)
            true -> true
            false -> false
            x when is_function(x) ->
              Function.apply_lambda(operator, operands, env)
            x when is_number(x) -> x
            _  -> eval(operator, operands, env)
          end
        end
      x when is_number(x) -> x
      #TODO Remove all of this duplication between here and apply
      "#t" -> Boolean.t()
      "#f" -> Boolean.f()
      :eq -> :eq
      :+ -> :+
      :* -> :*
      :- -> :-
      :/ -> :/
      :__if -> :__if
      x -> env.(x)
    end
  end

  @doc """
  Apply a list of arguments to a curried function.
  """
  def eval(operator, [h | []], env), do: operator.(env.(h))
  def eval(operator, [h | t], env), do: eval(operator.(env.(h)), t, env)
end

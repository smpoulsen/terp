defmodule Terp do
  @moduledoc """
  A toy interpreter.
  """
  alias Terp.Parser
  alias Terp.Arithmetic
  alias Terp.Boolean
  alias Terp.Function
  alias Terp.ModuleSystem

  @debug false

  @doc """
  Evaluates a terp expression.

  Only returns the result of evaluating the code.

  ## Example

      iex> "(* 2 4 (+ 4 1))"
      ...> |> Terp.eval()
      40

      iex> "(if #t (* 5 5) (+ 4 1))"
      ...> |> Terp.eval()
      25
  """
  def eval(str) do
    {result, _environment} = evaluate_source(str)
    result
  end

  @doc """
  Loads a terp module's code and returns both the result of evaluation and
  the resulting environment.
  """
  def evaluate_source(str, env \\ fn (z) -> {:error, {:unbound_variable, z}} end) do
    str
    |> Parser.parse()
    |> Enum.flat_map(&Parser.to_tree/1)
    |> run_eval(env)

  end
  def run_eval(trees, env) do
    trees
    |> filter_nodes(:__comment)
    |> eval_trees(env)
  end

  # Given a list of trees and an environment, evaluates the trees in
  # the context of the environment.
  defp eval_trees(_, env \\ fn (z) -> {:error, {:unbound_variable, z}} end)
  defp eval_trees([tree | []], env) do
    res = eval_expr(tree, env)
    case res do
      x when is_function(x) -> {nil, res}
      x -> {x, env}
    end
  end
  defp eval_trees([tree | trees], env) do
    case eval_expr(tree, env) do
      x when is_function(x) ->
        eval_trees(trees, x)
      {:error, e} ->
        e
      _ ->
        eval_trees(trees, env)
    end
  end
  defp eval_trees(x, env), do: {{:error, {:unable_to_evaluate, x}}, env}

  # Filters nodes out of the AST.
  defp filter_nodes(trees, node_name) do
    Enum.reject(trees, fn %RoseTree{node: node} -> node == node_name end)
  end

  @doc """
  Evaluate an expression represented by a parse tree.

  ## Example

      # (+ 5 3)
      iex> "(+ 5 3)"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.Parser.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.eval_expr(tree, fn (z) -> {:error, {:unbound_variable, z}} end) end)
      [8]

      # (* 2 4 5)
      iex> "(* 2 4 5)"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.Parser.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.eval_expr(tree, fn (z) -> {:error, {:unbound_variable, z}} end) end)
      [40]

      # (* 2 4 (+ 4 1))
      iex> "(* 2 4 (+ 4 1))"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.Parser.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.eval_expr(tree, fn (z) -> {:error, {:unbound_variable, z}} end) end)
      [40]
  """
  def eval_expr(%RoseTree{node: node, children: children} = tree, env \\ fn (y) -> {:error, {:unbound_variable, y}} end) do
    if @debug do
      IO.inspect({"TREE", tree})
    end
    case node do
      :__string ->
        str = List.first(children)
        str.node
      :__lambda ->
        :__lambda
      :__require ->
        :__require
      :__provide ->
        :__provide
      :__quote ->
        Enum.map(children, &(&1.node))
      :__cond ->
        Boolean.cond(children, env)
      :__apply ->
        [operator | operands] = children
        operator = eval_expr(operator, env)
        if @debug do
          IO.inspect({"OPERATOR", operator})
          IO.inspect({"OPERANDS", operands})
        end
        case operator do
          :__if ->
            Boolean.conditional(operands, env)
          :__letrec ->
            Function.letrec(operands, env)
          :__let ->
            [name | [bound | []]] = operands
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
          :__lambda ->
            Function.lambda(operands, env)
          :__require ->
            ModuleSystem.require_modules(Enum.map(operands, &eval_expr(&1, env)), env)
          :__provide ->
            :noop
          :+ ->
            Arithmetic.add(Enum.map(operands, &eval_expr(&1, env)))
          :* ->
            Arithmetic.multiply(Enum.map(operands, &eval_expr(&1, env)))
          :- ->
            Arithmetic.subtract(Enum.map(operands, &eval_expr(&1, env)))
          :/ ->
            Arithmetic.divide(Enum.map(operands, &eval_expr(&1, env)))
          :eq ->
            (fn [x | [y | []]] -> x == y end).(Enum.map(operands, &eval_expr(&1, env)))
          :__car ->
            with operand <- List.first(operands),
                 evaluated_list <- eval_expr(operand, env),
                 true <- is_list(evaluated_list) do
              List.first(evaluated_list)
            else
              nil -> {:error, {:terp, :empty_list}}
            end
          :__cdr ->
            with operand <- List.first(operands),
                 evaluated_list <- eval_expr(operand, env),
                 true <- is_list(evaluated_list) do
              case evaluated_list do
                [] -> {:error, {:terp, :empty_list}}
                [_h | t] -> t
              end
            else
              nil -> {:error, {:terp, :empty_list}}
            end
          :__empty ->
            operands
            |> Enum.map(&eval_expr(&1, env))
            |> List.first()
            |> Enum.empty?()
          true ->
            true
          false ->
            false
          x when is_function(x) ->
            Function.apply_lambda(operator, Enum.map(operands, &eval_expr(&1, env)), env)
          x when is_number(x) -> x
          _  -> eval(operator, Enum.map(operands, &eval_expr(&1, env)), env)
        end
      x when is_number(x) -> x
      #TODO Remove all of this duplication between here and apply
      "#t" -> Boolean.t()
      "#f" -> Boolean.f()
      "equal?" -> :eq
      :+ -> :+
      :* -> :*
      :- -> :-
      :/ -> :/
      :__if -> :__if
      :__cond -> :__cond
      :__car -> :__car
      :__cdr -> :__cdr
      :__empty -> :__empty
      :__let -> :__let
      :__letrec -> :__letrec
      x -> env.(x)
    end
  end

  @doc """
  Apply a list of arguments to a curried function.
  """
  def eval(operator, [h | []], env), do: operator.(env.(h))
  def eval(operator, [h | t], env), do: eval(operator.(env.(h)), t, env)
end

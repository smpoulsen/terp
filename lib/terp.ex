defmodule Terp do
  @moduledoc """
  A toy interpreter.
  """
  alias Terp.Parser
  alias Terp.Environment
  alias Terp.ModuleSystem
  alias Terp.Arithmetic
  alias Terp.Boolean
  alias Terp.Function

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
      :__quote ->
        Enum.map(children, &(&1.node))
      :__cond ->
        Boolean.cond(children, env)
      :"__#t" ->
        Boolean.t()
      :"__#f" ->
        Boolean.f()
      :__apply ->
        [operator | operands] = children
        operator = eval_expr(operator, env)
        case operator do
          :__if ->
            Boolean.conditional(operands, env)
          :__letrec ->
            Function.letrec(operands, env)
          :__let ->
            Environment.let(operands, env)
          :__lambda ->
            Function.lambda(operands, env)
          :__require ->
            ModuleSystem.require_modules(Enum.map(operands, &eval_expr(&1, env)), env)
          :__provide ->
            :noop
          :"__+" ->
            Arithmetic.add(Enum.map(operands, &eval_expr(&1, env)))
          :"__*" ->
            Arithmetic.multiply(Enum.map(operands, &eval_expr(&1, env)))
          :"__-" ->
            Arithmetic.subtract(Enum.map(operands, &eval_expr(&1, env)))
          :__div ->
            Arithmetic.divide(Enum.map(operands, &eval_expr(&1, env)))
          :__equal? ->
            (fn [x | [y | []]] -> x == y end).(Enum.map(operands, &eval_expr(&1, env)))
          :__cons ->
            Terp.List.cons(operands, env)
          :__car ->
            Terp.List.car(operands, env)
          :__cdr ->
            Terp.List.cdr(operands, env)
          :__empty? ->
            operands
            |> Enum.map(&eval_expr(&1, env))
            |> List.first()
            |> Enum.empty?()
          x when is_function(x) ->
            Function.apply_lambda(operator, Enum.map(operands, &eval_expr(&1, env)), env)
          _ ->
            {:error, {:not_a_procedure, operator}}
        end
      x when is_number(x) -> x
      x ->
        with true <- is_atom(x),
             s <- Atom.to_string(x),
             true <- String.starts_with?(s, "__") do
          x
        else
          _ -> env.(x)
        end
    end
  end
end

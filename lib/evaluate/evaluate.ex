defmodule Terp.Evaluate do
  @moduledoc """
  This module contains the core evaluation logic.
  """
  alias Terp.Error
  alias Terp.ModuleSystem
  alias Terp.Value
  alias Terp.Evaluate.Arithmetic
  alias Terp.Evaluate.Boolean
  alias Terp.Evaluate.Environment
  alias Terp.Evaluate.Function
  alias Terp.Evaluate.List, as: TerpList
  alias Terp.Evaluate.Match
  alias Terp.TypeSystem

  @debug false

  # Given a list of trees and an environment, evaluates the trees in
  # the context of the environment.
  def eval_trees(x, env) when is_bitstring(x), do: {x, env}
  def eval_trees(exprs, env) do
    res = exprs
    |> Enum.reduce({:ok, {:environment, env}}, fn
      (expr, {:ok, {:environment, environment}}) ->
        eval_tree(expr, environment)
      (expr, {:ok, {:evaluated, _result, environment}}) ->
        eval_tree(expr, environment)
      (_expr, {:error, error, environment}) ->
        {{:error, error}, environment}
    end)
    case res do
      {:ok, {:environment, environment}} ->
        {nil, environment}
      {:ok, {:evaluated, result, env}} ->
        {result, env}
      {:error, e, _environment} ->
        {{:error, e}, env}
      %Error{} = e ->
        {e, env}
      x ->
        {x, env}
    end
  end

  @doc """
  Evaluate a single AST.
  """
  def eval_tree(expr, env), do: eval_tree(expr, env, verbose: false)
  def eval_tree(expr, env, verbose: verbose) do
    case eval_expr(expr, env) do
      x when is_function(x) ->
        {:ok, {:environment, x}}
      {{:ok, msg}, env} ->
        {:ok, {:evaluated, msg, env}}
      {:error, error} ->
        {:error, error, env}
      %Error{} = error ->
        {:error, error, env}
      result ->
        if verbose do
          {:ok, {:evaluated, result, env, expr}}
        else
          {:ok, {:evaluated, result, env}}
        end
    end
  end

  @doc """
  Evaluate an expression represented by an AST.

  ## Example

      # (+ 5 3)
      iex> "(+ 5 3)"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.AST.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.Evaluate.eval_expr(tree, fn (z) -> {:error, {:unbound, z}} end) end)
      [8]

      # (* 2 4 5)
      iex> "(* 2 4 5)"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.AST.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.Evaluate.eval_expr(tree, fn (z) -> {:error, {:unbound, z}} end) end)
      [40]

      # (* 2 4 (+ 4 1))
      iex> "(* 2 4 (+ 4 1))"
      ...> |> Terp.Parser.parse()
      ...> |> Enum.flat_map(&Terp.AST.to_tree/1)
      ...> |> Enum.map(fn tree -> Terp.Evaluate.eval_expr(tree, fn (z) -> {:error, {:unbound, z}} end) end)
      [40]
  """
  def eval_expr(%RoseTree{node: node, children: children} = tree, env \\ fn (y) -> {:error, {:unbound, y}} end) do
    if @debug do
      IO.inspect(tree, label: "EVAL TREE :")
    end
    case node do
      :__data ->
        [_type_constructor, value_constructors] = children
        constructors = value_constructors.node
        env_prime = Enum.reduce(constructors, env, fn c, env ->  Value.constructor_fn(c, env) end)
        env_prime
      :__instance ->
        [_class, %{node: definitions}] = children
        Enum.reduce(definitions, env, fn (defn, env) ->
          eval_expr(defn, env)
        end)
      :__string ->
        str = List.first(children)
        str.node
      :__quote ->
        Environment.quote(children)
      :__cond ->
        Boolean.cond(children, env)
      :__match ->
        case Match.match(children, env) do
          %Error{} = e ->
            %{e | in_expression: tree}
          res ->
            res
        end
      :"__#t" ->
        Boolean.t()
      :"__#f" ->
        Boolean.f()
      :__let_values ->
        [%RoseTree{node: bindings} | [expr | []]] = children
        local_env = Enum.reduce(bindings, env, fn (binding, env) ->
          Environment.let(binding, env)
        end)
        eval_expr(expr, local_env)
      :__beam ->
        [%RoseTree{node: module} | [%RoseTree{node: function} | []]] = children
        module_first_char = String.first(module)
        is_capitalized? = String.upcase(module_first_char) == module_first_char
        fully_qualified_module = (if is_capitalized?, do: ("Elixir." <> module), else: module)
        |> String.to_atom
        {:__beam, curry(fn (args) -> apply(fully_qualified_module, function, args) end)}
      :__apply ->
        [operator | operands] = children
        operator = eval_operator_with_type_class(operator, tree, env)
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
            operands
            |> Enum.map(&(&1.node))
            |> ModuleSystem.require_modules(env)
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
            Boolean.equal?(operands, env)
          :__cons ->
            TerpList.cons(operands, env)
          :__car ->
            TerpList.car(operands, env)
          :__cdr ->
            TerpList.cdr(operands, env)
          :__empty? ->
            TerpList.empty?(operands, env)
          x when is_function(x) ->
            Function.apply_lambda(operator, Enum.map(operands, &eval_expr(&1, env)), env)
          {:error, reason} ->
            {:error, reason}
          {:__beam, function} ->
            evald_operands = Enum.map(operands, &eval_expr(&1, env))
            function.(evald_operands)
          x when is_boolean(x) ->
            {x, env}
          _ ->
            {:error, {:not_a_procedure, operator}}
        end
      x when is_number(x) -> x
      x when is_function(x) -> x
      x when is_boolean(x) -> x
      x ->
        if builtin?(x), do: x, else: env.(x)
    end
  end

  defp eval_operator_with_type_class(%{node: function} = operator, tree, env) do
    with false <- builtin?(function),
         {:ok, defn} <- TypeSystem.lookup_class_defn(function, tree) do
      eval_expr(defn, env).(function)
    else
      _ ->
        eval_expr(operator, env)
    end
  end

  defp builtin?(function) do
    with true <- is_atom(function),
         s <- Atom.to_string(function) do
           String.starts_with?(s, "__")
    else
      _ -> false
    end
  end

  def curry(fun) do
    {_, arity} = :erlang.fun_info(fun, :arity)
    curry(fun, arity, [])
  end

  def curry(fun, 0, arguments) do
    apply(fun, Enum.reverse arguments)
  end

  def curry(fun, arity, arguments) do
    fn arg -> curry(fun, arity - 1, [arg | arguments]) end
  end
end

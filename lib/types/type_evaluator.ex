defmodule Terp.Types.TypeEvaluator do
  alias Terp.Types.Types
  alias Terp.Types.TypeEvaluator
  alias Terp.Types.TypeVars

  defstruct [var_index: 0, errors: [], bound_vars: %{}]

  @type scheme :: {[Types.t], Types.t}
  @type type_environment :: map()
  @type substitution :: %{required(Types.t) => Types.t}
  @type errors :: [String.t]
  @type t :: %TypeEvaluator{var_index: integer(), errors: errors}

  @spec infer(RoseTree.t) :: Types.t
  def infer(%RoseTree{} = expr) do
    {_substitution, type} = infer(expr, %{})
    type
  end

  @spec infer(RoseTree.t, type_environment) :: {substitution, Type.t}
  def infer(%RoseTree{node: node, children: children}, type_env) do
    case TypeVars.start_link() do
      {:ok, _} -> :ok
      {:error, _} -> :ok
    end
    case node do
      x when is_integer(x) ->
        {null_substitution(), Types.int()}
      x when is_boolean(x) ->
        {null_substitution(), Types.bool()}
      :__string ->
        {null_substitution(), Types.string()}
      :"__#t" -> # Seems spurious, but probably don't need the when boolean?
        {null_substitution(), Types.bool()}
      :"__#f" ->
        {null_substitution(), Types.bool()}
      :__quote ->
        {type_env, sub, types} = children
        |> Enum.reduce({type_env, %{}, []},
            fn (expr, {type_env, sub, types}) ->
              {s, t} = infer(expr, type_env)
              {type_env, compose(sub, s), [t | types]}
            end
           )

        unique_types = types
        |> Enum.uniq()
        |> Enum.group_by(&(&1.constructor == :Tvar))

        case unique_types[false] do
          nil ->
            case unique_types[true] do
              nil ->
                tv = TypeVars.fresh()
                {sub, Types.list(tv)}
              [t | _ts] ->
                {sub, Types.list(t)}
            end
          [t | []] ->
            case unique_types[true] do
              nil ->
                {sub, Types.list(t)}
              vars ->
                sub = unify_list_types(Enum.map(vars, &{&1, t}))
                {sub, Types.list(t)}
            end
          ts ->
            type_strings = Enum.map(ts, &(&1.str))
            |> Enum.join(", ")
            {:error, {:type, "unable to unify: #{type_strings}"}}
        end
      :__apply ->
        [operator | operands] = children
        case operator.node do
          :"__+" ->
            t = Types.function(Types.function(Types.int(), Types.int()), Types.int())
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :"__-" ->
            t = Types.function(Types.function(Types.int(), Types.int()), Types.int())
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :"__*" ->
            t = Types.function(Types.function(Types.int(), Types.int()), Types.int())
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__div ->
            t = Types.function(Types.function(Types.int(), Types.int()), Types.int())
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__equal? ->
            # Using a single type variable because equality between different types would be ill-typed
            tv = TypeVars.fresh()
            t = Types.function(Types.function(tv, tv), Types.bool())
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__car ->
            [lst | []] = operands
            {sub, %Types{t: {:LIST, t}}} = lst
            |> infer(type_env)
            {sub, t}
          :__cdr ->
            [lst | []] = operands
            infer(lst, type_env)
          :__cons ->
            [elem | [lst | []]] = operands
            tv = TypeVars.fresh()
            {s1, %Types{t: {:LIST, t}} = list_type} = infer(lst, type_env)
            cons_type = Types.function(Types.function(t, list_type), list_type)
            infer_binary_op(type_env, cons_type, {elem, lst})
          :__let ->
            [_name | [bound | []]] = operands
            infer(bound, type_env)
          :__letrec ->
            # TODO not inferring the specific type
            [_name | [bound | []]] = operands
            {s1, t} = infer(bound, type_env)
            tv = TypeVars.fresh()
            s2 = unify(Types.function(tv, tv), t)
            {s2, apply_sub(s1, t)}
          :__if ->
            [test | [consequent | [alternative | []]]] = operands
            {s1, t1} = infer(test, type_env)
            {s2, t2} = infer(consequent, type_env)
            {s3, t3} = infer(alternative, type_env)
            s4 = unify(t1, Types.bool())
            s5 = unify(t2, t3)
            composed_scheme = s5
            |> compose(s4)
            |> compose(s3)
            |> compose(s2)
            |> compose(s1)
            {composed_scheme, apply_sub(s5, t2)}
          :__lambda ->
            [%RoseTree{node: :__apply, children: args} | body] = operands

            # Generate a fresh type variable for each argument
            type_vars = args
            |> Enum.map(fn (_arg) -> TypeVars.fresh() end)

            # Extend the type environment with the arguments
            type_env = args
            |> Enum.reverse()
            |> Enum.zip(type_vars)
            |> Enum.reduce(
              type_env,
              fn ({arg, var}, acc) -> extend(acc, {arg.node, {[arg.node], var}}) end
            )

            # Infer the type of the function body
            {_type_env, sub, fn_type} = body
            |> Enum.reduce(
              {type_env, null_substitution(), []},
              fn (expr, {type_env, _substitution, types}) ->
                {sub, type} = TypeEvaluator.infer(expr, type_env)
                {type_env, sub, [type | types]}
              end)

            substituted_type_vars = type_vars
            |> Enum.reverse()
            |> Enum.map(&apply_sub(sub, &1))
            {sub, build_up_arrows((substituted_type_vars ++ fn_type))}
          :__apply ->
            # applying a lambda
            {s1, t1} = infer(operator, type_env)
            {_type_env, {s2, ts}} = operands
            |> Enum.reduce({type_env, []},
            fn (expr, {t_env, types}) ->
              {sub_prime, type} = infer(expr, t_env)
              {type_env, {sub_prime, [type | types]}}
            end)

            tv = TypeVars.fresh()
            s3 = unify(apply_sub(s2, t1), Types.function(List.first(ts), tv))

            composed_scheme = compose(s3, compose(s2, s1))
            {composed_scheme, apply_sub(s3, tv)}
          :__provide ->
            # TODO filter our provide nodes
            {null_substitution(), nil}
          _ ->
            t = TypeVars.fresh()
            {null_substitution(), t}
        end
      _ ->
        lookup(type_env, node)
    end
  end

  @spec infer_binary_op(type_environment, Types.t, {RoseTree.t, RoseTree.t}) :: {t, {substitution, Types.t}}
  def infer_binary_op(type_env, binary_type, {arg1, arg2}) do
    tv = TypeVars.fresh()
    {s1, t1} = infer(arg1, type_env)
    {s2, t2} = infer(arg2, type_env)
    inferred_op_type = build_up_arrows([t1, t2, tv])
    s3 = unify(binary_type, inferred_op_type)
    composed_scheme = compose(s1, compose(s2, s3))
    {composed_scheme, apply_sub(s3, tv)}
  end

  @spec build_up_arrows([Types.t]) :: Types.t
  def build_up_arrows([], arrows), do: arrows
  def build_up_arrows([type | []], %Types{constructor: :Tarrow} = arrows) do
    Types.function(arrows, type)
  end
  def build_up_arrows([type | types], %Types{constructor: :Tarrow} = arrows) do
    new_arrow = Types.function(arrows, type)
    build_up_arrows(types, new_arrow)
  end
  def build_up_arrows([type1 | [type2 | types]]) do
    build_up_arrows(types, Types.function(type1, type2))
  end

  def unify_list_types(types), do: unify_list_types(types, %{})
  def unify_list_types([], unification), do: unification
  def unify_list_types([{type_var, type} | types], unification) do
    unification2 = unify(type_var, type)
    unify_list_types(types, compose(unification, unification2))
  end

  @spec extend(type_environment, {atom() | String.t, scheme}) :: type_environment
  def extend(%{} = type_env, {var, scheme}) do
    Map.put_new(type_env, var, scheme)
  end

  @spec restrict(type_environment, atom() | String.t) :: type_environment
  def restrict(%{} = type_env, var) do
    Map.drop(type_env, var)
  end

  @spec lookup(type_environment, atom() | String.t) :: {t, scheme} | {:error, {:unbound, atom() | String.t}}
  def lookup(type_environment, var) do
    case Map.get(type_environment, var) do
      nil -> {:error, {:unbound, var}}
      x ->
      {null_substitution(), instantiate(x)}
    end
  end

  @doc """
  Instantiate a type
  """
  def instantiate({xs, t}) do
    fresh_type_vars = xs
    |> Enum.map(fn (_x) -> TypeVars.fresh() end)

    type = xs
    |> Enum.zip(fresh_type_vars)
    |> Map.new()
    |> apply_sub(t)

    type
  end

  @doc """
  Generalize a bound type
  """
  def generalize(type_env, type) do
    xs = type
    |> MapSet.difference(ftv(type_env))
    |> ftv()
    |> MapSet.to_list()
    {xs, type}
  end

  ## Substitutions
  def null_substitution() do
    Map.new()
  end

  @spec compose(substitution, substitution) :: substitution
  def compose(sub1, sub2) do
    Map.merge(sub2, sub1, fn _k, v1, _v2 -> v1 end)
    |> Enum.map(fn {t_var, t_scheme} -> {t_var, apply_sub(sub1, t_scheme)} end)
    |> Map.new()
  end

  @spec apply_sub(substitution, Types.t) :: Types.t
  def apply_sub(_, %Types{constructor: :Tconst} = type), do: type
  def apply_sub(_, %Types{constructor: :Tlist} = type), do: type
  def apply_sub(substitution, %Types{constructor: :Tvar, t: t} = type) do
    Map.get(substitution, t, type)
  end
  def apply_sub(substitution, %Types{constructor: :Tarrow, t: {t1, t2}}) do
    Types.function(apply_sub(substitution, t1), apply_sub(substitution, t2))
  end
  def apply_sub(substitution, {as, t} = _type_scheme) do
    substitution_prime = as
    |> Enum.reduce(substitution, fn (type_var, new_sub) ->
      Map.drop(new_sub, type_var) end
    )
    t_prime = apply_sub(substitution_prime, t)
    {as, t_prime}
  end
  def apply_sub(substitution, xs) when is_list(xs) do
    Enum.map(xs, &apply(substitution, &1))
  end
  def apply_sub(substitution, %{} = type_env) do
    type_env
    |> Enum.map(fn {k, v} -> {k, apply_sub(substitution, v)} end)
    |> Map.new()
  end

  @doc """
  Query free type variables.
  """
  @spec ftv(Types.t | scheme | [Types.t] | type_environment) :: MapSet.t
  def ftv(%Types{constructor: :Tconst}), do: MapSet.new()
  def ftv(%Types{constructor: :Tlist, t: {:LIST, t}}), do: ftv(t)
  def ftv(%Types{constructor: :Tvar} = type), do: MapSet.new([type])
  def ftv(%Types{constructor: :Tarrow, t: {t1, t2}}) do
    MapSet.union(ftv(t1), ftv(t2))
  end
  def ftv({as, type}) do
    ftv(MapSet.difference(type, MapSet.new(as)))
  end
  def ftv(xs) when is_list(xs) do
    Enum.reduce(xs, MapSet.new(), fn (x, acc) -> MapSet.union(ftv(x), acc) end)
  end
  def ftv(%{} = type_env) do
    type_env
    |> Map.values()
    |> ftv()
  end

  ## Type unification
  @spec unify(Types.t, Types.t) :: {substitution, Types.t}
  def unify(%Types{constructor: :Tvar, t: a}, type), do: bind(a, type)
  def unify(type, %Types{constructor: :Tvar, t: a}), do: bind(a, type)
  def unify(%Types{constructor: :Tconst, t: a}, %Types{constructor: :Tconst, t: a}) do
    null_substitution()
  end
  def unify(%Types{constructor: :Tlist, t: {:LIST, t1}}, %Types{constructor: :Tlist, t: {:LIST, t2}}) do
    unify(t1, t2)
  end
  def unify(%Types{constructor: :Tarrow, t: {l1, r1}}, %Types{constructor: :Tarrow, t: {l2, r2}}) do
    s1 = unify(l1, l2)
    s2 = unify(apply_sub(s1, r1), apply_sub(s1, r2))
    compose(s2, s1)
  end
  def unify(t1, t2), do: {:error, {:type, "Unable to unify #{t1.str} with #{t2.str}"}}

  @spec bind(Types.t, Types.t) :: {t, substitution}
  def bind(a, type) do
    cond do
      a == type.t -> null_substitution()
      occurs_check(a, type) -> {:error, {:type, "Unable to construct infinite type"}}
      true -> %{a => type}
    end
  end

  def occurs_check(a, type) do
    MapSet.member?(ftv(type), a)
  end
end

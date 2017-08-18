defmodule Terp.Types.TypeEvaluator do
  alias Terp.Types.Types
  alias Terp.Types.TypeEvaluator

  defstruct [var_index: 0, errors: []]

  @type scheme :: {[Types.t], Types.t}
  @type type_environment :: map()
  @type substitution :: %{required(Types.t) => Types.t}
  @type errors :: [String.t]
  @type t :: %TypeEvaluator{var_index: integer(), errors: errors}

  @spec infer(RoseTree.t) :: Types.t
  def infer(%RoseTree{} = expr) do
    {_eval_env, {_substitution, type}} = infer(expr, %{}, %TypeEvaluator{})
    type
  end

  @spec infer(RoseTree.t, type_environment, t) :: {t, {substitution, Type.t}}
  def infer(%RoseTree{node: node, children: children}, type_env, %TypeEvaluator{} = eval_env) do
    case node do
      x when is_integer(x) ->
        {eval_env, {null_substitution(), Types.int()}}
      x when is_boolean(x) ->
        {eval_env, {null_substitution(), Types.bool()}}
      :__string ->
        {eval_env, {null_substitution(), Types.string()}}
      :"__#t" -> # Seems spurious, but probably don't need the when boolean?
        {eval_env, {null_substitution(), Types.bool()}}
      :"__#f" ->
        {eval_env, {null_substitution(), Types.bool()}}
      :__quote ->
        {eval_env, type_env, sub, types} = children
        |> Enum.reduce({eval_env, type_env, %{}, []},
            fn (expr, {eval_env, type_env, sub, types}) ->
              {eval_env, {s, t}} = infer(expr, type_env, eval_env)
              {eval_env, type_env, compose(sub, s), [t | types]}
            end
           )

        unique_types = types
        |> Enum.uniq()
        |> Enum.group_by(&(&1.constructor == :Tvar))

        # unique_types[false] = :Tconst, :Tarrow, :Tlist
        # unique_types[true] = :Tvar
        case unique_types[false] do
          nil ->
            case unique_types[true] do
              nil ->
                {eval_env, tv} = fresh_type_var(eval_env)
                {eval_env, {sub, Types.list(tv)}}
              [t | _ts] ->
                {eval_env, {sub, Types.list(t)}}
            end
          [t | []] ->
            case unique_types[true] do
              nil ->
                {eval_env, {sub, Types.list(t)}}
              vars ->
                {eval_env, sub} = unify_list_types(eval_env, Enum.flat_map(vars, &[&1, t]))
                {eval_env, {sub, Types.list(t)}}
            end
          ts ->
            type_strings = Enum.map(ts, &(&1.str))
            |> Enum.join(", ")
            {eval_env, {:error, {:type, "unable to unify: #{type_strings}"}}}
        end
      :__apply ->
        [operator | operands] = children
        case operator.node do
          :"__+" ->
            t = Types.function(Types.function(Types.int(), Types.int()), Types.int())
            infer_binary_op(eval_env, type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :"__-" ->
            t = Types.function(Types.function(Types.int(), Types.int()), Types.int())
            infer_binary_op(eval_env, type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :"__*" ->
            t = Types.function(Types.function(Types.int(), Types.int()), Types.int())
            infer_binary_op(eval_env, type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__div ->
            t = Types.function(Types.function(Types.int(), Types.int()), Types.int())
            infer_binary_op(eval_env, type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__equal? ->
            # Using a single type variable because equality between different types would be ill-typed
            {eval_env, tv} = fresh_type_var(eval_env)
            t = Types.function(Types.function(tv, tv), Types.bool())
            infer_binary_op(eval_env, type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__let ->
            [_name | [bound | []]] = operands
            infer(bound, type_env, eval_env)
          :__letrec ->
            # TODO not inferring the specific type
            [_name | [bound | []]] = operands
            {eval_env, {s1, t}} = infer(bound, type_env, eval_env)
            {eval_env, tv} = fresh_type_var(eval_env)
            {eval_env, s2} = unify(eval_env, Types.function(tv, tv), t)
            {eval_env, {s2, apply_sub(s1, t)}}
          :__if ->
            [test | [consequent | [alternative | []]]] = operands
            {eval_env, {s1, t1}} = infer(test, type_env, eval_env)
            {eval_env, {s2, t2}} = infer(consequent, type_env, eval_env)
            {eval_env, {s3, t3}} = infer(alternative, type_env, eval_env)
            {eval_env, s4} = unify(eval_env, t1, Types.bool())
            {eval_env, s5} = unify(eval_env, t2, t3)
            composed_scheme = s5
            |> compose(s4)
            |> compose(s3)
            |> compose(s2)
            |> compose(s1)
            {eval_env, {composed_scheme, apply_sub(s5, t2)}}
          :__lambda ->
            [%RoseTree{node: :__apply, children: args} | body] = operands

            # Generate a fresh type variable for each argument
            {eval_env, type_vars} = args
            |> Enum.reduce(
              {eval_env, []},
            fn (_arg, {env, vars}) ->
              {new_env, var} = fresh_type_var(env)
              {new_env, [var | vars]}
            end
            )

            # Extend the type environment with the arguments
            type_env = args
            |> Enum.reverse()
            |> Enum.zip(type_vars)
            |> Enum.reduce(
              type_env,
              fn ({arg, var}, acc) -> extend(acc, {arg.node, {[arg.node], var}}) end
            )

            # Infer the type of the function body
            {eval_env, _type_env, sub, fn_type} = body
            |> Enum.reduce(
              {eval_env, type_env, null_substitution(), []},
              fn (expr, {eval_env, type_env, _substitution, types}) ->
                {f, {sub, type}} = TypeEvaluator.infer(expr, type_env, eval_env)
                {f, type_env, sub, [type | types]}
              end)

            substituted_type_vars = type_vars
            |> Enum.reverse()
            |> Enum.map(&apply_sub(sub, &1))
            {eval_env, {sub, build_up_arrows((substituted_type_vars ++ fn_type))}}
          :__apply ->
            # applying a lambda
            {env_prime, {s1, t1}} = infer(operator, type_env, eval_env)
            {env_prime, _type_env, {s2, ts}} = operands
            |> Enum.reduce({env_prime, type_env, []},
            fn (expr, {e_env, t_env, types}) ->
              {env, {sub_prime, type}} = infer(expr, t_env, e_env)
              {env, type_env, {sub_prime, [type | types]}}
            end)

            {env_prime, tv} = fresh_type_var(env_prime)
            {env_prime, s3} = unify(env_prime, apply_sub(s2, t1), Types.function(List.first(ts), tv))

            composed_scheme = compose(s3, compose(s2, s1))
            {env_prime, {composed_scheme, apply_sub(s3, tv)}}
          _ ->
            {eval_env, t} = fresh_type_var(eval_env)
            {eval_env, {null_substitution(), t}}
        end
      _ ->
        lookup(eval_env, type_env, node)
    end
  end

  @spec infer_binary_op(t, type_environment, Types.t, {RoseTree.t, RoseTree.t}) :: {t, {substitution, Types.t}}
  def infer_binary_op(eval_env, type_env, binary_type, {arg1, arg2}) do
    {eval_env, tv} = fresh_type_var(eval_env)
    {eval_env, {s1, t1}} = infer(arg1, type_env, eval_env)
    {eval_env, {s2, t2}} = infer(arg2, type_env, eval_env)
    inferred_op_type = build_up_arrows([t1, t2, tv])
    {eval_env, s3} = unify(eval_env, binary_type, inferred_op_type)
    composed_scheme = compose(s1, compose(s2, s3))
    {eval_env, {composed_scheme, apply_sub(s3, tv)}}
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

  def unify_list_types(eval_env, types), do: unify_list_types(eval_env, types, %{})
  def unify_list_types(eval_env, [type1 | types], unification) do
    case List.pop_at(types, 0) do
      {nil, []} -> {eval_env, unification}
      {type2, _remaining_types} ->
        {eval_env, unification2} = unify(eval_env, type1, type2)
        unify_list_types(eval_env, types, compose(unification, unification2))
    end
  end

  @spec fresh_type_var(t) :: {t, Types.t}
  def fresh_type_var(%TypeEvaluator{var_index: index} = env) do
    vars = Stream.cycle([
      "a", "b", "c", "d", "e", "f", "g", "h", "i",
      "j", "k", "l", "m", "n", "o", "p", "q", "r",
      "s", "t", "u", "v", "w", "x", "y", "z"
    ])
    {%{env | var_index: index + 1}, Types.var(Enum.at(vars, index))}
  end

  @spec extend(type_environment, {atom() | String.t, scheme}) :: type_environment
  def extend(%{} = type_env, {var, scheme}) do
    Map.put_new(type_env, var, scheme)
  end

  @spec restrict(type_environment, atom() | String.t) :: type_environment
  def restrict(%{} = type_env, var) do
    Map.drop(type_env, var)
  end

  @spec lookup(t, type_environment, atom() | String.t) :: {t, scheme} | {:error, {:unbound, atom() | String.t}}
  def lookup(eval_env, type_environment, var) do
    case Map.get(type_environment, var) do
      nil -> {:error, {:unbound, var}}
      x ->
        {env_prime, t} = instantiate(eval_env, x)
        {env_prime, {null_substitution(), t}}
    end
  end

  @doc """
  Instantiate a type
  """
  def instantiate(eval_env, {xs, t}) do
    {env_prime, fresh_type_vars} = xs
    |> Enum.reduce({eval_env, []}, fn (_x, {env, vars}) ->
      {new_env, var} = fresh_type_var(env)
      {new_env, [var | vars]}
    end)
    type = xs
    |> Enum.zip(fresh_type_vars)
    |> Map.new()
    |> apply_sub(t)
    {env_prime, type}
  end

  @doc """
  Generalize a bound type
  """
  def generalize(_eval_env, type_env, type) do
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
  @spec unify(t, Types.t, Types.t) :: {t, {substitution, Types.t}}
  def unify(infer, %Types{constructor: :Tvar, t: a}, type) do
    bind(infer, a, type)
  end
  def unify(infer, type, %Types{constructor: :Tvar, t: a}) do
    bind(infer, a, type)
  end
  def unify(infer, %Types{constructor: :Tconst, t: a}, %Types{constructor: :Tconst, t: a}) do
    {infer, null_substitution()}
  end
  def unify(eval_env, %Types{constructor: :Tarrow, t: {l1, r1}}, %Types{constructor: :Tarrow, t: {l2, r2}}) do
    {eval_env, s1} = unify(eval_env, l1, l2)
    {eval_env, s2} = unify(eval_env, apply_sub(s1, r1), apply_sub(s1, r2))
    {eval_env, compose(s2, s1)}
  end
  def unify(%{errors: errors} = eval_env, t1, t2), do: {%{eval_env | errors: ["unable to unify #{t1.str} and #{t2.str}" | errors]}, %{}}

  @spec bind(t, Types.t, Types.t) :: {t, substitution}
  def bind(%{errors: errors} = eval_env, a, type) do
    cond do
      a == type.t -> {eval_env, null_substitution()}
      occurs_check(a, type) -> {%{eval_env | errors: ["infinite type" | errors]}, nil}
      true -> {eval_env, %{a => type}}
    end
  end

  def occurs_check(a, type) do
    MapSet.member?(ftv(type), a)
  end
end
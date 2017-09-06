defmodule Terp.Types.TypeEvaluator do
  alias Terp.Types.Types
  alias Terp.Types.Annotation
  alias Terp.Types.TypeVars
  alias Terp.Types.TypeEnvironment

  @type scheme :: {[Types.t], Types.t}
  @type type_environment :: map()
  @type substitution :: %{required(Types.t) => Types.t}
  @type errors :: [String.t]

  @spec run_infer(RoseTree.t) :: Types.t
  def run_infer(%RoseTree{} = expr) do
    case TypeVars.start_link() do
      {:ok, _} -> :ok
      {:error, _} -> TypeVars.reset()
    end
    case infer(expr, %{}) do
      {:ok, {_substitution, type}} ->
        if expr.node == :__type do
          :ok
        else
          case Annotation.reconcile_annotation(expr, type) do
            {:ok, annotated_type_scheme} ->
              {:ok, substitute_type_vars(annotated_type_scheme)}
            {:error, {:type, {:annotation, _map}}} = error ->
              error
            _ ->
              type_scheme = generalize(%{}, type)
              TypeEnvironment.extend_environment(expr, type)
              {:ok, substitute_type_vars(type_scheme)}
          end
        end
      {:error, _e} = error ->
        error
    end
  end

  defp format_constructor(defn) do
    [constructor | vars] = defn
    |> Enum.map(&(&1.node))
    {constructor, vars}
  end

  @spec infer(RoseTree.t, type_environment) :: {substitution, Type.t}
  def infer(%RoseTree{node: node, children: children}, type_env) do
    case node do
      :__data ->
        [type_constructor | [data_constructors | []]] = children
        {name, vars} = format_constructor(type_constructor.node)
        data_constructors = Enum.map(data_constructors.node, &format_constructor/1)
        t = Types.sum_type(name, vars, data_constructors)
        TypeEnvironment.define_type(name, t)
        {:ok, {%{}, t}}
      :__type ->
        [fn_name | [type_info | []]] = children
        Annotation.annotate_type(fn_name.node, type_info.node)
      x when is_integer(x) ->
        {:ok, {null_substitution(), Types.int()}}
      x when is_boolean(x) ->
        {:ok, {null_substitution(), Types.bool()}}
      :__string ->
        {:ok, {null_substitution(), Types.string()}}
      :"__#t" -> # Seems spurious, but probably don't need the when boolean?
        {:ok, {null_substitution(), Types.bool()}}
      :"__#f" ->
        {:ok, {null_substitution(), Types.bool()}}
      :__quote ->
        {type_env, sub, types} = children
        |> Enum.reduce({type_env, %{}, []},
          fn (_expr, {:error, e}) -> {:error, e}
            (expr, {type_env, sub1, types}) ->
              case infer(expr, type_env) do
                {:ok, {sub2, type}} ->
                  {type_env, compose(sub1, sub2), [type | types]}
                {:error, e} ->
                  {:error, e}
              end
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
                {:ok, {sub, Types.list(tv)}}
              [t | _ts] ->
                {:ok, {sub, Types.list(t)}}
            end
          [t | []] ->
            case unique_types[true] do
              nil ->
                {:ok, {sub, Types.list(t)}}
              vars ->
                case unify_list_types(Enum.map(vars, &{&1, t})) do
                  {:ok, sub} ->
                    {:ok, {sub, Types.list(t)}}
                  {:error, e} ->
                    {:error, e}
                end
            end
          ts ->
            type_strings = Enum.map(ts, &(to_string(&1)))
            |> Enum.join(", ")
            {:error, {:type, "Unable to unify list types: #{type_strings}"}}
        end
      :__cond ->
        infer_cond(type_env, children)
      :__apply ->
        [operator | operands] = children
        case operator.node do
          :"__+" ->
            t = Types.function(Types.int(), Types.function(Types.int(), Types.int()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :"__-" ->
            t = Types.function(Types.int(), Types.function(Types.int(), Types.int()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :"__*" ->
            t = Types.function(Types.int(), Types.function(Types.int(), Types.int()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__div ->
            t = Types.function(Types.int(), Types.function(Types.int(), Types.int()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__equal? ->
            # Using a single type variable because equality between different types would be ill-typed
            tv = TypeVars.fresh()
            t = Types.function(tv, Types.function(tv, Types.bool()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__car ->
            [lst | []] = operands
            case infer(lst, type_env) do
              {:ok, {_s1, list_type}} ->
                case list_type do
                  %Types{constructor: :Tlist, t: t} ->
                    {:ok, {null_substitution(), t}}
                  %Types{constructor: :Tvar} ->
                    tv = TypeVars.fresh()
                    {:ok, sub} = unify(list_type, Types.list(tv))
                    {:ok, {sub, tv}}
                  _ ->
                    {:error, {:type, "Cannot unify #{list_type.str} with [a]"}}
                end
              {:error, e} ->
                {:error, e}
            end
          :__cdr ->
            [lst | []] = operands
            case infer(lst, type_env) do
              {:ok, {_s1, list_type}} ->
                case list_type do
                  %Types{constructor: :Tlist} ->
                    {:ok, {null_substitution(), list_type}}
                  %Types{constructor: :Tvar} ->
                    tv = TypeVars.fresh()
                    {:ok, sub} = unify(list_type, Types.list(tv))
                    {:ok, {sub, Types.list(tv)}}
                  _ ->
                    {:error, {:type, "Cannot unify #{list_type.str} with [a]"}}
                end
              {:error, e} ->
                {:error, e}
            end
          :__empty? ->
            [lst | []] = operands
            case infer(lst, type_env) do
              {:ok, {_s1, list_type}} ->
                case list_type do
                  %Types{constructor: :Tlist} ->
                    {:ok, {null_substitution(), Types.bool()}}
                  %Types{constructor: :Tvar} ->
                    t = Types.list(TypeVars.fresh())
                    {:ok, sub} = unify(list_type, t)
                    {:ok, {sub, Types.bool()}}
                  _ ->
                    {:error, {:type, "Cannot unify #{list_type.str} with [a]"}}
                end
              {:error, e} ->
                {:error, e}
            end
          :__cons ->
            [elem | [lst | []]] = operands
            tv = TypeVars.fresh()
            case infer(lst, type_env) do
              {:ok, {_s1, %Types{t: t} = list_type}} ->
                cons_type = Types.function(t, Types.function(list_type, list_type))
                infer_binary_op(type_env, cons_type, {elem, lst})
              {:error, e} ->
                {:error, e}
            end
          :__let ->
            [name | [bound | []]] = operands
            tv = TypeVars.fresh()
            {s1, t1} = {null_substitution(), tv}
            type_env = apply_sub(s1, type_env)
            t1_prime = generalize(type_env, t1)
            type_env = extend(type_env, {name.node, t1_prime})
            infer(bound, type_env)
          :__letrec ->
            # TODO not inferring the specific type
            [name | [bound | []]] = operands
            tv = TypeVars.fresh()
            {s1, t1} = {null_substitution(), tv}
            type_env = apply_sub(s1, type_env)
            t1_prime = generalize(type_env, t1)
            type_env = extend(type_env, {name.node, t1_prime})
            with {:ok, {s1, t}} <- infer(bound, type_env) do
              fn_type = apply_sub(s1, tv)
              s2 = reconcile(t, fn_type)
              {:ok, {s1, apply_sub(s2, t)}}
            else
            {:error, e} -> {:error, e}
            end
          :__if ->
            [test | [consequent | [alternative | []]]] = operands
            with {:ok, {s1, t1}} <- infer(test, type_env),
                 {:ok, {s2, t2}} <- infer(consequent, apply_sub(s1, type_env)),
                 {:ok, {s3, t3}} <- infer(alternative, apply_sub(s1, type_env)),
                 {:ok, s4} <- unify(t1, Types.bool()),
                 {:ok, s5} = unify(t2, t3) do
              composed_substitution = s5
              |> compose(s4)
              |> compose(s3)
              |> compose(s2)
              |> compose(s1)
              {:ok, {composed_substitution, apply_sub(s5, t2)}}
            else
              {:error, e} -> {:error, e}
            end
          :__lambda ->
            [%RoseTree{node: :__apply, children: args} | [body | []]] = operands

            # Generate a fresh type variable for each argument
            type_vars = args
            |> Enum.map(fn (_arg) -> TypeVars.fresh() end)

            # Extend the type environment with the arguments
            type_env = args
            |> Enum.zip(type_vars)
            |> Enum.reduce(
              type_env,
              fn ({arg, var}, acc) ->
                extend(acc, {arg.node, generalize(type_env, var)})
              end)

            # Infer the type of the function body
            fn_type = infer(body, type_env)
            case fn_type do
              {:ok, {s, t}} ->
                substituted_type_vars = type_vars
                |> Enum.map(&apply_sub(s, &1))
                arrows = build_up_arrows((substituted_type_vars ++ List.wrap(t)))
                {:ok, {s, arrows}}
              {:error, e} ->
                {:error, e}
            end
          :__apply ->
            # applying a lambda
            tv = TypeVars.fresh()
            with {:ok, {s1, t1}} <- infer(operator, type_env),
                 {:ok, {_type_env, {s2, ts}}} <- infer_operands(operands, apply_sub(s1, type_env)),
                 arrows = build_up_arrows(Enum.reverse([tv | ts])),
                 {:ok, s3} <- unify(apply_sub(s2, t1), arrows) do
              composed_substitution = compose(s3, compose(s2, s1))
              {:ok, {composed_substitution, apply_sub(s3, tv)}}
            else
              {:error, e} ->
                {:error, e}
            end
          :__provide ->
            # TODO filter our provide nodes
            {:ok, {null_substitution(), nil}}
          _ ->
            {s1, t1} = case lookup(type_env, operator.node) do
                         {:ok, {s, t}} ->
                           {s, t}
                         {:error, _} ->
                           {null_substitution(), TypeVars.fresh()}
                       end
            tv = TypeVars.fresh()
            with {:ok, {_type_env, {s2, ts}}} <- infer_operands(operands, apply_sub(s1, type_env)),
                 arrows = build_up_arrows(Enum.reverse([tv | ts])),
                 {:ok, s3} <- unify(apply_sub(s2, t1), arrows) do
              composed_substitution = compose(s3, compose(s2, s1))
              {:ok, {composed_substitution, apply_sub(s3, tv)}}
            else
              {:error, e} ->
                {:error, e}
            end
        end
      _ ->
        lookup(type_env, node)
    end
  end

  def type_for_constructor(name) do
    case Types.constructor_for_type(name) do
      {:error, _e} = error -> error
      {:ok, t} ->
        {:ok, value_constructor} = Types.value_constructor(name)
        vs = for _var <- value_constructor.vars, do: TypeVars.fresh()
        fresh_t = Types.replace_type_vars({t, Enum.map(vs, &(to_string(&1)))})
        {:ok, build_up_arrows(Enum.reverse([fresh_t | vs]))}
    end
  end

  def infer_operands(operands, type_env, result_type \\ []) do
    operands
    |> Enum.reduce({:ok, {type_env, {%{}, result_type}}},
      fn (_expr, {:error, _} = error)  -> error
        (expr, {:ok, {t_env, {s1, types}}}) ->
        case infer(expr, t_env) do
          {:ok, {s2, type}} ->
            subbed_env = apply_sub(s2, type_env)
            composed_sub = compose(s1, s2)
            {:ok, {subbed_env, {composed_sub, apply_sub(s2, [type | List.wrap(types)])}}}
          {:error, e} ->
            {:error, e}
        end
      end)
  end

  @spec infer_binary_op(type_environment, Types.t, {RoseTree.t, RoseTree.t}) :: {substitution, Types.t}
  def infer_binary_op(type_env, binary_type, {arg1, arg2}) do
    tv = TypeVars.fresh()
    with {:ok, {s1, t1}} <- infer(arg1, type_env),
         {:ok, {s2, t2}} <- infer(arg2, type_env),
         inferred_op_type <- build_up_arrows([t1, t2, tv]),
         {:ok, s3} <- unify(binary_type, inferred_op_type) do
      composed_substitution = compose(s1, compose(s2, s3))
      {:ok, {composed_substitution, apply_sub(s3, tv)}}
    else
      {:error, e} ->
        {:error, e}
    end
  end

  def infer_cond(type_env, expr) do
    # This is a bit kludgy.
    # Cond is ({Bool, a} -> a)
    tv = TypeVars.fresh()
    cond_type = Types.function(Types.tuple(Types.bool(), tv), tv)

    {s, test_t, res_t} = expr
    |> Enum.reduce({%{}, nil, nil},
    fn (_expr, {:error, _} = e) -> e
      (%RoseTree{node: [test | [res | []]]}, {subs, test_types, res_types}) ->
        {:ok, {s1, t1}} = infer(test, type_env)
        {:ok, {s2, t2}} = infer(res, type_env)
        test_type = if test_types == nil || t1 == test_types, do: t1, else: {:error, {:bad_type, t1}}
        res_type = if res_types == nil || t2 == res_types, do: t2, else: {:error, {:bad_type, t2}}
        {compose(s1, compose(s2, subs)), test_type, res_type}
    end)

    case test_t do
      {:error, {:bad_type, x}} ->
        {:error, {:type, {:unification, %{expected: Types.bool(), received: x}}}}
      %Types{t: :Bool} ->
        case res_t do
          {:error, {:bad_type, x}} ->
            {:error, {:type, {:unification, %{expected: Types.bool(), received: x}}}}
          %Types{} = t ->
            expr_type = Types.function(Types.tuple(Types.bool(), t), t)
            with {:ok, s3} <- unify(expr_type, cond_type) do
              composed_substitution = compose(s3, s)
              {:ok, {composed_substitution, apply_sub(s3, tv)}}
            end
        end
    end
  end

  @spec build_up_arrows([Types.t]) :: Types.t
  def build_up_arrows([type | []]), do: type
  def build_up_arrows([type1 | types]) do
    Types.function(type1, build_up_arrows(types))
  end

  def unify_list_types(types), do: unify_list_types(types, %{})
  def unify_list_types([], unification), do: {:ok, unification}
  def unify_list_types([{type_var, type} | types], unification) do
    case unify(type_var, type) do
      {:ok, unification2} ->
        unify_list_types(types, compose(unification, unification2))
      {:error, e} ->
        {:error, e}
    end
  end

  @spec extend(type_environment, {atom() | String.t, scheme}) :: type_environment
  def extend(%{} = type_env, {var, scheme}) do
    Map.put_new(type_env, var, scheme)
  end

  @spec restrict(type_environment, atom() | String.t) :: type_environment
  def restrict(%{} = type_env, var) do
    Map.drop(type_env, var)
  end

  @spec lookup(type_environment, atom() | String.t) ::
  {:ok, scheme} | {:error, {:unbound, atom() | String.t}}
  def lookup(type_environment, var) do
    case Map.get(type_environment, var) do
      nil ->
        case type_for_constructor(var) do
          {:error, _e} ->
            TypeEnvironment.lookup(var)
          {:ok, t} ->
            {:ok, {null_substitution(), t}}
        end
      x ->
        {:ok, {null_substitution(), instantiate(x)}}
    end
  end

  @doc """
  Instantiate a type
  """
  def instantiate({xs, t}) do
    fresh_type_vars = xs
    |> Enum.map(fn (_x) -> TypeVars.fresh() end)

    xs
    |> Enum.zip(fresh_type_vars)
    |> Map.new()
    |> apply_sub(t)
  end

  @doc """
  Generalize a bound type
  """
  @spec generalize(type_environment, Types.t) :: scheme
  def generalize(type_env, type) do
    xs = type
    |> ftv()
    |> MapSet.difference(ftv(type_env))
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
  def apply_sub(substitution, %Types{constructor: :Ttuple, t: {t1, t2}}) do
    Types.tuple(apply_sub(substitution, t1), apply_sub(substitution, t2))
  end
  def apply_sub(substitution, %Types{constructor: :Tarrow, t: {t1, t2}}) do
    Types.function(apply_sub(substitution, t1), apply_sub(substitution, t2))
  end
  def apply_sub(substitution, %Types{t: ts} = type) when is_list(ts) do
    updated_ts = Enum.map(ts, &apply_sub(substitution, &1))
    updated_vars = Enum.map(type.vars,
      fn var ->
        case Map.get(substitution, var) do
          nil -> var
          type -> to_string(type)
        end
      end)
    %{type | t: updated_ts, vars: updated_vars}
  end
  def apply_sub(substitution, {as, t} = _type_scheme) do
    substitution_prime = as
    |> Enum.reduce(substitution, fn (type_var, new_sub) ->
      Map.delete(new_sub, type_var) end
    )
    t_prime = apply_sub(substitution_prime, t)
    {as, t_prime}
  end
  def apply_sub(substitution, xs) when is_list(xs) do
    Enum.map(xs, &apply_sub(substitution, &1))
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
  def ftv(%Types{constructor: :Tlist, t: t}), do: ftv(t)
  def ftv(%Types{constructor: :Ttuple, t: {t1, t2}}) do
    MapSet.union(ftv(t1), ftv(t2))
  end
  def ftv(%Types{constructor: :Tvar} = type), do: MapSet.new([type])
  def ftv(%Types{constructor: :Tarrow, t: {t1, t2}}) do
    MapSet.union(ftv(t1), ftv(t2))
  end
  def ftv(%Types{t: ts}) when is_list(ts) do
    # ADTs; t consists of a list of constructors.
    ts
    |> Enum.map(&ftv/1)
    |> Enum.reduce(MapSet.new(), fn (t, acc) -> MapSet.union(t, acc) end)
  end
  def ftv({as, type}) do
    MapSet.difference(ftv(type), MapSet.new(as))
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
    {:ok, null_substitution()}
  end
  def unify(%Types{constructor: :Tlist, t: t1}, %Types{constructor: :Tlist, t: t2}) do
    unify(t1, t2)
  end
  def unify(%Types{constructor: :Ttuple, t: {l1, r1}}, %Types{constructor: :Ttuple, t: {l2, r2}}) do
    with {:ok, s1} <- unify(l1, l2),
         {:ok, s2} <- unify(apply_sub(s1, r1), apply_sub(s1, r2)) do
      {:ok, compose(s2, s1)}
    else
      {:error, e} ->
        {:error, e}
    end
  end
  def unify(%Types{constructor: :Tarrow, t: {l1, r1}}, %Types{constructor: :Tarrow, t: {l2, r2}}) do
    with {:ok, s1} <- unify(l1, l2),
         {:ok, s2} <- unify(apply_sub(s1, r1), apply_sub(s1, r2)) do
      {:ok, compose(s2, s1)}
    else
      {:error, e} ->
        {:error, e}
    end
  end
  def unify(t1, t2), do: {:error, {:type, {:unification, %{expected: t1, received: t2}}}}

  @spec bind(Types.t, Types.t) :: {:ok, substitution} | {:error, {:type, String.t}}
  def bind(a, type) do
    cond do
      a == type.t -> {:ok, null_substitution()}
      occurs_check(a, type) -> {:error, {:type, "Unable to construct infinite type"}}
      true -> {:ok, %{a => type}}
    end
  end

  def occurs_check(a, type) do
    MapSet.member?(ftv(type), a)
  end

  @doc """
  Reconcile arrows with the inferred type of a function.

  This is kind of a hack to unify/reconcile higher order functions.
  The returned type of the higher order function wasn't correctly
  unifying with the overall types inferred for the function.
  """
  def reconcile(t1, t2, substitution \\ %{})
  def reconcile(%Types{t: {l1, r1}}, %Types{t: {l2, r2}}, substitution) do
    s2 = case unify(r2, r1) do
           {:ok, s} ->
             s
           {:error, _e} ->
             %{}
           s ->
             s
         end
    reconcile(l1, l2, Map.merge(substitution, s2, fn _k, v1, _v2 -> v1 end))
  end
  def reconcile(_, _, substitution), do: substitution

  @doc """
  Goes through and replaces type variables with fresh ones (after
  resetting TypeVars state).

  ## Examples

      iex> {[%Types{constructor: :Tvar, t: :b], %Types{constructor: :Tvar, t: :b}
      ...> |> substitute_type_vars()
      {[%Types{constructor: :Tvar, t: :a}], %Types{constructor: :Tvar, t: :a}}
  """
  def substitute_type_vars({vars, type}) do
    TypeVars.reset()
    fresh_vars = for _var <- vars, do: TypeVars.fresh()
    zipped = Enum.zip(vars, fresh_vars)
    updated_type = Enum.reduce(zipped, type, fn (var, type) -> substitute_type_var(type, var) end)
    {fresh_vars, updated_type}
  end
  defp substitute_type_var(%Types{constructor: :Tvar, t: t} = type, {old_var, new_var}) do
    if t == old_var.t, do: new_var, else: type
  end
  defp substitute_type_var(%Types{constructor: :Tlist, t: t} = type, vars) do
    subbed_t = substitute_type_var(t, vars)
    %{type | t: subbed_t}
  end
  defp substitute_type_var(%Types{constructor: :Ttuple, t: {t1, t2}} = type, vars) do
    subbed_t1 = substitute_type_var(t1, vars)
    subbed_t2 = substitute_type_var(t2, vars)
    %{type | t: {subbed_t1, subbed_t2}}
  end
  defp substitute_type_var(%Types{constructor: :Tarrow, t: {t1, t2}} = type, vars) do
    subbed_t1 = substitute_type_var(t1, vars)
    subbed_t2 = substitute_type_var(t2, vars)
    %{type | t: {subbed_t1, subbed_t2}}
  end
  defp substitute_type_var(%Types{t: ts} = type, vars) when is_list(ts) do
    Types.replace_type_var(type, {to_string(elem(vars, 0)), to_string(elem(vars, 1))})
  end
  defp substitute_type_var(type, _vars), do: type
end

defmodule Terp.TypeSystem.Evaluator do
  @moduledoc """
  The primary type inference module.
  """
  alias Terp.Error
  alias Terp.TypeSystem.Type
  alias Terp.TypeSystem.Annotation
  alias Terp.TypeSystem.Environment
  alias Terp.TypeSystem.Match
  alias Terp.TypeSystem.TypeClass
  alias Terp.TypeSystem.TypeVars

  @type scheme :: {[Type.t], Type.t}
  @type type_environment :: map()
  @type substitution :: %{required(Type.t) => Type.t}
  @type errors :: [String.t]

  @spec run_infer(RoseTree.t) :: Type.t
  def run_infer(%RoseTree{} = expr) do
    case TypeVars.start_link() do
      {:ok, _} -> :ok
      {:error, _} -> TypeVars.reset()
    end
    case infer(expr, %{}) do
      :ok ->
        {:ok, %{}}
      {:ok, {_substitution, type}} ->
        if expr.node == :__type do
          {:ok, {null_substitution(), type}}
        else
          case Annotation.reconcile_annotation(expr, type) do
            {:ok, annotated_type_scheme} ->
              {:ok, substitute_type_vars(annotated_type_scheme)}
            {:error, {:type, {:annotation, _map}}} = error ->
              error
            %Error{} = error ->
              error
            _ ->
              type_scheme = generalize(%{}, type)
              Environment.extend_environment(expr, type)
              {:ok, substitute_type_vars(type_scheme)}
          end
        end
      {:error, _e} = error ->
        error
      %Error{} = error ->
        %{error | in_expression: expr}
    end
  end

  defp format_constructor(defn) do
    [constructor | vars] = defn
    |> Enum.map(&(&1.node))
    {constructor, vars}
  end

  @spec infer(RoseTree.t, type_environment) :: {substitution, Type.t}
  def infer(%RoseTree{node: node, children: children} = expr, type_env) do
    case node do
      :__data ->
        [type_constructor | [data_constructors | []]] = children
        {name, vars} = format_constructor(type_constructor.node)
        data_constructors = Enum.map(data_constructors.node, &format_constructor/1)
        t = Type.higher_kinded(name, vars, data_constructors)
        Environment.define_type(name, t)
        {:ok, {%{}, t}}
      :__type ->
        [fn_name | [type_info | []]] = children
        Annotation.annotate_type(fn_name, type_info.node)
      :__class ->
        [type_class, %{node: types}] = children
        class_name = type_class.node
        |> Enum.map(&(&1.node))
        TypeClass.define_class(class_name, types)
      :__instance ->
        [type_class, %{node: definitions}] = children
        TypeClass.define_instance(type_class, definitions)
      x when is_integer(x) ->
        {:ok, {null_substitution(), Type.int()}}
      x when is_float(x) ->
        {:ok, {null_substitution(), Type.float()}}
      x when is_boolean(x) ->
        {:ok, {null_substitution(), Type.bool()}}
      :__string ->
        {:ok, {null_substitution(), Type.string()}}
      :"__#t" -> # Seems spurious, but probably don't need the when boolean?
        {:ok, {null_substitution(), Type.bool()}}
      :"__#f" ->
        {:ok, {null_substitution(), Type.bool()}}
      :__quote ->
        {type_env, sub, types} = children
        |> Enum.reduce({type_env, %{}, []},
          fn (_expr, {:error, e}) -> {:error, e}
            (expr, {type_env, sub1, types}) ->
              case infer(expr, type_env) do
                {:ok, {sub2, type}} ->
                  {type_env, compose(sub1, sub2), [type | types]}
                error ->
                  error
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
                {:ok, {sub, Type.list(tv)}}
              [t | _ts] ->
                {:ok, {sub, Type.list(t)}}
            end
          [t | []] ->
            case unique_types[true] do
              nil ->
                {:ok, {sub, Type.list(t)}}
              vars ->
                case unify_list_types(Enum.map(vars, &{&1, t})) do
                  {:ok, sub} ->
                    {:ok, {sub, Type.list(t)}}
                  error ->
                    error
                end
            end
          ts ->
            type_strings = ts
            |> Enum.map(&(to_string(&1)))
            |> Enum.join(", ")
            {:error, {:type, "Unable to unify list types: #{type_strings}"}}
        end
      :__empty? ->
        tv = TypeVars.fresh()
        t = build_up_arrows([Type.list(tv), Type.bool()])
        {:ok, {null_substitution(), t}}
      :__cons ->
        tv = TypeVars.fresh()
        t = build_up_arrows([tv, Type.list(tv), Type.list(tv)])
        {:ok, {null_substitution(), t}}
      :__car ->
        tv = TypeVars.fresh()
        t = build_up_arrows([Type.list(tv), tv])
        {:ok, {null_substitution(), t}}
      :__cdr ->
        tv = TypeVars.fresh()
        t = build_up_arrows([Type.list(tv), Type.list(tv)])
        {:ok, {null_substitution(), t}}
      :__cond ->
        infer_cond(type_env, children)
      :__match ->
        case infer_match(type_env, children) do
          %Error{} = error ->
            %{error | in_expression: expr}
          res ->
            res
        end
      :__let_values ->
        [%RoseTree{node: bindings} | [expr | []]] = children
        {type_env, _subs} = Enum.reduce(bindings, {type_env, null_substitution()},
          fn ([var | [val | []]], {type_env, subs}) ->
            {:ok, {s, t}} = infer(val, type_env)
            t_prime = generalize(type_env, t)
            type_env = extend(type_env, {var.node, t_prime})
            {type_env, compose(s, subs)}
          end
        )
        infer(expr, type_env)
      :__apply ->
        [operator | operands] = children
        case operator.node do
          :"__+" ->
            t = Type.function(Type.int(), Type.function(Type.int(), Type.int()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :"__-" ->
            t = Type.function(Type.int(), Type.function(Type.int(), Type.int()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :"__*" ->
            t = Type.function(Type.int(), Type.function(Type.int(), Type.int()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__div ->
            t = Type.function(Type.int(), Type.function(Type.int(), Type.int()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__equal? ->
            # Using a single type variable because equality between different types would be ill-typed
            tv = TypeVars.fresh()
            t = Type.function(tv, Type.function(tv, Type.bool()))
            infer_binary_op(type_env, t, {Enum.at(operands, 0), Enum.at(operands, 1)})
          :__car ->
            [lst | []] = operands
            with {:ok, {s1, car_type}} <- infer(operator, type_env),
                 {:ok, {s2, list_type}} <- infer(lst, type_env),
                   %Type{constructor: :Tlist, t: t} = list_type do
              {:ok, s3} = unify(car_type, build_up_arrows([list_type, t]))
              {:ok, {compose(s3, compose(s2, s1)), t}}
            else
              error ->
                error
            end
          :__cdr ->
            [lst | []] = operands
            with {:ok, {s1, cdr_type}} <- infer(operator, type_env),
                 {:ok, {s2, list_type}} <- infer(lst, type_env) do
              {:ok, s3} = unify(cdr_type, build_up_arrows([list_type, list_type]))
              {:ok, {compose(s3, compose(s2, s1)), list_type}}
            else
              error ->
                error
            end
          :__empty? ->
            [lst | []] = operands
            with {:ok, {s1, empty_type}} <- infer(operator, type_env),
                 {:ok, {s2, list_type}} <- infer(lst, type_env) do
              {:ok, s3} = unify(empty_type, build_up_arrows([list_type, Type.bool()]))
              {:ok, {compose(s3, compose(s2, s1)), Type.bool()}}
            else
              error ->
                error
            end
          :__cons ->
            [elem | [lst | []]] = operands
            tv = TypeVars.fresh()
            {:ok, {s1, cons_type}} = infer(operator, type_env)
            infer_binary_op(type_env, cons_type, {elem, lst})
          :__let ->
            [name | [bound | []]] = operands
            tv = TypeVars.fresh()
            {s1, t1} = {null_substitution(), tv}
            type_env = apply_sub(s1, type_env)
            t1_prime = generalize(type_env, t1)
            type_env = extend(type_env, {name.node, t1_prime})
            infer(bound, type_env)
          :__letrec ->
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
                 {:ok, s4} <- unify(t1, Type.bool()),
                 {:ok, s5} = unify(t2, t3) do
              composed_substitution = s5
              |> compose(s4)
              |> compose(s3)
              |> compose(s2)
              |> compose(s1)
              {:ok, {composed_substitution, apply_sub(s5, t2)}}
            else
              error -> error
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
              %Error{} = error ->
                error
            end
          :__apply ->
            # applying a lambda
            # why does this not work with infer_application/3?
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
              %Error{} = error ->
                error
            end
          :__provide ->
            # TODO filter our provide nodes
            {:ok, {null_substitution(), TypeVars.fresh()}}
          :__beam ->
            fn_name = operator.children
            |> Enum.map(&(&1.node))
            |> Enum.join()

            infer_application(fn_name, operands, type_env)
          _ ->
            infer_application(operator.node, operands, type_env)
        end
      _ ->
        lookup(type_env, node)
    end
  end

  @doc """
  Infers function application.
  """
  def infer_application(operator, operands, type_env) do
    tv = TypeVars.fresh()
    with {:ok, {s1, t1}} <- lookup(type_env, operator),
         {:ok, {_type_env, {s2, ts}}} <- infer_operands(operands, apply_sub(s1, type_env)),
           arrows = build_up_arrows(Enum.reverse([tv | ts])),
         {:ok, s3} <- unify(apply_sub(s2, t1), arrows) do
      composed_substitution = compose(s3, compose(s2, s1))
      # TODO Somewhere here, need to check if the inferred type
      # is a member of the type class; throw error otherwise
      {:ok, {composed_substitution, apply_sub(s3, tv)}}
    else
      {:error, e} ->
        {:error, e}
      %Error{} = error ->
        Error.evaluating_lens
        |> Focus.over(error, fn x -> Map.put(x, :expr, operands) end)
    end
  end

  def type_for_constructor(name) do
    case Type.constructor_for_type(name) do
      {:error, _e} = error -> error
      %Error{} = error -> error
      {:ok, t} ->
        {:ok, value_constructor} = Type.value_constructor(name)
        {:ok, build_up_arrows(Enum.reverse([t | value_constructor.t]))}
    end
  end

  @spec infer_operands([RoseTree.t], type_environment, [RoseTree.t]) :: {:ok, {type_environment, [Type.t]}} | {:error, any()}
  def infer_operands(operands, type_env, result_type \\ []) do
    operands
    |> Enum.reduce({:ok, {type_env, {%{}, result_type}}},
      fn (_expr, {:error, _} = error)  -> error
        (_expr, %Error{} = error)  -> error
        (expr, {:ok, {t_env, {s1, types}}}) ->
        case infer(expr, t_env) do
          {:ok, {s2, type}} ->
            subbed_env = apply_sub(s2, type_env)
            composed_sub = compose(s1, s2)
            {:ok, {subbed_env, {composed_sub, apply_sub(s2, [type | List.wrap(types)])}}}
          {:error, e} ->
            {:error, e}
          %Error{} = error ->
            error
        end
      end)
  end

  @spec infer_binary_op(type_environment, Type.t, {RoseTree.t, RoseTree.t}) :: {substitution, Type.t}
  def infer_binary_op(type_env, binary_type, {arg1, arg2}) do
    tv = TypeVars.fresh()
    with {:ok, {s1, t1}} <- infer(arg1, type_env),
         {:ok, {s2, t2}} <- infer(arg2, type_env),
         inferred_op_type <- build_up_arrows([t1, t2, tv]),
         {:ok, s3} <- unify(inferred_op_type, binary_type) do
      composed_substitution = compose(s1, compose(s2, s3))
      {:ok, {composed_substitution, apply_sub(s3, tv)}}
    else
      error ->
        error
    end
  end

  def infer_cond(type_env, expr) do
    # This is a bit kludgy.
    # Cond is ({Bool, a} -> a)
    tv = TypeVars.fresh()
    cond_type = Type.function(Type.tuple(Type.bool(), tv), tv)

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
        {:error, {:type, {:unification, %{expected: Type.bool(), received: x}}}}
      %Type{t: :Bool} ->
        case res_t do
          {:error, {:bad_type, x}} ->
            {:error, {:type, {:unification, %{expected: Type.bool(), received: x}}}}
          %Type{} = t ->
            expr_type = Type.function(Type.tuple(Type.bool(), t), t)
            with {:ok, s3} <- unify(expr_type, cond_type) do
              composed_substitution = compose(s3, s)
              {:ok, {composed_substitution, apply_sub(s3, tv)}}
            end
        end
    end
  end

  def infer_match(type_env, expr) do
    [match_var | match_exprs] = expr
    exhaustive = Match.exhaustive_matches?(match_exprs)
    case exhaustive do
      :ok ->
        Match.match_type(match_var, match_exprs, type_env)
      error ->
        error
    end
  end

  @spec build_up_arrows([Type.t]) :: Type.t
  def build_up_arrows([type | []]), do: type
  def build_up_arrows([type1 | types]) do
    Type.function(type1, build_up_arrows(types))
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
            Environment.lookup(var)
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
  @spec generalize(type_environment, Type.t) :: scheme
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
    sub2
    |> Map.merge(sub1, fn _k, v1, _v2 -> v1 end)
    |> Enum.map(fn {t_var, t_scheme} -> {t_var, apply_sub(sub1, t_scheme)} end)
    |> Map.new()
  end

  @spec apply_sub(substitution, Type.t) :: Type.t
  def apply_sub(_, %Type{constructor: :Tconst} = type), do: type
  def apply_sub(substitution, %Type{constructor: :Tlist, t: %Type{constructor: :Tlist, t: t}}) do
    Type.list(apply_sub(substitution, t))
  end
  def apply_sub(substitution, %Type{constructor: :Tlist, t: t}) do
    Type.list(apply_sub(substitution, t))
  end
  def apply_sub(substitution, %Type{constructor: :Tvar, t: t} = type) do
    Map.get(substitution, t, type)
  end
  def apply_sub(substitution, %Type{constructor: :Ttuple, t: {t1, t2}}) do
    Type.tuple(apply_sub(substitution, t1), apply_sub(substitution, t2))
  end
  def apply_sub(substitution, %Type{constructor: :Tarrow, t: {t1, t2}} = type) do
    classes = if is_nil(type.classes) do
      nil
    else
      Enum.filter(type.classes, fn {_c, v} -> v == t1.t end)
    end
    t1 = %{t1 | classes: classes}
    t2 = %{t2 | classes: type.classes}
    t = Type.function(apply_sub(substitution, t1), apply_sub(substitution, t2))
    %{t | classes: type.classes}
  end
  def apply_sub(substitution, %Type{classes: classes} = type) when is_list(classes) do
    # TODO Sub in the constructor if it's a variable
    class_vars = Enum.map(classes, &elem(&1, 1))
    Enum.reduce(class_vars, type, fn (var, _acc) ->
      case Map.get(substitution, var) do
        nil -> type
        sub_type -> sub_type
      end
    end)
  end
  def apply_sub(substitution, %Type{t: ts} = type) when is_list(ts) do
    # TODO Sub in the constructor if it's a variable
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
  @spec ftv(Type.t | scheme | [Type.t] | type_environment) :: MapSet.t
  def ftv(%Type{constructor: :Tconst}), do: MapSet.new()
  def ftv(%Type{constructor: :Tlist, t: t}), do: ftv(t)
  def ftv(%Type{constructor: :Ttuple, t: {t1, t2}}) do
    MapSet.union(ftv(t1), ftv(t2))
  end
  def ftv(%Type{constructor: :Tvar} = type), do: MapSet.new([type])
  def ftv(%Type{constructor: :Tarrow, t: {t1, t2}}) do
    MapSet.union(ftv(t1), ftv(t2))
  end
  def ftv(%Type{t: ts}) when is_list(ts) do
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
  @spec unify(Type.t, Type.t) :: {substitution, Type.t}
  def unify(%Type{constructor: :Tvar, t: a}, type), do: bind(a, type)
  def unify(type, %Type{constructor: :Tvar, t: a}), do: bind(a, type)
  def unify(%Type{constructor: :Tconst, t: a}, %Type{constructor: :Tconst, t: a}) do
    {:ok, null_substitution()}
  end
  def unify(%Type{constructor: :Tlist, t: t1}, %Type{constructor: :Tlist, t: t2}) do
    unify(t1, t2)
  end
  def unify(%Type{constructor: :Ttuple, t: {l1, r1}}, %Type{constructor: :Ttuple, t: {l2, r2}}) do
    with {:ok, s1} <- unify(l1, l2),
         {:ok, s2} <- unify(apply_sub(s1, r1), apply_sub(s1, r2)) do
      {:ok, compose(s2, s1)}
    else
      {:error, e} ->
        {:error, e}
    end
  end
  def unify(%Type{constructor: :Tarrow, t: {l1, r1}} = t1, %Type{constructor: :Tarrow, t: {l2, r2}} = t2) do
    # TODO check that types implement class or throw error
    with :ok <- TypeClass.implements_class?(t1.classes, t2),
         {:ok, s1} <- unify(l1, l2),
         {:ok, s2} <- unify(apply_sub(s1, r1), apply_sub(s1, r2)) do
      {:ok, compose(s2, s1)}
    else
      error ->
        error
    end
  end
  def unify(%Type{type_constructor: c, vars: v1}, %Type{type_constructor: c, vars: v2})
    when is_list(v1) and is_list(v2) do
    v1_types = Enum.map(v1, &Type.to_type/1)
    v2_types = Enum.map(v2, &Type.to_type/1)
    Enum.reduce(Enum.zip(v1_types, v2_types), {:ok, %{}},
      fn (_, {:error, _e} = error) -> error
        ({t1, t2}, {:ok, subs}) ->
          case unify(t1, t2) do
            {:ok, s} ->
              {:ok, compose(s, subs)}
            e -> e
          end
      end)
  end
  def unify(%Type{type_constructor: c, vars: v1}, %Type{type_constructor: nil, t: t} = t2) when is_list(v1) do
    # Unification for higher kinded type vars
    with c_type <- Type.to_type(c),
         v1_types <- Enum.map(v1, &Type.to_type/1),
           v1_type <- List.first(v1_types) do
      Enum.reduce([{t2, c_type}, {v1_type, t}], {:ok, %{}},
        fn (_, {:error, _e} = error) -> error
          ({t1, t2}, {:ok, subs}) ->
            case unify(apply_sub(subs, t1), t2) do
              {:ok, s} ->
                {:ok, compose(s, subs)}
              e -> e
            end
        end)
    end
  end
  def unify(%Type{type_constructor: c, vars: v1} = t1, %Type{type_constructor: d, vars: v2} = t2) do
    # Unification for higher kinded types and higher kinded type variables
    # Starting to get a good bit of duplication in these unifiers. Should be refactored soon.
    with false <- is_nil(c),
         false <- is_nil(d),
         c_type <- Type.to_type(c),
           v1_types <- Enum.map(v1, &Type.to_type/1),
           v2_types <- Enum.map(v2, &Type.to_type/1) do
      Enum.reduce(Enum.zip([t2 | v2_types], [c_type | v1_types]), {:ok, %{}},
        fn (_, {:error, _e} = error) -> error
          ({t1, t2}, {:ok, subs}) ->
            case unify(t1, t2) do
              {:ok, s} ->
                {:ok, compose(s, subs)}
              e -> e
            end
        end)
    else
      _ ->
        %Error{kind: :type,
               type: :unification,
               message: "Unable to unify types",
               evaluating: %{expected: t1, actual: t2}}
    end
  end
  def unify(%Type{t: [var]}, %Type{constructor: _concrete} = t) do
    if class_var?(var), do: bind(var, t)
  end
  def unify(%Type{constructor: _concrete} = t, %Type{t: [var]}) do
    if class_var?(var), do: bind(var, t)
  end
  def unify(t1, t2) do
    %Error{kind: :type,
           type: :unification,
           message: "Unable to unify types",
           evaluating: %{expected: t1, actual: t2}}
  end

  defp class_var?(var) do
    is_bitstring(var) && (String.downcase(var) == var)
  end

  @spec bind(Type.t, Type.t) :: {:ok, substitution} | {:error, {:type, String.t}}
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
  def reconcile(%Type{t: {l1, r1}}, %Type{t: {l2, r2}}, substitution) do
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

      iex> {[%Type{constructor: :Tvar, t: :b], %Type{constructor: :Tvar, t: :b}
      ...> |> substitute_type_vars()
      {[%Type{constructor: :Tvar, t: :a}], %Type{constructor: :Tvar, t: :a}}
  """
  def substitute_type_vars({vars, %Type{classes: cs} = type}) when is_list(cs) do
    {Enum.uniq_by(vars, &(&1.t)), type}
  end
  def substitute_type_vars({vars, type}) do
    TypeVars.reset()
    fresh_vars = for _var <- vars, do: TypeVars.finalize()
    zipped = Enum.zip(vars, fresh_vars)
    updated_type = Enum.reduce(zipped, type, fn (var, type) -> substitute_type_var(type, var) end)
    {fresh_vars, updated_type}
  end
  defp substitute_type_var(%Type{constructor: :Tvar, t: t} = type, {old_var, new_var}) do
    if t == old_var.t, do: new_var, else: type
  end
  defp substitute_type_var(%Type{constructor: :Tlist, t: t} = type, vars) do
    subbed_t = substitute_type_var(t, vars)
    %{type | t: subbed_t}
  end
  defp substitute_type_var(%Type{constructor: :Ttuple, t: {t1, t2}} = type, vars) do
    subbed_t1 = substitute_type_var(t1, vars)
    subbed_t2 = substitute_type_var(t2, vars)
    %{type | t: {subbed_t1, subbed_t2}}
  end
  defp substitute_type_var(%Type{constructor: :Tarrow, t: {t1, t2}} = type, vars) do
    subbed_t1 = substitute_type_var(t1, vars)
    subbed_t2 = substitute_type_var(t2, vars)
    %{type | t: {subbed_t1, subbed_t2}}
  end
  defp substitute_type_var(%Type{t: ts} = type, vars) when is_list(ts) do
    Type.replace_type_var(type, {to_string(elem(vars, 0)), to_string(elem(vars, 1))})
  end
  defp substitute_type_var(type, _vars), do: type
end

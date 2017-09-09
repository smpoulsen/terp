defmodule Terp.Types.Match do
  alias Terp.Types.Types
  alias Terp.Types.TypeEvaluator

  @doc """
  Checks to see if all constructors for a type are present in the match.
  If the match isn't exhaustive, an error is returned.
  """
  def exhaustive_matches?(expr) do
    match_constructors = expr
    |> Stream.map(&(&1.node))
    |> Stream.map(&Enum.at(&1, 0))
    |> Stream.map(&(&1.children))
    |> Stream.map(&Enum.at(&1, 0))
    |> Enum.map(&(&1.node))

    case Types.constructor_for_type(Enum.at(match_constructors, 0)) do
      {:error, _e} ->
        {:error, {:type, :non_exhaustive_match}}
      {:ok, t} ->
        type_constructors = Enum.map(t.t, &(&1.constructor))
        # TODO This is not super robust
        same_set = MapSet.equal?(MapSet.new(type_constructors), MapSet.new(match_constructors))
        case same_set do
          true ->
            :ok
          false ->
            {:error, {:type, :non_exhaustive_match}}
        end
    end
  end

  @doc """
  Infer the type of a pattern match expression.
  """
  def match_type(vars, expr, type_env) do
    res_exprs = expr
    |> Stream.map(&(&1.node))
    |> Enum.map(&Enum.at(&1, 1))

    # Infer the type of the variable to match against
    {s, t} = infer_expr_list(vars.node, type_env)
    # Infer the type of the match expression
    {s1, t1} = infer_expr_list(res_exprs, type_env)
    # Unify the variable's type and the match expression's type
    {:ok, s2} = TypeEvaluator.unify(t, t1)
    {:ok, {TypeEvaluator.compose(s, TypeEvaluator.compose(s1, s2)), t1}}
  end

  defp infer_expr_list(exprs, type_env) do
    exprs
    |> Enum.reduce({%{}, nil},
    fn (_expr, {:error, _} = e) -> e
      (expr, {subs, res_types}) ->
        {:ok, {s1, t1}} = case TypeEvaluator.infer(expr, type_env) do
                            {:error, _} = error ->
                              error
                            x -> x
                          end
        subbed_t1 = TypeEvaluator.apply_sub(subs, t1)
        res_type = if res_types == nil || subbed_t1 == res_types, do: subbed_t1, else: {:error, {:bad_type, t1}}
        {TypeEvaluator.compose(s1, subs), res_type}
    end)
  end
end

defmodule Terp.Types.Annotation do
  alias Terp.Types.TypeEnvironment
  alias Terp.Types.TypeEvaluator
  alias Terp.Types.Types

  def annotate_type(name, type_trees) do
    vs = type_trees
    |> Enum.map(fn t ->
      if is_list(t), do: Enum.map(t, &(&1.node)), else: t.node
    end)
    t = apply(Types, :to_type, vs)
    if annotated?(name) do
      reconcile_annotation(name, t)
    else
      TypeEnvironment.annotate(name, t)
      {:ok, {%{}, t}}
    end
  end

  @spec annotated?(RoseTree.t) :: boolean()
  def annotated?(name) do
    annotation = name
    |> TypeEnvironment.lookup_annotation()
    case annotation do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @spec reconcile_annotation(String.t | RoseTree.t, Types.t) :: {:ok, TypeEvaluator.scheme} | {:error, any()}
  def reconcile_annotation(fn_name, type) when is_bitstring(fn_name) do
    with {:ok, annotated_type} <- TypeEnvironment.lookup_annotation(fn_name),
         {:ok, _sub} <- TypeEvaluator.unify(annotated_type, type) do
      # If annotated, check to see if the annotation matches the inferred type
      {:ok, TypeEvaluator.generalize(%{}, annotated_type)}
    else
      {:error, {:type, {:unification, %{expected: expected, received: actual}}}} ->
        {:error, {:type, {:annotation, %{expected: expected, actual: actual}}}}
      {:error, _e} = error ->
        error
    end
  end
  def reconcile_annotation(expr, type) do
    case Terp.fn_name(expr) do
      {:ok, fn_name} ->
        reconcile_annotation(fn_name, type)
      {:error, _e} = error ->
        error
    end
  end
end

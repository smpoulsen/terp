defmodule Terp.Types.Annotation do
  alias Terp.Error
  alias Terp.Types.TypeEnvironment
  alias Terp.Types.TypeEvaluator
  alias Terp.Types.Types

  def annotate_type(%RoseTree{node: :__beam, children: children}, type_trees) do
    children
    |> Enum.map(&(&1.node))
    |> Enum.join()
    |> RoseTree.new()
    |> annotate_type(type_trees)
  end
  def annotate_type(%RoseTree{node: name}, type_trees) do
    vs = type_trees
    |> Enum.map(&extract_type_nodes/1)
    t = apply(Types, :to_type, vs)
    if annotated?(name) do
      reconcile_annotation(name, t)
    else
      TypeEnvironment.annotate(name, t)
      {:ok, {%{}, t}}
    end
  end

  # Helper to pull out the type annotations from arbitrarily nested
  # type defs.
  defp extract_type_nodes(%RoseTree{node: node, children: []}), do: node
  defp extract_type_nodes(type_trees) when is_list(type_trees) do
    for tree <- type_trees do
      extract_type_nodes(tree)
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
  def reconcile_annotation(%{node: :__beam, children: children}, type) do
    children
    |> Enum.map(&(&1.node))
    |> Enum.join()
    |> reconcile_annotation(type)
  end
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
      %Error{} = error ->
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

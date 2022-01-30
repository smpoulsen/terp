defmodule Terp.TypeSystem.Annotation do
  alias Terp.AST
  alias Terp.Error
  alias Terp.TypeSystem.Environment
  alias Terp.TypeSystem.Evaluator
  alias Terp.TypeSystem.Type

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
    t = apply(Type, :to_type, vs)
    if annotated?(name) do
      reconcile_annotation(name, t)
    else
      Environment.annotate(name, t)
      {:ok, {%{}, t}}
    end
  end

  # Helper to pull out the type annotations from arbitrarily nested
  # type defs.
  def extract_type_nodes(%RoseTree{node: node, children: []}), do: node
  def extract_type_nodes(type_trees) when is_list(type_trees) do
    for tree <- type_trees do
      extract_type_nodes(tree)
    end
  end
  def extract_type_nodes(type), do: type

  @spec annotated?(RoseTree.t) :: boolean()
  def annotated?(name) do
    annotation = name
    |> Environment.lookup_annotation()
    case annotation do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @spec reconcile_annotation(String.t | RoseTree.t, Type.t) :: {:ok, Evaluator.scheme} | {:error, any()}
  def reconcile_annotation(%{node: :__beam, children: children}, type) do
    children
    |> Enum.map(&(&1.node))
    |> Enum.join()
    |> reconcile_annotation(type)
  end
  def reconcile_annotation(fn_name, type) when is_bitstring(fn_name) do
    with {:ok, annotated_type} <- Environment.lookup_annotation(fn_name),
         {:ok, _sub} <- Evaluator.unify(annotated_type, type) do
      # If annotated, check to see if the annotation matches the inferred type
      {:ok, Evaluator.generalize(%{}, annotated_type)}
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
    case AST.fn_name(expr) do
      {:ok, fn_name} ->
        reconcile_annotation(fn_name, type)
      {:error, _e} = error ->
        error
    end
  end
end

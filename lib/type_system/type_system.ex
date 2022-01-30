defmodule Terp.TypeSystem do
  @moduledoc """
  The context entry point/interface for terp's type system.
  """
  alias Terp.AST
  alias Terp.Error
  alias Terp.TypeSystem.Type
  alias Terp.TypeSystem.Evaluator
  alias Terp.TypeSystem.Environment

  @doc """
  Run the type evaluator for a given piece of source code.

  Converts the source to an AST and then type checks it.
  """
  @spec check_src(String.t) :: [Type.t]
  def check_src(src) do
    src
    |> AST.from_src()
    |> check_ast()
  end

  @doc """
  Runs the type evaluator for an AST.
  """
  @spec check_ast([RoseTree.t]) :: {:ok, [Type.t]} | {:error, any} | Error.t
  def check_ast(ast) when is_list(ast) do
    start_environment()

    res = ast
    |> Enum.reduce({:ok, []}, &check_tree/2)

    case res do
      {:ok, types} ->
        {:ok, Enum.reverse(types)}
      error ->
        error
    end
  end
  def check_ast(ast) do
    start_environment()
    case check_tree(ast, {:ok, []}) do
      {:ok, [type]} ->
        {:ok, type}
      error ->
        error
    end
  end

  # Checks an individual tree. If it type checks :ok, adds
  # the type to the running list of types for the AST.
  # Otherwise bails with the error.
  defp check_tree(tree, {:ok, types}) do
    case Evaluator.run_infer(tree) do
      {:ok, type} ->
        {:ok, [type | types]}
      error ->
        error
    end
  end
  defp check_tree(_tree, error), do: error

  @doc """
  Starts the GenServer for the type environment if it is not already initialized.
  """
  def start_environment(), do: Environment.start_if_unstarted()

  def stringify_type_scheme({type_vars, type}) do
    if Enum.empty?(type_vars) do
      to_string(type)
    else
      variables = Enum.map(type_vars, &(to_string(&1)))
      type
      |> reconcile_classes_and_vars(variables)
      |> finalize_scheme_string(type)
    end
  end

  def finalize_scheme_string({vars, classes}, type) do
    if classes == "" do
      "∀ #{vars} => #{to_string(type)}"
    else
      if vars == "" do
        "(#{classes}) => #{to_string(type)}"
      else
        "∀ #{vars}; (#{classes}) => #{to_string(type)}"
      end
    end
  end

  def reconcile_classes_and_vars(%Type{classes: nil}, variables) do
    {Enum.join(variables, " "), ""}
  end
  def reconcile_classes_and_vars(%Type{classes: classes}, variables) do
    {vars, classes} = classes
    |> Enum.reduce({variables, []}, fn ({c, v}, {vs, cs}) ->
      class_string = "#{c} #{v}"
      if Enum.member?(vs, v) do
        new_vs = List.delete(vs, v)
        {new_vs, [class_string | cs]}
      else
        # This should probably be an error?
        # Class variable shouldn't be possible
        # without a general variable
        {variables, [class_string | cs]}
      end
    end)
    {Enum.join(vars, " "), Enum.join(classes, ", ")}
  end

  def lookup_class_defn(fn_name, %{children: [_operator | args]}) do
    with {:ok, defn} <- Environment.lookup_instance_defn(fn_name),
         {:ok, types} = check_ast(args) do
      arg_types = Enum.map(types, &elem(&1, 1))
      get_defn_for_args(defn, arg_types)
    else
      error ->
        error
    end
  end
  def lookup_class_defn(_fn_name, _args), do: {:error, :no_matching_class_instance}

  def get_defn_for_args(defn, types) when is_list(types) do
    types
    |> Enum.reduce({:error, :no_matching_class_instance},
    fn (type, acc) ->
      case get_defn_for_type(defn, type) do
        nil ->
          acc
        instance ->
          {:ok, instance}
      end
    end
    )
  end
  def get_defn_for_type(defn, %Type{constructor: nil, type_constructor: c}) do
    Map.get(defn, c)
  end
  def get_defn_for_type(defn, %Type{constructor: c}), do: Map.get(defn, c)
  def get_defn_for_type(_defn, _type), do: nil
end

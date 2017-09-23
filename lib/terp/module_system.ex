defmodule Terp.ModuleSystem do
  @moduledoc"""
  Functionality supporting a basic module system, e.g. export functions from a
  module and import them in another.
  """
  alias Terp.AST
  alias Terp.Error
  alias Terp.Parser
  alias Terp.Types.Types
  alias RoseTree.Zipper

  @doc """
  Imports the definitions that are exported from external modules
  into the current one.

  Current implementation:
    1. Receives a list of filenames that contain the modules to import
         along with the current environment.
    2. Recurses through the list of filenames to:
      a. Read the source code in the file,
      b. parse the file and convert it into an AST,
      c. evaluate the AST in the current environment,
      d. parse out the list definitions the module provides,
      e. parse out all of the definitions in the module,
      f. hide un-exported definitions from the environment by unbinding them.
    3. Returns the environment that now contains the exported definitions
         from the required modules.
  """
  def require_modules(filenames, env), do: require_modules(filenames, env, [])
  def require_modules([], env, imports) do
    stringified_imports = Enum.map(imports, fn {f, i} -> "#{f}: #{Enum.join(i, ", ")}" end)
    {{:ok, {:imported, Enum.join(stringified_imports, "\n")}}, env}
  end
  def require_modules([filename | filenames], env, imports) do
    with {:ok, module} <- File.read(filename <> ".tp"),
         {:ok, _types} = Types.type_check(module),
         ast = module |> Parser.parse() |> Enum.flat_map(&AST.to_tree/1),
         {_res, environment} = Terp.run_eval(ast, env) do

      provides = find_exported_definitions(ast)
      defined = find_node_values_of_type(ast, [:__let, :__letrec])
      cleaned_environment = hide_private_fns({provides, defined}, environment)
      updated_imports = [{filename, provides} | imports]

      require_modules(filenames, cleaned_environment, updated_imports)
    else
      {:error, :enoent} ->
        {:error, {:module_doesnt_exist, filename}}
      %Error{} = error ->
        error
    end
  end

  # Loads the list of functions defined in a module of a given type.
  @spec find_node_values_of_type([RoseTree.t], [atom()]) :: [atom] | [String.t]
  defp find_node_values_of_type(trees, node_types) do
    nodes = find_node_types(trees, node_types)
    if Enum.empty?(nodes) do
      []
    else
      nodes
      |> Enum.map(&RoseTree.to_list/1)
      |> Enum.map(fn [_p | [_i | name]] -> List.first(name) end)
    end
  end

  @spec find_exported_definitions([RoseTree.t]) :: [atom] | [String.t]
  defp find_exported_definitions(trees) do
    nodes = find_node_types(trees, [:__provide])
    if Enum.empty?(nodes) do
      []
    else
      nodes
      |> Enum.map(&RoseTree.to_list/1)
      |> Enum.flat_map(fn [_p | [_i | name]] -> name end)
    end
  end

  # After loading the required module, hides the private
  # functions from the environment by resetting them to :unbound.
  defp hide_private_fns({provided, defined}, environment) do
    Enum.reduce(defined, environment,
      fn (definition, environment) ->
        if Enum.member?(provided, definition) do
          environment
        else
          fn name ->
            if name == definition, do: {:error, {:unbound, name}}, else: environment.(name)
          end
        end
      end
    )
  end

  # Filter the trees in a module to find only those of the given types.
  defp find_node_types(trees, node_types) do
    trees
    |> Enum.filter(fn tree ->
      first_node = tree
      |> Zipper.from_tree()
      |> Zipper.first_child()
      |> Zipper.lift(&Zipper.to_tree/1)
      Enum.member?(node_types, first_node.node)
    end)
  end
end

defmodule Terp.ModuleSystem do
  alias Terp.Parser
  alias RoseTree.Zipper

  def require_modules([], env), do: env
  def require_modules([filename | filenames], env) do
    case File.read(filename) do
      {:ok, module} ->
        {_res, environment} = module
        |> Parser.parse()
        |> Enum.flat_map(&Parser.to_tree/1)
        |> Terp.run_eval(env)

        require_modules(filenames, environment)
      {:error, :enoent} ->
        {:error, {:module_doesnt_exist, filename}}
    end
  end

  # Loads the list of functions a module provides.
  # Don't have a good solution for a function depending on another
  # function that is private. Currently everything will be an export.
  defp load_provides(trees) do
    provides = filter_for_provides(trees, :__provide)
    if Enum.empty?(provides) do
      []
    else
      [_p | [_i | provided]] = RoseTree.to_list(List.first(provides))
      provided
    end
  end

  # Obtain the list of provided functions from the module.
  defp filter_for_provides(trees, node_value) do
    trees
    |> Enum.filter(fn tree ->
      first_node = Zipper.from_tree(tree)
      |> Zipper.first_child()
      |> Zipper.lift(&Zipper.to_tree/1)
      (first_node.node == node_value)
    end)
  end
end

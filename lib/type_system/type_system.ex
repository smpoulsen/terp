defmodule Terp.TypeSystem do
  @moduledoc """
  The context entry point/interface for terp's type system.
  """
  alias Terp.Error
  alias Terp.TypeSystem.Type
  alias Terp.TypeSystem.Evaluator
  alias Terp.TypeSystem.TypeEnvironment

  @doc """
  Run the type evaluator for a given piece of source code.

  Converts the source to an AST and then type checks it.
  """
  @spec check_src(String.t) :: [Type.t]
  def check_src(src) do
    src
    |> Terp.to_ast()
    |> check_ast()
  end

  @doc """
  Runs the type evaluator for an AST.
  """
  @spec check_ast([RoseTree.t]) :: {:ok, [Type.t]} | {:error, any} | Error.t
  def check_ast(ast) do
    TypeEnvironment.start_if_unstarted()

    res = ast
    |> Enum.reduce({:ok, []}, &check_tree/2)

    case res do
      {:ok, types} ->
        {:ok, Enum.reverse(types)}
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
end

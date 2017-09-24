defmodule Terp.TypeSystem do
  @moduledoc """
  The context entry point/interface for terp's type system.
  """
  alias Terp.Error
  alias Terp.TypeSystem.TypeEvaluator
  alias Terp.TypeSystem.TypeEnvironment

  @doc """
  Run the type evaluator for a given piece of source code.

  Converts the source to an AST and then type checks it.
  """
  @spec check_src(String.t) :: [Types.t]
  def check_src(src) do
    src
    |> Terp.to_ast()
    |> check_ast()
  end

  @doc """
  Runs the type evaluator for an AST.
  """
  @spec check_ast([RoseTree.t]) :: {:ok, [Types.t]} | {:error, any} | Error.t
  def check_ast(ast) do
    TypeEnvironment.start_if_unstarted()
    res = ast
    |> Enum.reduce({:ok, []},
    fn tree, {:ok, types} ->
      case TypeEvaluator.run_infer(tree) do
        {:error, _} = e ->
          e
        %Error{} = e ->
          e
        {:ok, type} ->
          {:ok, [type | types]}
      end
      (_tree, {:error, e}) -> {:error, e}
      (_tree, %Error{} = e) -> e
    end)

    case res do
      {:ok, types} -> {:ok, Enum.reverse(types)}
      error -> error
    end
  end
end

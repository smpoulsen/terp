defmodule Terp.Evaluate.Environment do
  @moduledoc """
  Functionality for updating the environment in the interpreter.
  """
  alias __MODULE__
  alias Terp.Evaluate

  @doc """
  Add a new named [function | variable] to the environment.
  """
  def let([name | [bound | []]], env) do
    Evaluate.eval_expr(name, fn arg ->
      fn name ->
        if arg == name, do: Evaluate.eval_expr(bound, env), else: env.(name)
      end
    end)
  end

  def quote(children) do
    for child <- children do
      case child.node do
        :__quote ->
          Environment.quote(child.children)
        res ->
          res
      end
    end
  end
end

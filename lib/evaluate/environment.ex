defmodule Terp.Evaluate.Environment do
  @moduledoc """
  Functionality for updating the environment in the interpreter.
  """
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
    Enum.map(children, &(&1.node))
  end
end

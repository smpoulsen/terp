defmodule Terp.Environment do
  @moduledoc """
  Functionality for updating the environment in the interpreter.
  """

  @doc """
  Add a new named [function | variable] to the environment.
  """
  def let([name | [bound | []]], env) do
    Terp.eval_expr(name, fn arg ->
      fn name ->
        if arg == name, do: Terp.eval_expr(bound, env), else: env.(name)
      end
    end)
  end

  def quote(children) do
    Enum.map(children, &(&1.node))
  end
end

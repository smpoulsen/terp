defmodule Mix.Tasks.Terp.Repl do
  @moduledoc """
  Runs a terp file (extension '.tp').
  """
  use Mix.Task
  alias Terp.Repl

  def run(args) do
    Repl.init()
  end
end

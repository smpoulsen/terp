defmodule Mix.Tasks.Terp.Run do
  @moduledoc """
  Runs a terp file (extension '.tp').
  """
  use Mix.Task
  @debug true

  def run(args) do
    for file <- args do
      res = Terp.IO.run_terp(file)
      if @debug do
        IO.inspect(res)
      end
      res
    end
  end
end

defmodule Mix.Tasks.Terp.Run do
  @moduledoc """
  Runs a terp file (extension '.tp').
  """
  use Mix.Task
  alias Terp.Error
  @debug true

  def run(args) do
    for file <- args do
      res = Terp.IO.run_terp(file)
      case res do
        {:error, _e} ->
          Error.pretty_print_error(res, file)
        %Error{} ->
          Error.pretty_print_error(res)
        _ ->
          if @debug do
            IO.inspect(res, charlists: :as_lists)
          end
      end
      res
    end
  end
end

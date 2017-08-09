defmodule Terp.Repl do
  @moduledoc """
  A REPL (read-eval-print-loop) for terp.
  """

  def loop(environment \\ fn (z) -> {:error, {:unbound_variable, z}} end) do
    expr = IO.gets("terp> ")
    case expr do
      :eof -> IO.write("\nBye!")
      _ ->
        {res, env} = Terp.evaluate_source(expr, environment)
        case res do
          {:error, {type, msg}} ->
            Bunt.puts([:red, "There was an error:"])
            IO.puts("\tmsg: #{Atom.to_string(type)}")
            IO.puts("\targ: #{msg}")
          nil ->
            Bunt.puts([:green, "ok"])
          _ ->
            IO.puts(res)
        end
        loop(env)
    end
  end

  def track_history() do
    # use a zipper to track history.
  end
end

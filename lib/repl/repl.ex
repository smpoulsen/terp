defmodule Terp.Repl do
  @moduledoc """
  A REPL (read-eval-print-loop) for terp.
  """

  def loop(environment \\ fn (z) -> {:error, {:unbound, z}} end) do
    expr = IO.gets("terp> ")
    case expr do
      :eof -> IO.write("\nBye!")
      _ ->
        {res, env} = Terp.evaluate_source(expr, environment)
        IO.write(res)
        IO.write("\n")
        loop(env)
    end
  end
end

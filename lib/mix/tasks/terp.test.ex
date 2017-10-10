defmodule Mix.Tasks.Terp.Test do
  @moduledoc """
  Tests a terp file.
  """
  use Mix.Task
  alias Terp.Test

  @doc """
  Runs the tests in the provided paths.
  Paths can be either files or directories.
  """
  def run(args) do
    started_at = :erlang.timestamp()
    {:ok, prelude} = File.read("prelude/prelude.tp")
    # Load definitions and types into the environment.
    {_, environment} = Terp.eval_source(prelude)
    res = Enum.reduce(args, %{env: environment, tests: 0, failures: 0}, fn
      (path, %{tests: t1, failures: f1}) ->
        test_result =
          if File.dir?(path) do
            Test.test_dir(path, environment)
          else
            Test.run_tests(path, environment)
          end
      case test_result do
        %{tests: t2, failures: f2} ->
          %{tests: t1 + t2, failures: f1 + f2}
        {:error, _e} = error ->
          error
      end
    end)
    case res do
      {:error, e} ->
        Bunt.puts([:red, e])
      %{tests: tests, failures: failures} ->
        stopped_at = :erlang.timestamp()
        micro_sec_diff = :timer.now_diff(stopped_at, started_at)
        IO.puts("\nfinished in #{micro_sec_diff / 1_000_000} seconds\n")
        color = if failures > 0, do: :red, else: :green
        Bunt.puts([color, "#{tests} tests, #{failures} failures"])
    end
  end
end

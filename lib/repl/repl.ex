defmodule Terp.Repl do
  @moduledoc """
  A REPL (read-eval-print-loop) for terp.
  """
  alias Terp.TypeSystem
  alias Terp.Error

  def init() do
    # Starts a persisted type environment for the current session.
    TypeSystem.start_environment()
    loop(fn (z) -> {:error, {:unbound, z}} end)
  end

  def loop(environment) do
    expr = IO.gets("terp> ")
    case expr do
      :eof ->
        # Ctl-D
        IO.write("\nBye!")
      _ ->
        cond do
          String.starts_with?(expr, ":t ") || String.starts_with?(expr, ":type ") ->
            type_check(expr)
            loop(environment)
          true ->
            env = expr
            |> String.trim()
            |> eval(environment)
            loop(env)
        end
    end
  end

  defp type_check(expr) do
    trimmed = expr
    |> String.trim()
    |> String.trim_leading(":type ")
    |> String.trim_leading(":t ")

    inference = trimmed
    |> TypeSystem.check_src()

    case inference do
      {:error, _} = e ->
        Error.pretty_print_error(e, trimmed)
      :ok ->
        {:ok, nil}
      {:ok, types} ->
        {type_vars, type} = List.first(types)
        type_str = if Enum.empty?(type_vars) do
          to_string(type)
        else
          variables = Enum.map(type_vars, &(to_string(&1)))
          "∀ #{Enum.join(variables, " ")} => #{to_string(type)}"
        end
        Bunt.puts([:blue, trimmed, :green, " : ", :yellow, type_str])
    end
  end

  # Evaluate the expression and return the updated environment.
  defp eval(expr, environment) do
    with {:ok, _type} <- TypeSystem.check_src(expr),
         {res, env} <- Terp.eval_source(expr, environment) do
      case res do
        {:error, _} = e ->
          Error.pretty_print_error(e, expr)
          environment
        %Error{} ->
          Error.pretty_print_error(res)
          environment
        {:ok, {_type, msg}} ->
          Bunt.puts([:green, "Success!"])
          Bunt.puts([:blue, "#{msg}"])
          env
        {:imported, functions} ->
          Bunt.puts([:green, "Success! Imported the following:"])
          Bunt.puts([:blue, "#{functions}"])
          env
        nil ->
          env
        _ ->
          IO.inspect(res, charlists: :as_lists)
          env
      end
      env
    else
      nil ->
        environment
      :ok ->
        environment
      {:error, _} = e ->
        Error.pretty_print_error(e, expr)
        environment
      %Error{} = error->
        Error.pretty_print_error(error)
      environment
    end
  end
end

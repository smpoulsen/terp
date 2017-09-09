defmodule Terp.Repl do
  @moduledoc """
  A REPL (read-eval-print-loop) for terp.
  """
  alias Terp.Types.Types
  alias Terp.Types.TypeEnvironment

  def init() do
    # Starts a persisted type environment for the current session.
    TypeEnvironment.start_link()
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
    |> Types.type_check()
    |> List.first() #TODO

    case inference do
      {:error, _} = e ->
        pretty_print_error(e, trimmed)
      :ok ->
        {:ok, nil}
      {:ok, {type_vars, type}} ->
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
    with {:ok, _type} <- List.first(Types.type_check(expr)),
         {res, env} <- Terp.evaluate_source(expr, environment) do
      case res do
        {:error, _} = e ->
          pretty_print_error(e, expr)
          environment
        {:ok, {_type, msg}} ->
          Bunt.puts([:green, "Success!"])
          Bunt.puts([:blue, "#{msg}"])
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
        pretty_print_error(e, expr)
        environment
    end
  end

  defp pretty_print_error({:error, {:type, {:unification, %{expected: e, received: r}}}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["Unable to unify types in the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts(["Expected: ", :blue, "#{to_string(e)}"])
    Bunt.puts(["Received: ", :red, "#{to_string(r)}"])
  end
  defp pretty_print_error({:error, {:type, {:annotation, %{expected: e, actual: a}}}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["The expected type does not match the actual type in the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts(["Expected type: ", :blue, "#{to_string(e)}"])
    Bunt.puts(["  Actual type: ", :red, "#{to_string(a)}"])
  end
  defp pretty_print_error({:error, {:type, msg}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["#{msg} when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
  end
  defp pretty_print_error({:error, {:unbound, name}}, expr) do
    Bunt.puts([:red, "--UNBOUND VARIABLE ERROR--"])
    Bunt.puts([:yellow, :bright,  name, :lightgray, " is not in scope when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
  end
  defp pretty_print_error({:error, {module, reason}}, expr) do
    Bunt.puts([:red, "--#{String.upcase(to_string(module))} ERROR--"])
    Bunt.puts(["#{reason} when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
  end
end

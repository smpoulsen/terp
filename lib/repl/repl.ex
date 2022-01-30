defmodule Terp.Repl do
  @moduledoc """
  A REPL (read-eval-print-loop) for terp.
  """
  alias Terp.TypeSystem
  alias Terp.ModuleSystem
  alias Terp.Error
  alias Terp.Value

  def init() do
    # Starts a persisted type environment for the current session.
    TypeSystem.start_environment()
    {:ok, code} = File.read("prelude/prelude.tp")
    {_, environment} = code
    |> Terp.eval_source()
    loop(environment)
  end

  def loop(environment, current_expr \\ "") do
    expr = IO.gets("terp> ")
    case expr do
      :eof ->
        # Ctl-D
        IO.write("\nBye!")
      _ ->
        full_expr = current_expr <> expr
        cond do
          incomplete_expr?(full_expr) && !double_new_line(full_expr) ->
            loop(environment, full_expr)
          String.starts_with?(expr, ":h") || String.starts_with?(expr, ":help") ->
            print_help()
            loop(environment)
          String.starts_with?(expr, ":t ") || String.starts_with?(expr, ":type ") ->
            type_check(expr)
            loop(environment)
          String.starts_with?(expr, ":m ") || String.starts_with?(expr, ":module ") ->
            env = expr
            |>require_module(environment)
            loop(env)
          true ->
            env = full_expr
            |> String.trim()
            |> eval(environment)
            loop(env)
        end
    end
  end

  defp print_help() do
    Bunt.puts([:green, "--TERP REPL HELP--"])
    IO.puts("The REPL is a place to interactively write programs.\n")
    IO.puts("--Special commands--")
    IO.puts("Special REPL commands begin with a \":\"; they can be given in full or with just the first character.")
    Bunt.puts([:green, "\t:help\n", :lightgray, "\t\tDisplay this help message."])
    Bunt.puts([:green, "\t:type\n", :lightgray, "\t\tType check an expression and output the result."])
    Bunt.puts([:green, "\t:module\n", :lightgray, "\t\tImport a module into the REPL. A shortcut for ", :green, "(require MODULES)"])
  end

  # Import modules into the REPL
  defp require_module(expr, env) do
    trimmed = expr
    |> String.trim_leading(":module")
    |> String.trim_leading(":m")
    |> String.trim()

    imports = trimmed
    |> String.split()
    |> ModuleSystem.require_modules(env)

    case imports do
      {{:ok, {:imported, functions}}, env} ->
        Bunt.puts([:green, "Success! Imported the following:"])
        Bunt.puts([:blue, "#{functions}"])
        env
      error ->
        Error.pretty_print_error(error, trimmed)
    end
  end

  # Type check an expression and print the type
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
        type_str = types
        |> List.first()
        |> TypeSystem.stringify_type_scheme()

        Bunt.puts([:blue, trimmed, :green, " : ", :yellow, type_str])
      error ->
        Error.pretty_print_error(error)
    end
  end

  # Evaluate the expression and return the updated environment.
  defp eval(expr, environment) do
    with {:ok, type} <- TypeSystem.check_src(expr),
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
          if List.first(type) !== %{} do
            type_str = elem(List.first(type), 1)
            case res do
              %Value{} ->
                res
                |> to_string()
                |> IO.puts()
              _ ->
                IO.inspect(res, charlists: :as_lists)
            end
            Bunt.puts([:blue, " : #{type_str}"])
          end
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

  # Helpers for determining whether a full command has been
  # entered into the REPL.
  defp complete_expr?(expr) do
    # Compares the number of opening parens to closing parens.
    opening_parens = expr
    |> String.graphemes()
    |> Enum.filter(&(&1 == "(" || &1 == "["))
    |> length()

    closing_parens = expr
    |> String.graphemes()
    |> Enum.filter(&(&1 == ")" || &1 == "]"))
    |> length()
    (opening_parens == closing_parens) && String.ends_with?(expr, "\n")
  end

  defp incomplete_expr?(expr) do
    !complete_expr?(expr)
  end

  defp double_new_line(expr) do
    String.ends_with?(expr, "\n\n")
  end
end

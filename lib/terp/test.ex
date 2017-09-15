defmodule Terp.Test do
  @moduledoc """
  Functions to facilitate terp testing itself.
  """
  alias Terp.Error
  alias Terp.AST
  alias Terp.Parser
  alias Terp.Types.Types
  alias RoseTree.Zipper

  def test_dir(path) do
    with {:ok, files} <- File.ls(path) do
      Enum.reduce(files, %{tests: 0, failures: 0}, fn
        (file, %{tests: t1, failures: f1} = state) ->
        if String.ends_with?(file, "_test.tp") do
          {%{tests: t2, failures: f2}, _env} = run_tests("#{path}/#{file}")
          %{tests: t1 + t2, failures: f1 + f2}
        else
          state
        end
      end)
    end
  end

  def run_tests(file) do
    Bunt.puts([file])
    with true <- Terp.IO.is_terp_file(file),
         {:ok, src} <- File.read(file),
         {:ok, _type} <- Types.type_check(src),
           ast = src |> Parser.parse() |> Enum.flat_map(&AST.to_tree/1) do
      initial_environment = fn x -> {:error, {:unbound, x}} end
      ast
      |> Enum.reduce({%{tests: 0, failures: 0}, initial_environment}, fn
        (tree, {state, env}) -> run_test(tree, {state, env}) end)
    else
      false ->
        "#{file} is not a valid terp file"
      %Error{} = error ->
        Error.pretty_print_error(error)
      {:error, _} = error ->
        Error.pretty_print_error(error)
    end
  end

  def run_test(expr, {state, env}) do
    res = Terp.eval_tree(expr, env)
    case res do
      {:ok, {:environment, environment}} ->
        # Environment was updated
        {state, environment}
      {:ok, {:evaluated, false, environment}} ->
        # Assertion made and evaluated false
        new_state = failed(expr, state)
        {new_state, environment}
      {:ok, {:evaluated, _res, environment}} ->
        # Some evaluation occurred, but did not make an assertion
        new_state = passed(expr, state)
        {new_state, environment}
      {:error, _e, environment} ->
        # An error occurred, e.g. a type error.
        new_state = failed(expr, state)
        {new_state, environment}
      %Error{} ->
        # An error occurred, e.g. a type error.
        new_state = failed(expr, state)
        {new_state, env}
    end
  end

  defp passed(expr, %{tests: tests} = state) do
    message = [:green, "\t. ", AST.stringify(expr)]
    if is_test?(expr) do
      Bunt.puts(message)
      %{state | tests: tests + 1}
    else
      state
    end
  end
  defp failed(expr, %{tests: tests, failures: failures} = state) do
    message = [:red, "\tx ", AST.stringify(expr)]
    if is_test?(expr) do
      Bunt.puts(message)
      %{tests: tests + 1, failures: failures + 1}
    else
      state
    end
  end

  defp is_test?(expr) do
    z = Zipper.from_tree(expr)
    {:ok, {%RoseTree{node: t}, _history}} = Zipper.first_child(z)
    Enum.member?(["assert", "refute"], t)
  end
end

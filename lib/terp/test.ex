defmodule Terp.Test do
  @moduledoc """
  Functions to facilitate terp testing itself.
  """
  alias Terp.AST
  alias Terp.Error
  alias Terp.Evaluate
  alias Terp.ModuleSystem
  alias Terp.Parser
  alias Terp.TypeSystem
  alias RoseTree.Zipper

  def test_dir(path, state \\ %{tests: 0, failures: 0}) do
    with {:ok, files} <- File.ls(path) do
      Enum.reduce(files, state, fn
        (file, %{tests: t1, failures: f1} = state) ->
        full_path = "#{path}/#{file}"
        if File.dir?(full_path) do
          test_dir(full_path, state)
        else
          if String.ends_with?(file, "_test.tp") do
            %{tests: t2, failures: f2} = run_tests(full_path)
            %{tests: t1 + t2, failures: f1 + f2}
          else
            state
          end
        end
      end)
    end
  end

  def run_tests(file) do
    Bunt.puts([file])
    with true <- Terp.IO.is_terp_file(file),
         {:ok, raw_src} <- File.read(file),
           src <- ModuleSystem.inject_prelude(raw_src),
           ast = src |> Parser.parse() |> Enum.flat_map(&AST.to_tree/1),
         {:ok, _type} <- TypeSystem.check_ast(ast) do
      initial_environment = fn x -> {:error, {:unbound, x}} end
      {stats, _env} = ast
      |> Enum.reduce({%{tests: 0, failures: 0}, initial_environment}, fn
        (tree, {state, env}) ->
          run_test(tree, {state, env})
      end)
      stats
    else
      false ->
        {:error, "#{file} is not a valid terp file"}
      %Error{} = error ->
        Error.pretty_print_error(error)
      {:error, :enoent} ->
        {:error, "#{file} not found"}
      {:error, _} = error ->
        Error.pretty_print_error(error)
    end
  end

  def run_test(expr, {state, env}) do
    with {:ok, _type} <- TypeSystem.check_ast(List.wrap(expr)),
         res <- Evaluate.eval_tree(expr, env) do
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
          Error.pretty_print_error(res)
          new_state = failed(expr, state)
          {new_state, environment}
        %Error{} ->
          # An error occurred, e.g. a type error.
          Error.pretty_print_error(res)
          new_state = failed(expr, state)
          {new_state, env}
      end
    else
      %Error{} = error ->
        new_state = failed(expr, state)
        Error.pretty_print_error(error)
        {new_state, env}
    end
  end

  defp passed(expr, %{tests: tests} = state) do
    if is_test?(expr) do
      message = [:green, "✓ ", test_name(expr)]
      Bunt.puts(message)
      %{state | tests: tests + 1}
    else
      state
    end
  end
  defp failed(expr, %{tests: tests, failures: failures} = state) do
    if is_test?(expr) do
      message = [:red, "x ", test_name(expr)]
      Bunt.puts(message)
      %{tests: tests + 1, failures: failures + 1}
    else
      state
    end
  end

  defp is_test?(expr) do
    {:ok, {%RoseTree{node: t}, _history}} = expr
    |> Zipper.from_tree()
    |> Zipper.first_child()
    t == "test"
  end

  defp test_name(expr) do
    %RoseTree{node: name} = expr
    |> Zipper.from_tree()
    |> Zipper.first_child()
    |> Zipper.lift(&Zipper.next_sibling/1)
    |> Zipper.lift(&Zipper.first_child/1)
    |> Zipper.lift(&Zipper.to_tree/1)
    name
  end
end

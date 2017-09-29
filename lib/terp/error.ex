defmodule Terp.Error do
  @moduledoc """
  Errors and functions to pretty print them.
  """
  import Lens
  deflenses [:kind, :type, :message, :evaluating, :in_expression]
  alias __MODULE__
  alias Terp.AST

  def pretty_print_error({:error, {:type, {:unification, %{expected: e, received: r}}}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["Unable to unify types in the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts(["Expected: ", :blue, "#{to_string(e)}"])
    Bunt.puts(["Received: ", :red, "#{to_string(r)}"])
    Bunt.puts([:red, "--TYPE ERROR--"])
  end
  def pretty_print_error({:error, {:type, {:annotation, %{expected: e, actual: a}}}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["The expected type does not match the actual type in the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts(["Expected type: ", :blue, "#{to_string(e)}"])
    Bunt.puts(["  Actual type: ", :red, "#{to_string(a)}"])
    Bunt.puts([:red, "--TYPE ERROR--"])
  end
  def pretty_print_error({:error, {:type, msg}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["#{msg} when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts([:red, "--TYPE ERROR--"])
  end
  def pretty_print_error({:error, {:unbound, name}}, expr) do
    Bunt.puts([:red, "--UNBOUND VARIABLE ERROR--"])
    Bunt.puts([:yellow, :bright,  name, :lightgray, " is not in scope when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts([:red, "--UNBOUND VARIABLE ERROR--"])
  end
  def pretty_print_error({:error, {:file, reason}}, file) do
    Bunt.puts([:red, "--FILE ERROR--"])
    Bunt.puts(["The following file was #{reason}:"])
    Bunt.puts([:blue, "\t#{file}"])
    Bunt.puts([:red, "--FILE ERROR--"])
  end
  def pretty_print_error({:error, {module, reason}}, expr) do
    Bunt.puts([:red, "--#{String.upcase(to_string(module))} ERROR--"])
    Bunt.puts(["#{reason} when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts([:red, "--#{String.upcase(to_string(module))} ERROR--"])
  end

  def pretty_print_error({:error, %Error{} = error}, _expr), do: pretty_print_error(error)

  def pretty_print_error({:error, {module, reason}}) do
    Bunt.puts([:red, "--#{String.upcase(to_string(module))} ERROR--"])
    Bunt.puts(["#{reason} when evaluating the expression"])
    Bunt.puts([:red, "--#{String.upcase(to_string(module))} ERROR--"])
  end

  def pretty_print_error(%Error{kind: k} = error) do
    Bunt.puts([:red, "--#{String.upcase(to_string(k))} ERROR--"])
    print_error_for_kind(error)
    Bunt.puts([:red, "--#{String.upcase(to_string(k))} ERROR--"])
  end

  defp print_error_for_kind(%Error{kind: :evaluation, type: _t, message: m, evaluating: v, in_expression: expr}) do
    Bunt.puts(["#{m} when evaluating"])
    Bunt.puts([:yellow, :bright, "\t#{v}"])
    Bunt.puts(["in the expression"])
    Bunt.puts([:blue, "\t#{AST.stringify(expr)}"])
  end
  defp print_error_for_kind(%Error{kind: :type, type: :unification, message: _m, evaluating: %{expr: val, expected: e, actual: a}, in_expression: expr}) do
    Bunt.puts(["The expected type does not match the actual type for the value"])
    Bunt.puts([:red, "\t#{AST.stringify(val)}"])
    Bunt.puts(["in the expression"])
    Bunt.puts([:blue, "\t#{AST.stringify(expr)}"])
    Bunt.puts(["Expected type: ", :blue, "#{to_string(e)}"])
    Bunt.puts(["  Actual type: ", :red, "#{to_string(a)}"])
  end
  defp print_error_for_kind(%Error{kind: :type, type: :unification, message: _m, evaluating: %{expected: e, actual: a}, in_expression: expr}) do
    # This duplicates the previous except for the expr key in evaluating.
    # Should be able to refactor to reduce repetition.
    Bunt.puts(["The expected type does not match the actual type"])
    Bunt.puts(["in the expression"])
    Bunt.puts([:blue, "\t#{AST.stringify(expr)}"])
    Bunt.puts(["Expected type: ", :blue, "#{to_string(e)}"])
    Bunt.puts(["  Actual type: ", :red, "#{to_string(a)}"])
  end
  defp print_error_for_kind(%Error{kind: :type, type: _t, message: m, evaluating: v, in_expression: expr}) do
    Bunt.puts(["#{m} when type checking the expression"])
    Bunt.puts([:blue, "\t#{AST.stringify(expr)}"])
    Bunt.puts(["for the type"])
    Bunt.puts([:yellow, :bright, "\t#{v}"])
  end
end

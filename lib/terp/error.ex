defmodule Terp.Error do

  def pretty_print_error({:error, {:type, {:unification, %{expected: e, received: r}}}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["Unable to unify types in the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts(["Expected: ", :blue, "#{to_string(e)}"])
    Bunt.puts(["Received: ", :red, "#{to_string(r)}"])
  end
  def pretty_print_error({:error, {:type, {:annotation, %{expected: e, actual: a}}}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["The expected type does not match the actual type in the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
    Bunt.puts(["Expected type: ", :blue, "#{to_string(e)}"])
    Bunt.puts(["  Actual type: ", :red, "#{to_string(a)}"])
  end
  def pretty_print_error({:error, {:type, msg}}, expr) do
    Bunt.puts([:red, "--TYPE ERROR--"])
    Bunt.puts(["#{msg} when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
  end
  def pretty_print_error({:error, {:unbound, name}}, expr) do
    Bunt.puts([:red, "--UNBOUND VARIABLE ERROR--"])
    Bunt.puts([:yellow, :bright,  name, :lightgray, " is not in scope when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
  end
  def pretty_print_error({:error, {:file, reason}}, file) do
    Bunt.puts([:red, "--FILE ERROR--"])
    Bunt.puts(["The following file was #{reason}:"])
    Bunt.puts([:blue, "\t#{file}"])
  end
  def pretty_print_error({:error, {module, reason}}, expr) do
    Bunt.puts([:red, "--#{String.upcase(to_string(module))} ERROR--"])
    Bunt.puts(["#{reason} when evaluating the expression"])
    Bunt.puts([:blue, "\t#{expr}"])
  end

  def pretty_print_error({:error, {module, reason}}) do
    Bunt.puts([:red, "--#{String.upcase(to_string(module))} ERROR--"])
    Bunt.puts(["#{reason} when evaluating the expression"])
  end
end

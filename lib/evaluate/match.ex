defmodule Terp.Evaluate.Match do
  @moduledoc """
  Pattern matching functionality.
  """
  alias Terp.Error
  alias Terp.Evaluate
  alias Terp.Value

  def match(expr, env) do
    [var | match_exprs] = expr
    evald_vars = Enum.map(var.node, &Evaluate.eval_expr(&1, env))
    # TODO Should this be just the head? Can you match multiple vars?
    # Not for now at least.
    check_matches((hd evald_vars), match_exprs, env)
  end

  def check_matches(_evald_vars, [], _env) do
    %Error{kind: :evaluation,
           type: :match,
           message: "No successful pattern match"}
  end
  def check_matches(evald_vars, [match_expr | match_exprs], env) do
    case check_match(evald_vars, match_expr) do
      {:no_match} ->
        check_matches(evald_vars, match_exprs, env)
      {:ok, consequent} ->
        Evaluate.eval_expr(consequent, env)
    end
  end

  def check_match(evald_var, %{node: [match | [consequent | []]]}) do
    case match.node do
      :__apply ->
        [constructor | vars] = match.children
        with true <- matches?(evald_var, constructor),
             true <- length(evald_var.args) == length(vars) do
          updated_consequent = vars
          |> Stream.map(&(&1.node))
          |> Enum.zip(evald_var.args)
          |> Enum.reduce(consequent, fn ({var, val}, consequent_expr) ->
            RoseTree.update_node(consequent_expr, var, val)
          end)
          {:ok, updated_consequent}
        else
          false -> {:no_match}
        end
    end
  end

  # Does the evaluated var match the thing in the pattern match
  defp matches?(%Value{constructor: c}, %RoseTree{node: c}), do: true
  defp matches?(x, %RoseTree{node: x}), do: true
  defp matches?(_evald_var, _comparison), do: false
end

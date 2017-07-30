defmodule Terp do
  @moduledoc """
  A toy interpreter.
  """
  alias Terp.Parser
  alias Terp.Arithmetic
  alias Terp.Boolean
  alias Terp.Function

  @doc """
  Evaluate a terp expression.

  ## Example

      iex> "(+ 5 3)"
      ...> |> Terp.eval()
      8

      iex> "(* 2 4 5)"
      ...> |> Terp.eval()
      40

      iex> "(* 2 4 (+ 4 1))"
      ...> |> Terp.eval()
      40

      iex> "(if #t (* 5 5) (+ 4 1))"
      ...> |> Terp.eval()
      25

      iex> "(if #f (* 5 5) 5)"
      ...> |> Terp.eval()
      5
  """
  def eval(str) do
    str
    |> Parser.parse()
    |> Parser.to_tree()
    |> eval_tree()
  end

  @doc """
  Evaluate an expression represented by a parse tree.

  ## Example

      # (+ 5 3)
      iex> "(+ 5 3)"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.Parser.to_tree()
      ...> |> Terp.eval_tree()
      8

      # (* 2 4 5)
      iex> "(* 2 4 5)"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.Parser.to_tree()
      ...> |> Terp.eval_tree()
      40

      # (* 2 4 (+ 4 1))
      iex> "(* 2 4 (+ 4 1))"
      ...> |> Terp.Parser.parse()
      ...> |> Terp.Parser.to_tree()
      ...> |> Terp.eval_tree()
      40
  """
  def eval_tree(%RoseTree{node: node, children: children} = tree, env \\ fn (_y) -> {:error, :unbound} end) do
    case node do
      x when is_number(x) -> x
      x when is_atom(x) and x !== :__apply ->
        env.(x)
      :__apply ->
        %{node: f, children: cs} = func = List.first(children)
        IO.inspect({"*****", tree})
        IO.inspect({"!!!!!", f})
        IO.inspect({"?????", cs})
        if f == "lambda" do
          eval_tree(func, env)
        else
          Function.apply_fn(eval_tree(func, env), Enum.map(cs, &eval_tree(&1, env)))
        end
      "#t" -> Boolean.t()
      "#f" -> Boolean.f()
      "+" -> fn y -> Arithmetic.add(y) end
      "*" -> fn y -> Arithmetic.multiply(y) end
      "-" -> fn y -> Arithmetic.subtract(y) end
      "/" -> fn y -> Arithmetic.divide(y) end
      "if" -> fn y -> Boolean.conditional(y) end
      "lambda" ->
          fn _ -> Function.lambda(children, env) end
      x when is_function(x) -> apply(x, Enum.map(children, &(eval_tree(&1, env))))
      x = %RoseTree{} -> # This is the case for lambda application; should be able to neaten up
        eval_tree(%RoseTree{node: eval_tree(x, env), children: children}, env)
    end
  end
end

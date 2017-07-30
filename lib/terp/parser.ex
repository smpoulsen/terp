defmodule Terp.Parser do
  @moduledoc """
  Parse a string into an AST
  """
  use Combine
  use Combine.Helpers

  @doc """
  The parser and tokenizes for terp.

  ## Examples

      iex> Terp.Parser.parse("(+ 1 2 3)")
      [__apply: ["+", 1, 2, 3]]

      iex> Terp.Parser.parse("(+ 1 2 (* 2 3))")
      [__apply: ["+", 1, 2, {:__apply, ["*", 2, 3]}]]
  """
  def parse(str) do
    str
    |> Combine.parse(expr_parser())
  end

  @doc """
  `expr_parser/0` parses a terp expression.

  Valid expressions are s-expression like (they are not however
  stored internally as binary trees); expressions are enclosed
  in parentheses and use prefix notation.

  TODO: currently, parsing application of literals works even
  if it doesn't make sense to, e.g. `(#t)` is considered valid
  """
  def expr_parser() do
    choice([
      literal_parser(),
      list_parser(),
      application_parser(),
    ])
  end

  @doc """
  `application_parser/0` parses function application.
  """
  def application_parser() do
    app_parser = between(
      char("("),
      many(
        choice([
          literal_parser(),
          ignore(space()),
          list_parser(),
          lazy(fn -> application_parser() end)
        ])
      ),
      char(")")
    )
    map(app_parser, fn x -> {:__apply, x} end)
  end

  @doc """
  Parses a quoted list, e.g. '(4 23 6).

  """
  def list_parser() do
    between(
      string("'("),
      many(
        choice([
          literal_parser(),
          ignore(space()),
        ])
      ),
      char(")")
    )
  end

  @doc """
  Parses literals
  """
  def literal_parser() do
    choice([
      built_ins_parser(),
      bool_parser(),
      integer(),
      map(ignore(char(":")) |> word(), fn atom -> String.to_atom(atom) end),
      word(),
      lazy(fn -> list_parser() end),
    ])
  end

  defp bool_parser() do
    either(
      string("#t"), # boolean true
      string("#f") # boolean false
    )
  end

  # Parses valid built-in functions/terms in terp.
  defp built_ins_parser() do
    choice([
      char("+"),
      char("-"),
      char("*"),
      char("/"),
    ])
  end

  @doc """
  `to_tree/1` takes a tokenized expression and builds a parse tree.

  ## Examples

      iex> ["+", 1, 2, 3]
      ...> |> Terp.Parser.to_tree()
      %RoseTree{
        node: %RoseTree{node: "+", children: []},
        children: [
          %RoseTree{node: 1, children: []},
          %RoseTree{node: 2, children: []},
          %RoseTree{node: 3, children: []},
      ]}

      iex> ["+", 1, 2, ["*", 2, 3]]
      ...> |> Terp.Parser.to_tree()
      %RoseTree{
        node: %RoseTree{node: "+", children: []},
        children: [
          %RoseTree{node: 1, children: []},
          %RoseTree{node: 2, children: []},
          %RoseTree{
            node: %RoseTree{node: "*", children: []},
            children: [
              %RoseTree{node: 2, children: []},
              %RoseTree{node: 3, children: []},
            ]
          },
        ]
      }
  """
  def to_tree([]), do: {:error, :no_parse}
  def to_tree({:__apply, parsed}), do: RoseTree.new(:__apply, to_tree(parsed))
  def to_tree([{:__apply, parsed}]), do: RoseTree.new(:__apply, to_tree(parsed))
  def to_tree(expr) when is_list(expr) do
    [operator | operands] = for v <- expr do
      case v do
        val when is_list(val) ->
          to_tree(val)
        {:__apply, x} ->
          RoseTree.new(:__apply, to_tree(x))
        _ -> v
      end
    end
    RoseTree.new(operator, operands)
  end

  # lazy parser implementation from
  # https://github.com/bitwalker/combine/issues/12#issuecomment-222539479
  defparser lazy(%Combine.ParserState{status: :ok} = state, generator) do
    (generator.()).(state)
  end
end

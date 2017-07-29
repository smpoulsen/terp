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
      [["+", 1, 2, 3]]

      iex> Terp.Parser.parse("(+ 1 2 (* 2 3))")
      [["+", 1, 2, ["*", 2, 3]]]
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
      application_parser(),
      literal_parser(),
    ])
  end

  @doc """
  `application_parser/0` parses function application.
  """
  def application_parser() do
    between(
      char("("),
      many(
        choice([
          literal_parser(),
          ignore(space()),
          lazy(fn -> application_parser() end)
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
      %RoseTree{node: "+", children: [
        %RoseTree{node: 1, children: []},
        %RoseTree{node: 2, children: []},
        %RoseTree{node: 3, children: []},
      ]}

      iex> ["+", 1, 2, ["*", 2, 3]]
      ...> |> Terp.Parser.to_tree()
      %RoseTree{node: "+", children: [
        %RoseTree{node: 1, children: []},
        %RoseTree{node: 2, children: []},
        %RoseTree{node: "*", children: [
          %RoseTree{node: 2, children: []},
          %RoseTree{node: 3, children: []},
        ]},
      ]}
  """
  def to_tree([]), do: {:error, :no_parse}
  def to_tree([nested_parse]) when is_list(nested_parse), do: to_tree(nested_parse)
  def to_tree([operator | operands]) do
    operator = if is_list(operator), do: to_tree(operator), else: operator
    children = for child <- operands do
      if is_list(child), do: to_tree(child), else: child
    end
    RoseTree.new(operator, children)
  end

  # lazy parser implementation from
  # https://github.com/bitwalker/combine/issues/12#issuecomment-222539479
  defparser lazy(%Combine.ParserState{status: :ok} = state, generator) do
    (generator.()).(state)
  end
end

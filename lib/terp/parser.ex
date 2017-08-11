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
      [[__apply: [:"__+", 1, 2, 3]]]

      iex> Terp.Parser.parse("(+ 1 2 (* 2 3))")
      [[__apply: [:"__+", 1, 2, {:__apply, [:"__*", 2, 3]}]]]
  """
  def parse(str) do
    str
    |> Combine.parse(many(expr_parser()))
  end

  # `expr_parser/0` parses a terp expression.
  # Valid expressions are s-expression like (they are not however
  # stored internally as binary trees); expressions are enclosed
  # in parentheses and use prefix notation.
  defp expr_parser() do
    choice([
      comment_parser(),
      literal_parser(),
      list_parser(),
      cond_parser(),
      application_parser(),
      ignore(newline()),
    ])
  end

  # `application_parser/0` parses function application.
  defp application_parser() do
    app_parser = valid_expr_parser()
      |> between_parens_parser()
    map(app_parser, fn x -> {:__apply, x} end)
  end

  # Parse a cond expression: (cond [c r] [c r])
  defp cond_parser() do
    l_parser = between(
      string("(cond"),
      many(
        choice([
          between(
            either(string("["), string("(")),
            valid_expr_parser(),
            either(string("]"), string(")"))
          ),
          ignore(space()),
          ignore(newline()),
        ])
      ),
      char(")")
    )
    map(l_parser, fn x -> {:__cond, x} end)
  end

  defp valid_expr_parser() do
    many(
      choice([
        literal_parser(),
        list_parser(),
        lazy(fn -> cond_parser() end),
        lazy(fn -> application_parser() end),
        ignore(space()),
        ignore(newline()),
      ])
    )
  end

  # Wrapper for parenthesized expressions
  defp between_parens_parser(parser) do
    between(
      either(char("("), char("[")),
      parser,
      either(char(")"), char("]"))
    )
  end

  # Parses a quoted list, e.g. '(4 23 6).
  defp list_parser() do
    lst_parser = between(
      string("'("),
      many(
        choice([
          literal_parser(),
          ignore(space()),
        ])
      ),
      char(")")
    )
    map(lst_parser, fn x -> {:__quote, x} end)
  end

  # Parses literals
  defp literal_parser() do
    choice([
      built_ins_parser(),
      bool_parser(),
      integer(),
      punctuation_parser(),
      string_to_atom(ignore(char(":")) |> word()),
      both(word(), char("?"), &(&1 <> &2)),
      both(word(), char("!"), &(&1 <> &2)),
      string_parser(),
      word(),
      lazy(fn -> list_parser() end),
    ])
  end

  defp string_parser() do
    map(
      between(
        char("\""),
        lazy(fn -> many(literal_parser()) end),
        char("\"")
      ),
      &({:__string, Enum.join(&1)})
    )
  end

  defp punctuation_parser() do
    choice([
      string("/"),
      string("."),
    ])
  end

  # Converts the result of a parser to an atom
  defp string_to_atom(parser) do
    map(parser, fn atom -> String.to_atom(atom) end)
  end

  # Parses true/false booleans
  defp bool_parser() do
    either(
      to_built_in(string("#t")), # boolean true
      to_built_in(string("#f")) # boolean false
    )
  end

  # Parses valid built-in functions/terms in terp.
  defp built_ins_parser() do
    choice([
      to_built_in(char("+")),
      to_built_in(char("-")),
      to_built_in(char("*")),
      to_built_in(string("div")), # / collides with the symbol
      to_built_in(string("if")),
      to_built_in(string("car")),
      to_built_in(string("cdr")),
      to_built_in(string("cons")),
      to_built_in(string("empty?")),
      to_built_in(string("equal?")),
      to_built_in(string("lambda")),
      to_built_in(string("letrec")),
      to_built_in(string("let")),
      to_built_in(string("require")),
      to_built_in(string("provide")),
    ])
  end

  defp to_built_in(parser) do
    map(parser, fn x -> String.to_atom("__" <> x) end)
  end

  defp comment_parser() do
    map(
      string(";;")
      |> take_while(
           fn ?\n -> false
                _ -> true end),
      fn x -> {:__comment, x} end
    )
  end

  @doc """
  `to_tree/1` takes a tokenized expression and builds a parse tree.

  ## Examples

      iex> [:__apply, [:+, 1, 2, 3]]
      ...> |> Terp.Parser.to_tree()
      [
      %RoseTree{node: :__apply, children: []},
        [
          %RoseTree{node: :+, children: []},
          %RoseTree{node: 1, children: []},
          %RoseTree{node: 2, children: []},
          %RoseTree{node: 3, children: []},
        ]
      ]

      iex> [:+, 1, 2, [:*, 2, 3]]
      ...> |> Terp.Parser.to_tree()
      [
        %RoseTree{node: :+, children: []},
        %RoseTree{node: 1, children: []},
        %RoseTree{node: 2, children: []},
        [
          %RoseTree{node: :*, children: []},
          %RoseTree{node: 2, children: []},
          %RoseTree{node: 3, children: []},
        ]
      ]
  """
  def to_tree([]), do: []
  def to_tree(expr) when is_list(expr) do
    for v <- expr do
      case v do
        {s, x} when is_atom(s) ->
          RoseTree.new(s, to_tree(x))
        val when is_list(val) ->
          to_tree(val)
        _ -> RoseTree.new(v)
      end
    end
  end
  def to_tree(x), do: RoseTree.new(x)

  # lazy parser implementation from
  # https://github.com/bitwalker/combine/issues/12#issuecomment-222539479
  defparser lazy(%Combine.ParserState{status: :ok} = state, generator) do
    (generator.()).(state)
  end
end

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
      [[__apply: ["+", 1, 2, 3]]]

      iex> Terp.Parser.parse("(+ 1 2 (* 2 3))")
      [[__apply: ["+", 1, 2, {:__apply, ["*", 2, 3]}]]]
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
      let_values_parser(),
      cond_parser(),
      typedef_parser(),
      type_class(),
      type_class_instance(),
      type_annotation_parser(),
      defn_parser(),
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
    l_parser = sequence([
      either(string("cond"), string("match")),
      many(
        choice([
          between_parens_parser(
            valid_expr_parser()
          ),
          ignore(space()),
          ignore(newline()),
        ])
      )
      ])
    |> between_parens_parser()

    map(l_parser, fn [f_name, rest] ->
      f = case f_name do
            "cond" -> :__cond
            "match" -> :__match
          end
      {f, rest}
    end)
  end

  # local bindings
  defp let_values_parser() do
    parser = between_parens_parser(
      sequence([
        string("let-values"),
        ignore(many(either(space(), newline()))),
        between_parens_parser(
          many(
            choice([
              between_parens_parser(
                valid_expr_parser()
              ),
              ignore(space()),
              ignore(newline()),
            ])
          )
        ),
        ignore(many(either(space(), newline()))),
        application_parser(),
      ])
    )
    map(parser, fn ["let-values" | rest] -> {:__let_values, rest} end)
  end

  #
  ## Data type definition
  #
  defp typedef_parser() do
    l_parser = sequence([
      ignore(char("(")),
      ignore(string("data")),
      ignore(either(newline(), space())),
      between(string("("), valid_expr_parser(), string(")")),
      ignore(either(newline(), space())),
      many1(
        choice([
          between(
            string("["),
            valid_expr_parser(),
            string("]")
          ),
          ignore(either(newline(), space())),
        ])
      ),
      ignore(char(")"))
    ])
    map(l_parser, fn x -> {:__data, x} end)
  end

  #
  ## Type Annotation
  #
  defp type_annotation_parser() do
    # This is specifically function annotation currently.
    t_parser = sequence([
      ignore(char("(")),
      ignore(string("type")),
      ignore(either(newline(), space())),
      choice([hyphenated_word(), type_parser(), beam_term_parser(), punctuation_parser()]),
      ignore(either(newline(), space())),
      type_parser(),
      ignore(char(")"))
    ])
    map(t_parser, fn x -> {:__type, x} end)
  end

  defp type_parser() do
    choice([
      between(
        char("["),
        word() |> sep_by(space()),
        char("]")
      ),
      arrow_parser(),
      word(),
    ])
  end

  defp type_class() do
    c_parser = sequence([
      ignore(char("(")),
      ignore(string("class")),
      ignore(many(either(newline(), space()))),
      between(string("["),
        sep_by(hyphenated_word(), space()),
        string("]")),
      ignore(many(either(newline(), space()))),
      between(
        string("["),
        many1(
          choice([
            type_annotation_parser(),
            ignore(either(newline(), space())),
          ])
        ),
        string("]")
      ),
      ignore(char(")"))
    ])
    map(c_parser, fn x -> {:__class, x} end)
  end

  defp type_class_instance() do
    c_parser = sequence([
      ignore(char("(")),
      ignore(string("instance")),
      ignore(many(either(newline(), space()))),
      between(string("["),
        sep_by(type_parser(), space()),
        string("]")),
      ignore(many(either(newline(), space()))),
      between(
        string("["),
        many1(
          choice([
            lazy(fn -> expr_parser() end),
            ignore(either(newline(), space()))
          ])
        ),
        string("]")
      ),
      ignore(char(")"))
    ])
    map(c_parser, fn x -> {:__instance, x} end)
  end

  defp arrow_parser() do
    between(
      char("("),
      sequence([
        map(string("->"), fn _x -> :__arrow end),
        ignore(space()),
        lazy(fn -> type_parser() end),
        ignore(space()),
        lazy(fn -> type_parser() end),
      ]),
      char(")")
    )
  end

  defp defn_parser() do
    p = sequence([
      either(string("defn"), string("defrec")),
      ignore(either(newline(), space())),
      either(hyphenated_word(), punctuation_parser()), # fn_name
      ignore(either(newline(), space())),
      between_parens_parser( # Args
        valid_expr_parser()
      ),
      ignore(either(newline(), space())),
      valid_expr_parser(),
    ])
    |> between_parens_parser()
    # Desugar defn to a let/lambda definition.
    map(p, fn [defn, name, args, body] ->
      f = case defn do
            "defn" -> :__let
            "defrec" -> :__letrec
          end
      b = case body do
            [__apply: x] ->
              {:__apply, x}
            [x] ->
              x
          end
      {:__apply, [f, name, {:__apply, [:__lambda, {:__apply, args}, b]}]}
    end)
  end

  defp valid_expr_parser() do
    many(
      choice([
        literal_parser(),
        list_parser(),
        lazy(fn -> let_values_parser() end),
        lazy(fn -> cond_parser() end),
        lazy(fn -> application_parser() end),
        string_parser(),
        ignore(space()),
        ignore(newline()),
      ])
    )
  end

  # Wrapper for parenthesized expressions
  defp between_parens_parser(parser) do
    between(
      either(char("("), char("[")),
      followed_by(
        parser,
        either(char(")"), char("]"))
      ),
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
      beam_term_parser(),
      built_ins_parser(),
      bool_parser(),
      float(),
      integer(),
      punctuation_parser(),
      string_to_atom(ignore(char(":")) |> word()),
      both(hyphenated_word(), char("?"), &(&1 <> &2)),
      both(hyphenated_word(), char("!"), &(&1 <> &2)),
      module_path(),
      hyphenated_word(),
      string_parser(),
      lazy(fn -> list_parser() end),
    ])
  end

  # BEAM terms, Elixir and Erlang expressions, should
  # be prefixed with a `:`.
  defp beam_term_parser() do
    [ignore(char(":")),
     hyphenated_word(),
     ignore(char(".")),
     string_to_atom(either(hyphenated_word(), punctuation_parser()))]
    |> sequence()
    |> map(&{:__beam, &1})
  end

  defp string_parser() do
    map(
      between(
        char("\""),
        lazy(fn ->
          many(
            choice([
              literal_parser(),
              space(),
            ])
          )
        end),
        char("\"")
      ),
      &({:__string, Enum.join(&1)})
    )
  end

  defp punctuation_parser() do
    choice([
      string("/"),
      string("."),
      string("+"),
      string("-"),
      string("="),
      string("<"),
      string(">"),
      string("*"),
      string("$"),
      string("^"),
      string("%"),
      string("#"),
    ])
  end

  # Converts the result of a parser to an atom
  # Tries to use an existing atom if one exists
  defp string_to_atom(parser) do
    map(parser, fn atom ->
      try do
        String.to_existing_atom(atom)
      rescue
        _e in ArgumentError ->
          String.to_atom(atom)
      end
    end)
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
      #to_built_in(char("+")),
      #to_built_in(char("-")),
      #to_built_in(char("*")),
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

  defp module_path() do
    hyphenated_word()
    |> sep_by1(char("/"))
    |> map(&Enum.join(&1, "/"))
  end

  defp hyphenated_word() do
    word()
    |> sep_by1(char("-"))
    |> map(&Enum.join(&1, "-"))
  end

  # lazy parser implementation from
  # https://github.com/bitwalker/combine/issues/12#issuecomment-222539479
  defparser lazy(%Combine.ParserState{status: :ok} = state, generator) do
    (generator.()).(state)
  end
end

defmodule Terp.ParserTest do
  use ExUnit.Case
  doctest Terp.Parser
  alias Terp.Parser

  test "defn desugars into a let/lambda" do
    defn = Parser.parse("(defn id (x) x)")
    let_lambda = Parser.parse("(let id (lambda (x) x))")

    assert defn == let_lambda
  end
end

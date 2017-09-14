defmodule Terp.ParserTest do
  use ExUnit.Case
  doctest Terp.Parser
  alias Terp.Parser

  test "defn desugars into a let/lambda" do
    defn = Parser.parse("(defn id (x) x)")
    let_lambda = Parser.parse("(let id (lambda (x) x))")

    assert defn == let_lambda
  end

  test "defrec desugars into a letrec/lambda" do
    defrec = "(defrec factorial (n) (if (equal? n 0) 1 (* n (factorial (- n 1)))))"
    |> Parser.parse
    letrec_lambda = "(letrec factorial (lambda (n) (if (equal? n 0) 1 (* n (factorial (- n 1))))))"
    |> Parser.parse

    assert defrec == letrec_lambda
  end
end

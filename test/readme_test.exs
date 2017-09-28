defmodule Terp.ReadmeTest do
  use ExUnit.Case
  @moduledoc """
  The examples from the README
  """

  test "comment" do
    assert ";; A comment\n(+ 5 1)"
    |> Terp.eval() == 6
  end

  test "let" do
    assert "(let x 5)\n(let identity (lambda (x) x))\n(identity x)"
    |> Terp.eval() == 5
  end

  test "let values" do
    assert "(let-values ([x 5] [y 3]) (+ x y))"
    |> Terp.eval() == 8
  end

  test "let values in a function" do
    assert "(defn plusOne (x) (let-values ([y 1]) (+ x y)))\n(plusOne 5)"
    |> Terp.eval() == 6
  end

  test "conditionals" do
    assert "(if #t 5 10)"
    |> Terp.eval() == 5
  end

  test "conditionals with evaluated block" do
    assert "(let x 5)\n(if (equal? x 5) (* x x) 0)"
    |> Terp.eval() == 25
  end

  test "cond" do
    assert "(let sound (lambda (animal) (cond [(equal? animal \"cow\") \"moo\"] [(equal? animal \"cat\") \"meow\"] [(equal? animal \"dog\") \"bark\"] [#t \"zzz\"])))\n(sound \"dog\")"
    |> Terp.eval == "bark"
  end

  test "function definition" do
    assert "(let double (lambda (x) (* 2 x)))\n(double 5)"
    |> Terp.eval == 10
  end

  test "another function definition" do
    assert "(let square (lambda (x) (* x x)))\n(square 5)"
    |> Terp.eval == 25
  end

  test "nested functions" do
    assert "(((lambda (x) (lambda '(y) (+ x y))) 5 ) 3)"
    |> Terp.eval == 8
  end

  test "multi argument functions" do
    assert "((lambda (x y) (+ x y)) 5 3)"
    |> Terp.eval == 8
  end

  test "curried functions" do
    assert "(let add (lambda (x y) (+ x y)))\n(let add_five (add 5))\n(add_five 3)"
    |> Terp.eval == 8
  end

  test "defn syntactic sugar" do
    assert "(defn add (x y) (+ x y))\n(add 1 2)"
    |> Terp.eval == 3
  end

  test "recursive functions" do
    assert "(letrec factorial (lambda (n) (if (equal? n 0) 1 (* n (factorial (- n 1))))))\n(factorial 5)"
    |> Terp.eval == 120
  end

  test "Elixir interop" do
    assert "(:String.upcase \"asdf\")"
    |> Terp.eval == "ASDF"
  end

  test "Erlang interop" do
    assert "(:string.uppercase \"asdf\")"
    |> Terp.eval == "ASDF"
  end
end

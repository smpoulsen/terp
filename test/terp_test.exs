defmodule TerpTest do
  use ExUnit.Case
  use Support.TerpTest
  doctest Terp, import: true

  test "1 + 1 = 2" do
    assert "(+ 1 1)"
    |> eval_terp() == 2
  end

  test "identity function" do
    assert "((lambda '(:x) :x) 5)"
    |> eval_terp() == 5
  end

  test "nested lambda application" do
    res = "(((lambda '(:x) (lambda '(:y) (+ :x :y))) 5 ) 3)"
    |> eval_terp()
    assert res == 8

    res2 = "(((lambda '(:x) :x) (lambda '(:y) :y)) 5)"
    |> eval_terp()
    assert res2 == 5
  end

  test "conditionals work as expected" do
    assert "(if #t 5 9)"
    |> eval_terp() == 5

    assert "(if #f 5 9)"
    |> eval_terp() == 9
  end

  test "arithmetic inside conditionals works as expected" do
    assert "(if #t (- 10 3) 9)"
    |> eval_terp() == 7
  end

  test "multiple arguments in function definition" do
    expr = "((lambda '(:x :y) (+ :x :y)) 5 3)"
    assert eval_terp(expr) == 8
  end

  test "variable binding with let" do
    expr = """
    (let :identity (lambda '(:x) :x))
    (:identity 5)
    """
    assert eval_terp(expr) == 5
  end

  test "comments are ignored" do
    expr = """
    ;; a comment
    (let :identity (lambda '(:x) :x))
    (:identity 5)
    """
    assert eval_terp(expr) == 5
  end
end

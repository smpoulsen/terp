defmodule TerpTest do
  use ExUnit.Case
  doctest Terp

  test "1 + 1 = 2" do
    assert "(+ 1 1)"
    |> Terp.eval() == 2
  end

  test "identity function" do
    assert "((lambda '(:x) :x) 5)"
    |> Terp.eval() == 5
  end

  test "nested lambda application" do
    res = "(((lambda '(:x) (lambda '(:y) (+ :x :y))) 5 ) 3)"
    |> Terp.eval()
    assert res == 8

    res2 = "(((lambda '(:x) :x) (lambda '(:y) :y)) 5)"
    |> Terp.eval()
    assert res2 == 5
  end

  test "conditionals work as expected" do
    assert "(if #t 5 9)"
    |> Terp.eval() == 5

    assert "(if #f 5 9)"
    |> Terp.eval() == 9
  end

  test "arithmetic inside conditionals works as expected" do
    assert "(if #t (- 10 3) 9)"
    |> Terp.eval() == 7
  end
end

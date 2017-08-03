defmodule Terp.ReadmeTest do
  use ExUnit.Case
  @moduledoc """
  The examples from the README
  """

  test "(+ 5 3)" do
    assert "(+ 5 3)"
    |> Terp.eval() == 8
  end

  test "(* 2 4 5)" do
    assert "(* 2 4 5)"
    |> Terp.eval() == 40
  end

  test "(* 2 4 (+ 4 1))" do
    assert "(* 2 4 (+ 4 1))"
    |> Terp.eval() == 40
  end

  test "(if #t (* 5 5) (+ 4 1))" do
    assert "(if #t (* 5 5) (+ 4 1))"
    |> Terp.eval() == 25
  end

  test "(if #f (* 5 5) (+ 4 1))" do
    assert "(if #f (* 5 5) (+ 4 1))"
    |> Terp.eval() == 5
  end

  test "((lambda (:x) :x) 5)" do
    assert "((lambda (:x) :x) 5)"
    |> Terp.eval == 5
  end

  test "(((lambda (:x) (lambda '(:y) (+ :x :y))) 5 ) 3)" do
    assert "(((lambda (:x) (lambda '(:y) (+ :x :y))) 5 ) 3)"
    |> Terp.eval == 8
  end

  test "((lambda (:x :y) (+ :x :y)) 5 3)" do
    assert "((lambda (:x :y) (+ :x :y)) 5 3)"
    |> Terp.eval == 8
  end
end

defmodule Terp.Evaluate.BooleanTest do
  use ExUnit.Case
  use Support.TerpTest
  doctest Terp.Evaluate.Boolean


  test "(cond [(equal? 1 5) (9)] [#t 5])" do
    res = "(cond [(equal? 1 5) (9)] [#t 5])"
    |> eval_terp()
    assert res == 5
  end

  test "(cond [(equal? 1 5) (9)] [#f 5])" do
    res = "(cond [(equal? 1 5) (9)] [#f 5])"
    |> eval_terp()
    assert res == {:error, {:cond, "no true condition"}}
  end

  test "(cond [(equal? (+ 2 3) 5) 9] [#f 5])" do
    res = "(cond [(equal? (+ 2 3) 5) 9] [#f 5])"
    |> eval_terp()
    assert res == 9
  end

  test "(equal? 5 3)" do
    res = "(equal? 5 3)" |> eval_terp()
    assert res == false
  end

  test "(equal? 5 5)" do
    res = "(equal? 5 5)" |> eval_terp()
    assert res == true
  end
end

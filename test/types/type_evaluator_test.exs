defmodule Terp.Types.Type.TypeEvaluatorTest do
  use ExUnit.Case
  alias Terp.Types.Types

  test "type-checking a lambda" do
    type = "(lambda (x) x)"
    |> Types.type_check()
    |> List.first()
    assert type.str == "a -> a"
  end

  test "type-checking a literal int" do
    type = "5"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Int"
  end

  test "type-checking a lambda application" do
    type = "((lambda (x) x) 5)"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Int"
  end

  test "type-checking a binary operation application" do
    type = "((lambda (x) (+ x 5)) 5)"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Int"
  end

  test "type-checking a binary operation inside a lambda" do
    type = "(lambda (x) (+ x 5))"
    |> Types.type_check()
    |> List.first()
    assert type.str == "a -> Int"
  end

  test "type-checking equality" do
    type = "(lambda (x) (equal? x 5))"
    |> Types.type_check()
    |> List.first()
    assert type.str == "a -> Bool"
  end

  test "type-checking equality pt. 2" do
    type = "(lambda (x y) (equal? x y))"
    |> Types.type_check()
    |> List.first()
    assert type.str == "b -> a -> Bool"
  end
end

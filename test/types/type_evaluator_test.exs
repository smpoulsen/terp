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

  test "type-checking a literal bool - true" do
    type = "#t"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Bool"
  end

  test "type-checking a literal bool - false" do
    type = "#f"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Bool"
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
    assert type.str == "Int -> Int"
  end

  test "type-checking equality" do
    type = "(lambda (x) (equal? x 5))"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Int -> Bool"
  end

  test "type-checking equality pt. 2" do
    type = "(lambda (x y) (equal? x y))"
    |> Types.type_check()
    |> List.first()
    assert type.str == "b -> b -> Bool"
  end

  test "type-checking an if statement" do
    type = "(if #t 8 1)"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Int"
  end

  test "type-checking an if statement wrapped in a lambda" do
    type = "(lambda (x) (if #t x 1))"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Int -> Int"
  end

  test "type-checking an if statement with an equals? test" do
    type = "(lambda (x) (if (equal? x #t) 8 1))"
    |> Types.type_check()
    |> List.first()
    assert type.str == "Bool -> Int"
  end
end

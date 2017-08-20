defmodule Terp.Types.Type.TypeEvaluatorTest do
  use ExUnit.Case
  alias Terp.Types.Types

  describe "basic function inference" do
    test "type-checking a lambda" do
      type = "(lambda (x) x)"
      |> Types.type_check()
      |> List.first()
      assert type.str == "a -> a"
    end

    test "type-checking a lambda application" do
      type = "((lambda (x) x) 5)"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int"
    end
  end

  describe "literal inference" do
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

    test "type-checking a literal string" do
      type = "\"testing\""
      |> Types.type_check()
      |> List.first()
      assert type.str == "String"
    end
  end

  describe "binary operation inference" do
    test "type-checking a binary operation application" do
      type = "((lambda (x) (+ x 5)) 5)"
      |> Types.type_check() |> List.first()
      assert type.str == "Int"
    end

    test "type-checking a binary operation inside a lambda" do
      type = "(lambda (x) (+ x 5))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int -> Int"
    end
  end

  describe "equality inference" do
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
      assert type.str == "a -> a -> Bool"
    end
  end

  describe "if statement inference" do
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

  describe "list inference" do
    test "type-checking a list of integers" do
      type = "'(3 2 5 9)"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[Int]"
    end

    test "type-checking a function that builds a list of integers" do
      type = "(lambda (x) '(3 2 5 x))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int -> [Int]"
    end

    test "type-checking a list of lists of integers" do
      type = "'('(1 2 3) '(4 5 6))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[[Int]]"
    end

    test "type-checking car" do
      type = "(car '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int"
    end

    test "type-checking cdr" do
      type = "(cdr '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[Int]"
    end

    test "type-checking cons" do
      type = "(cons 4 '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[Int]"
    end

    test "type-checking cons in a function" do
      type = "(lambda (x) (cons x '(3 2 5 9)))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int -> [Int]"
    end

    test "type-checking cons consing a string to [Int]" do
      type = "(cons \"asdf\" '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type == {:error, {:type, "Unable to unify Int with String"}}
    end

    test "type-checking empty? for a list of integers" do
      type = "(empty? '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Bool"
    end

    test "type-checking empty? inside a function application" do
      type = "(lambda (x) (empty? x))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[c] -> Bool"
    end
  end

  describe "recursive functions" do
    test "type-checking factorial" do
      type = """
      (letrec factorial
        (lambda (n)
          (if (equal? n 0)
            1
            (* n (factorial (- n 1))))))
      """
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int -> Int"
    end
  end
end

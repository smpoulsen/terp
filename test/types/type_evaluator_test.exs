defmodule Terp.Types.Type.TypeEvaluatorTest do
  use ExUnit.Case
  alias Terp.Types.Types

  describe "basic function inference" do
    test "infer a lambda" do
      {:ok, {_vars, type}} = "(lambda (x) x)"
      |> Types.type_check()
      |> List.first()
      assert type.str == "(a -> a)"
    end

    test "infer a lambda application" do
      {:ok, {_vars, type}} = "((lambda (x) x) 5)"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int"
    end
  end

  describe "literal inference" do
    test "infer a literal int" do
      {:ok, {_vars, type}} = "5"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int"
    end

    test "infer a literal bool - true" do
      {:ok, {_vars, type}} = "#t"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Bool"
    end

    test "infer a literal bool - false" do
      {:ok, {_vars, type}} = "#f"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Bool"
    end

    test "infer a literal string" do
      {:ok, {_vars, type}} = "\"testing\""
      |> Types.type_check()
      |> List.first()
      assert type.str == "String"
    end
  end

  describe "binary operation inference" do
    test "infer a binary operation application" do
      {:ok, {_vars, type}} = "((lambda (x) (+ x 5)) 5)"
      |> Types.type_check() |> List.first()
      assert type.str == "Int"
    end

    test "infer a binary operation inside a lambda" do
      {:ok, {_vars, type}} = "(lambda (x) (+ x 5))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "(Int -> Int)"
    end
  end

  describe "equality inference" do
    test "infer equality" do
      {:ok, {_vars, type}} = "(lambda (x) (equal? x 5))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "(Int -> Bool)"
    end

    test "infer equality pt. 2" do
      {:ok, {_vars, type}} = "(lambda (x y) (equal? x y))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "((a -> a) -> Bool)"
    end
  end

  describe "if statement inference" do
    test "infer an if statement" do
      {:ok, {_vars, type}} = "(if #t 8 1)"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int"
    end

    test "infer an if statement wrapped in a lambda" do
      {:ok, {_vars, type}} = "(lambda (x) (if #t x 1))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "(Int -> Int)"
    end

    test "infer an if statement with an equals? test" do
      {:ok, {_vars, type}} = "(lambda (x) (if (equal? x #t) 8 1))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "(Bool -> Int)"
    end
  end

  describe "list inference" do
    test "infer a list of integers" do
      {:ok, {_vars, type}} = "'(3 2 5 9)"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[Int]"
    end

    test "infer a function that builds a list of integers" do
      {:ok, {_vars, type}} = "(lambda (x) '(3 2 5 x))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "(Int -> [Int])"
    end

    test "infer a list of lists of integers" do
      {:ok, {_vars, type}} = "'('(1 2 3) '(4 5 6))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[[Int]]"
    end

    test "infer car" do
      {:ok, {_vars, type}} = "(car '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Int"
    end

    test "infer cdr" do
      {:ok, {_vars, type}} = "(cdr '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[Int]"
    end

    test "infer cons" do
      {:ok, {_vars, type}} = "(cons 4 '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "[Int]"
    end

    test "infer cons in a function" do
      {:ok, {_vars, type}} = "(lambda (x) (cons x '(3 2 5 9)))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "(Int -> [Int])"
    end

    test "infer cons consing a string to [Int]" do
      {:error, e} = "(cons \"asdf\" '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert e == {
        :type, {
          :unification,
          %{expected: %Terp.Types.Types{constructor: :Tconst, str: "Int", t: :INTEGER},
            received: %Terp.Types.Types{constructor: :Tconst, str: "String", t: :STRING}}
        }
      }
    end

    test "infer empty? for a list of integers" do
      {:ok, {_vars, type}} = "(empty? '(3 2 5 9))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "Bool"
    end

    test "infer empty? inside a function application" do
      {:ok, {_vars, type}} = "(lambda (x) (empty? x))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "([a] -> Bool)"
    end
  end

  describe "recursive functions" do
    test "infer factorial" do
      {:ok, {_vars, type}} = """
      (letrec factorial
        (lambda (n)
          (if (equal? n 0)
            1
            (* n (factorial (- n 1))))))
      """
      |> Types.type_check()
      |> List.first()
      assert type.str == "(Int -> Int)"
    end
  end

  describe "higher order functions" do
    test "infer a general higher order function" do
      {:ok, {_vars, type}} = "(lambda (f x) (f x))"
      |> Types.type_check()
      |> List.first()
      assert type.str == "(((a -> b) -> a) -> b)"
    end

    test "infer a specific higher order function" do
      {:ok, {_vars, type}} = """
      (lambda (f x)
        (if (equal? x 0)
          x
          (f x)))
      """
      |> Types.type_check()
      |> List.first()
      assert type.str == "(((Int -> Int) -> Int) -> Int)"
    end
  end
end

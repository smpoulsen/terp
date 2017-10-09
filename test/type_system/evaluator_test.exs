defmodule Terp.TypeSystem.EvaluatorTest do
  use ExUnit.Case
  use Support.TerpTest
  alias Terp.TypeSystem
  alias Terp.TypeSystem.Type

  setup do
    TypeSystem.start_environment()
    "(require prelude/typeclass/classes)"
    |> TypeSystem.check_src()
    {:ok, %{}}
  end

  describe "basic function inference" do
    test "infer a lambda" do
      {:ok, types} = "(lambda (x) x)"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "a -> a"
    end

    test "infer a lambda application" do
      {:ok, types} = "((lambda (x) x) 5)"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int"
    end
  end

  describe "literal inference" do
    test "infer a literal int" do
      {:ok, types} = "5"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int"
    end

    test "infer a literal bool - true" do
      {:ok, types} = "#t"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Bool"
    end

    test "infer a literal bool - false" do
      {:ok, types} = "#f"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Bool"
    end

    test "infer a literal string" do
      {:ok, types} = "\"testing\""
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "String"
    end
  end

  describe "binary operation inference" do
    test "infer a binary operation application" do
      {:ok, types} = "((lambda (x) (+ x 5)) 5)"
      |> type_check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int"
    end

    test "infer a binary operation inside a lambda" do
      {:ok, types} = "(lambda (x) (+ x 5))"
      |> type_check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int -> Int"
    end
  end

  describe "equality inference" do
    test "infer equality" do
      {:ok, types} = "(lambda (x) (equal? x 5))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int -> Bool"
    end

    test "infer equality pt. 2" do
      {:ok, types} = "(lambda (x y) (equal? x y))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "a -> a -> Bool"
    end
  end

  describe "if statement inference" do
    test "infer an if statement" do
      {:ok, types} = "(if #t 8 1)"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int"
    end

    test "infer an if statement wrapped in a lambda" do
      {:ok, types} = "(lambda (x) (if #t x 1))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int -> Int"
    end

    test "infer an if statement with an equals? test" do
      {:ok, types} = "(lambda (x) (if (equal? x #t) 8 1))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Bool -> Int"
    end
  end

  describe "cond inference" do
    test "infer a cond from string -> string" do
      {:ok, types} = """
      (let sound
        (lambda (animal)
          (cond
            [(equal? animal "cow") "moo"]
            [(equal? animal "cat") "meow"]
            [(equal? animal "dog") "bark"]
            [#t "zzz"]
            )))
      """
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "String -> String"
    end
  end

  describe "list inference" do
    test "infer a list of integers" do
      {:ok, types} = "'(3 2 5 9)"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "[Int]"
    end

    test "infer a function that builds a list of integers" do
      {:ok, types} = "(lambda (x) '(3 2 5 x))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int -> [Int]"
    end

    test "infer a list of lists of integers" do
      {:ok, types} = "'('(1 2 3) '(4 5 6))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "[[Int]]"
    end

    test "infer car" do
      {:ok, types} = "(car '(3 2 5 9))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int"
    end

    test "infer cdr" do
      {:ok, types} = "(cdr '(3 2 5 9))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "[Int]"
    end

    test "infer cons" do
      {:ok, types} = "(cons 4 '(3 2 5 9))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "[Int]"
    end

    test "infer cons in a function" do
      {:ok, types} = "(lambda (x) (cons x '(3 2 5 9)))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int -> [Int]"
    end

    test "infer cons consing a string to [Int]" do
      error = "(cons \"asdf\" '(3 2 5 9))"
      |> TypeSystem.check_src()

      assert error.kind == :type
      assert error.type == :unification
      assert error.evaluating == %{expected: %Type{constructor: :Tconst, t: :Int},
                                   actual: %Type{constructor: :Tconst, t: :String}}
    end

    test "infer empty? for a list of integers" do
      {:ok, types} = "(empty? '(3 2 5 9))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Bool"
    end

    test "infer empty? inside a function application" do
      {:ok, types} = "(lambda (x) (empty? x))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "[a] -> Bool"
    end
  end

  describe "recursive functions" do
    test "infer factorial" do
      {:ok, types} = """
      (letrec factorial
        (lambda (n)
          (if (equal? n 0)
            1
            (* n (factorial (- n 1))))))
      """
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "Int -> Int"
    end
  end

  describe "higher order functions" do
    test "infer a general higher order function" do
      {:ok, types} = "(lambda (f x) (f x))"
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "(a -> b) -> a -> b"
    end

    test "infer a specific higher order function" do
      {:ok, types} = """
      (lambda (f x)
        (if (equal? x 0)
          x
          (f x)))
      """
      |> TypeSystem.check_src()
      {_vars, type} = List.first(types)
      assert to_string(type) == "(Int -> Int) -> Int -> Int"
    end
  end
end

defmodule Terp.Types.Type.TypeEvaluatorTest do
  use ExUnit.Case
  alias Terp.Types.Types
  alias Terp.Types.TypeEvaluator

  test "type-checking a lambda" do
    type = "(lambda (x) x)"
    |> Types.type_check()
    |> List.first()
    assert type.str == "a -> a"
  end

  test "type-checking a literal int" do
    type = "(lambda (x) x)"
    |> Types.type_check()
    |> List.first()
    assert type.str == "a -> a"
  end
end

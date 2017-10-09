defmodule Terp.TypeSystem.MatchTest do
  use ExUnit.Case
  alias Terp.TypeSystem

  setup do
    TypeSystem.start_environment()
    {:ok, %{}}
  end

  describe "Type checking Maybes" do
    setup do
      TypeSystem.start_environment()
      maybe = """
      (require prelude/typeclass/classes)
      (require examples/higher_kinded_types)
      """
      Terp.eval(maybe)
      {:ok, %{maybe: maybe}}
    end

    test "Matching a (Just Int) value" do
      {:ok, types} = """
      (maybePlusFive (Just 4))
      """
      |> TypeSystem.check_src()
      {_vars, type} = List.last(types)
      assert to_string(type) == "[Maybe Int]"
    end

    test "Matching a (Nothing) value" do
      {:ok, types} = """
      (maybePlusFive (Nothing))
      """
      |> TypeSystem.check_src()
      {_vars, type} = List.last(types)
      assert to_string(type) == "[Maybe Int]"
    end
  end
end

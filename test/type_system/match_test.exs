defmodule Terp.TypeSystem.MatchTest do
  use ExUnit.Case
  alias Terp.TypeSystem
  alias Terp.TypeSystem.TypeEnvironment

  setup do
    TypeEnvironment.start_link()
    {:ok, %{}}
  end

  describe "Type checking Maybes" do
    setup do
      maybe = """
      (data (Maybe a) [Just a] [Nothing])
      (defn maybePlusFive (x) (match (x) [(Just z) (Just (+ z 5))] [(Nothing) (Nothing)]))
      """
      {:ok, %{maybe: maybe}}
    end
    test "Matching a (Just Int) value", %{maybe: maybe} do
      {:ok, types} = maybe <> """
      (maybePlusFive (Just 4))
      """
      |> TypeSystem.check_src()
      {_vars, type} = List.last(types)
      assert to_string(type) == "[Maybe Int]"
    end

    test "Matching a (Nothing) value", %{maybe: maybe} do
      {:ok, types} = maybe <> """
      (maybePlusFive (Nothing))
      """
      |> TypeSystem.check_src()
      {_vars, type} = List.last(types)
      assert to_string(type) == "[Maybe Int]"
    end
  end
end

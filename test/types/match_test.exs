defmodule Terp.Types.MatchTest do
  use ExUnit.Case
  alias Terp.Types.Types
  alias Terp.Types.TypeEnvironment

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
      {:ok, {_vars, type}} = maybe <> """
      (maybePlusFive (Just 4))
      """
      |> Types.type_check()
      |> List.last()
      assert to_string(type) == "[Maybe Int]"
    end

    test "Matching a (Nothing) value", %{maybe: maybe} do
      {:ok, {_vars, type}} = maybe <> """
      (maybePlusFive (Nothing))
      """
      |> Types.type_check()
      |> List.last()
      assert to_string(type) == "[Maybe Int]"
    end
  end
end

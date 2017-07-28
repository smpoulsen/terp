defmodule Terp.Arithmetic do
  @moduledoc """
  Arithmetic operations.
  """

  @doc """
  Subtraction proceeds pairwise.
  Given [2, 3, 5, 7], evaluates as:
  (((2 - 3) - 5) - 7)
  -13

  ## Examples

      iex> Terp.Arithmetic.subtract([2, 3, 5, 7])
      -13
  """
  def subtract([]), do: 0
  def subtract([h | []]), do: h
  def subtract([l | [r | t]]), do: subtract([(l - r) | t])
end

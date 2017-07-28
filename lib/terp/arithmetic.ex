defmodule Terp.Arithmetic do
  @moduledoc """
  Arithmetic operations.
  """

  @doc """
  Addition; sums a list of numbers.

  ## Examples

      iex> Terp.Arithmetic.add([1, 2, 3, 4])
      10
  """
  def add(ns) do
    Enum.sum(ns)
  end

  @doc """
  Multiple; multiplies a list of numbers.

  ## Examples

      iex> Terp.Arithmetic.multiply([1, 2, 3, 4])
      24
  """
  def multiply(ns) do
    Enum.reduce(ns, 1, fn (x, acc) -> x * acc end)
  end

  @doc """
  Subtraction proceeds pairwise.
  Given [2, 3, 5, 7], evaluates as:
  (((2 - 3) - 5) - 7)
  -13

  ## Examples

      iex> Terp.Arithmetic.subtract([2, 3, 5, 7])
      -13
  """
  def subtract(ns) do
    case ns do
      [x | []] -> -x
      _ -> rec_subtract(ns)
    end
  end
  defp rec_subtract([]), do: 0
  defp rec_subtract([h | []]), do: h
  defp rec_subtract([l | [r | t]]), do: rec_subtract([(l - r) | t])

  @doc """
  Division proceeds pairwise.
  Given [2, 3, 5, 4], evaluates as:
  (((2 / 3) / 5) / 4)
  1/30 == 0.03333333

  ## Examples

      iex> Terp.Arithmetic.divide([2, 3, 5, 4])
      0.03333333333333333
  """
  def divide(ns) do
    case ns do
      [x | []] -> 1 / x
      _ -> rec_divide(ns)
    end
  end
  defp rec_divide([]), do: 1
  defp rec_divide([h | []]), do: h
  defp rec_divide([l | [r | t]]), do: rec_divide([(l / r) | t])
end

defmodule Terp.Boolean do
  @moduledoc """
  Boolean values and conditional evaluation.
  """

  @doc """
  true

  ## Examples

      iex> Terp.Boolean.t
      true
  """
  def t(), do: true

  @doc """
  false

  ## Examples

      iex> Terp.Boolean.f
      false
  """
  def f(), do: false

  @doc """
  If - then - else conditional logic.

  ## Examples

      iex> [true, 5, 3]
      ...> |> Terp.Boolean.conditional()
      5

      iex> [false, 5, 3]
      ...> |> Terp.Boolean.conditional()
      3
  """
  def conditional([test | [consequent | [alternative | []]]]) do
    if test, do: consequent, else: alternative
  end
end

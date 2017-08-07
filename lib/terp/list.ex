defmodule Terp.List do
  @moduledoc """
  Provides functionality for working with lists.
  """

  @doc """
  Take the first element from a list.

  ## Examples

      iex> "(car '(1 2 3))"
      ...> |> Terp.eval()
      1

      iex> "(car '())"
      ...> |> Terp.eval()
      {:error, {:terp, :empty_list}}

      iex> "(car 5)"
      ...> |> Terp.eval()
      {:error, {:terp, {:not_a_list, 5}}}
  """
  def car(operands, environment) do
    operands
    |> List.first()
    |> Terp.eval_expr(environment)
    |> car_helper()
  end
  defp car_helper([]), do: {:error, {:terp, :empty_list}}
  defp car_helper([h | _t]), do: h
  defp car_helper(e), do: {:error, {:terp, {:not_a_list, e}}}

  @doc """
  Take the tail of a list.

  ## Examples

  iex> "(cdr '(1 2 3))"
  ...> |> Terp.eval()
  [2, 3]

  iex> "(cdr '())"
  ...> |> Terp.eval()
  {:error, {:terp, :empty_list}}

  iex> "(cdr 5)"
  ...> |> Terp.eval()
  {:error, {:terp, {:not_a_list, 5}}}
  """
  def cdr(operands, environment) do
    operands
    |> List.first()
    |> Terp.eval_expr(environment)
    |> cdr_helper()
  end
  defp cdr_helper([]), do: {:error, {:terp, :empty_list}}
  defp cdr_helper([_h | t]), do: t
  defp cdr_helper(e), do: {:error, {:terp, {:not_a_list, e}}}
end

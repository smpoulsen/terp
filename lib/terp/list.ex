defmodule Terp.List do
  @moduledoc """
  Provides functionality for working with lists.
  """

  @doc """
  Build a list by prepending an item to it.
  """
  def cons([], environment), do: Terp.eval_expr(nil, environment)
  def cons([x | xs], environment) do
    e = Terp.eval_expr(x, environment)
    acc = case xs do
      []  -> []
      [t] -> Terp.eval_expr(t, environment)
    end
    [e | acc]
  end

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

  @doc """
  Predicate to check if a list is empty.

  ## Examples
      iex> "(empty? '(1 2 3))"
      ...> |> Terp.eval()
      false

      iex> "(empty? '())"
      ...> |> Terp.eval()
      true
  """
  def empty?(operands, environment) do
    operands
    |> Enum.map(&Terp.eval_expr(&1, environment))
    |> List.first()
    |> Enum.empty?()
  end
end
